//
// Part of penne
// Copyright (c) 2020 Sander in 't Veld
// License: MIT
//

//! The compiler for the Penne programming language.
//!
//! This is the internal documentation for the Penne compiler.
//! General information about the Penne programming language,
//! its syntax and semantics
//! is found in the [README](https://github.com/SLiV9/penne#readme).
//! Usage instructions for the command line interface can be found
//! by running `penne help` from the command line.
//!
//! The Abstract Syntax Tree is detailed in the modules [common], [value_type]
//! and [resolved]. Errors are laid out in [error]. The compiler stages are
//! (in order):
//! [lexer], [parser], [expander], [scoper], [typer], [analyzer],
//! [linter], [resolver] and [generator].
//! The [rebuilder] module allows turning the AST back into (annotated) code.
//! The [stdout] module contains helper code for the command line interface.
//! The [included] module contains Penne source code for core and vendor
//! libraries.

pub mod analyzer;
pub mod common;
pub mod error;
pub mod expander;
pub mod generator;
pub mod included;
pub mod lexer;
pub mod linter;
pub mod parser;
pub mod rebuilder;
pub mod resolved;
pub mod resolver;
pub mod scoper;
pub mod stdout;
pub mod typer;
pub mod value_type;

pub use error::Error;
pub use error::Errors;
pub use resolved::Declaration;

/// Convenience method that parses source code and runs it through each of the
/// compiler stages.
pub fn compile_source(
	source: &str,
	filename: &str,
) -> Result<Vec<Declaration>, Errors>
{
	let tokens = lexer::lex(source, filename);
	let declarations = parser::parse(tokens);
	let declarations = expander::expand_one(filename, declarations);
	resolver::check_surface_level_errors(&declarations)?;
	let declarations = scoper::analyze(declarations);
	let mut compiler = Compiler::default();
	compiler.add_module(filename).unwrap();
	compiler
		.analyze_and_resolve(declarations)
		.expect("failed to generate IR")
}

#[derive(Default)]
pub struct Compiler
{
	typer: typer::Typer,
	analyzer: analyzer::Analyzer,
	linter: linter::Linter,
	generator: generator::Generator,
}

impl Compiler
{
	pub fn for_wasm(&mut self) -> Result<(), anyhow::Error>
	{
		self.generator.for_wasm()
	}

	pub fn add_module(&mut self, module_name: &str)
		-> Result<(), anyhow::Error>
	{
		self.typer = typer::Typer::default();
		self.analyzer = analyzer::Analyzer::default();
		self.linter = linter::Linter::default();
		self.generator.add_module(module_name)
	}

	pub fn analyze_and_resolve(
		&mut self,
		mut declarations: Vec<common::Declaration>,
	) -> Result<Result<Vec<resolved::Declaration>, error::Errors>, anyhow::Error>
	{
		// Sort the declarations so that the functions are at the end and
		// the constants and structures are declared in the right order.
		declarations.sort_by_key(|x| scoper::get_container_depth(x, u32::MAX));

		let offset = declarations.partition_point(|x| scoper::is_container(x));
		// First analyze and resolve all the constants and structures,
		// then analyze and resolve all the functions.
		// This works because constants and structures cannot use functions.
		let functions = declarations.split_off(offset);
		let containers = declarations;
		let containers = self.analyze_and_resolve_sorted(containers, true)?;
		let functions = self.analyze_and_resolve_sorted(functions, false)?;
		let declarations = match (containers, functions)
		{
			(Ok(mut declarations), Ok(mut more)) =>
			{
				declarations.append(&mut more);
				Ok(declarations)
			}
			(Ok(_), Err(errors)) => Err(errors),
			(Err(errors), Ok(_)) => Err(errors),
			(Err(errors), Err(more)) =>
			{
				Err(errors.combined_with(more).sorted())
			}
		};
		Ok(declarations)
	}

	fn analyze_and_resolve_sorted(
		&mut self,
		declarations: Vec<common::Declaration>,
		are_all_containers: bool,
	) -> Result<Result<Vec<resolved::Declaration>, error::Errors>, anyhow::Error>
	{
		let declarations = if are_all_containers
		{
			declarations
		}
		else
		{
			// Align all poisoned structures at the same time, because they
			// might contain references to each other.
			let mut declarations = declarations;
			for declaration in &mut declarations
			{
				typer::align_structure(declaration, &mut self.typer);
			}
			// Declare all function signatures and analyze their types.
			let declarations: Vec<_> = declarations
				.into_iter()
				.map(|x| typer::declare(x, &mut self.typer))
				.collect();
			for declaration in &declarations
			{
				analyzer::declare(declaration, &mut self.analyzer);
			}
			declarations
		};

		// Forward declare all structures as opaque types so that pointer types
		// are valid (because pointers do not affect container depth).
		for name in declarations.iter().filter_map(scoper::get_structure_name)
		{
			generator::forward_declare_structure(name, &mut self.generator)?;
		}

		// Type, analyze, lint and resolve the program one declaration at
		// a time, and generate IR for structures and constants in advance.
		// This works because the declarations are sorted by container depth.
		// Also generate IR for function signatures in advance.
		let r = declarations.into_iter().try_fold(Ok(Vec::new()), |acc, x| {
			let declaration = x;
			let declaration = if are_all_containers
			{
				let mut declaration = declaration;
				typer::align_structure(&mut declaration, &mut self.typer);
				typer::declare(declaration, &mut self.typer)
			}
			else
			{
				declaration
			};
			let declaration = typer::analyze(declaration, &mut self.typer);
			let declaration =
				analyzer::analyze(declaration, &mut self.analyzer);
			linter::lint(&declaration, &mut self.linter);
			let resolved = resolver::resolve(declaration);
			let acc = match (acc, resolved)
			{
				(Ok(mut declarations), Ok(declaration)) =>
				{
					generator::declare(&declaration, &mut self.generator)?;
					self.fetch_declared_constants(&declaration);
					declarations.push(declaration);
					Ok(declarations)
				}
				(Ok(_), Err(errors)) => Err(errors),
				(Err(errors), Ok(_)) => Err(errors),
				(Err(errors), Err(more)) => Err(errors.combined_with(more)),
			};
			Ok(acc)
		});
		match r
		{
			Ok(Ok(declarations)) => Ok(Ok(declarations)),
			Ok(Err(errors)) => Ok(Err(errors.sorted())),
			Err(error) => Err(error),
		}
	}

	fn fetch_declared_constants(&mut self, declaration: &resolved::Declaration)
	{
		match declaration
		{
			Declaration::Constant {
				name,
				value_type: value_type::ValueType::Usize,
				..
			} =>
			{
				if let Some(value) = self.generator.get_named_length(name)
				{
					self.typer.resolve_named_length(name.resolution_id, value);
				}
			}
			_ => (),
		}
	}

	pub fn compile(
		&mut self,
		declarations: &[resolved::Declaration],
	) -> Result<(), anyhow::Error>
	{
		for declaration in declarations
		{
			generator::generate(declaration, &mut self.generator)?;
		}
		self.generator.verify();
		Ok(())
	}

	pub fn take_lints(&mut self) -> Vec<linter::Lint>
	{
		std::mem::take(&mut self.linter).into()
	}

	pub fn link_modules(&mut self) -> Result<(), anyhow::Error>
	{
		self.generator.link_modules()
	}

	pub fn generate_ir(&self) -> Result<String, anyhow::Error>
	{
		self.generator.generate_ir()
	}
}
