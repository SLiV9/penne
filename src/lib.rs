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
	let mut declarations = scoper::analyze(declarations);

	// Sort the declarations so that the functions are at the end and
	// the constants and structures are declared in the right order.
	let max: u32 = declarations.len().try_into().unwrap();
	declarations.sort_by_key(|x| scoper::get_container_depth(x, max));

	let mut typer = typer::Typer::default();
	let mut analyzer = analyzer::Analyzer::default();
	// First, align all structures.
	for declaration in &mut declarations
	{
		typer::align_structure(declaration, &mut typer);
	}
	// Then, declare all constants and functions and analyze their types.
	let declarations: Vec<_> = declarations
		.into_iter()
		.map(|x| typer::declare(x, &mut typer))
		.collect();
	// Note the `collect()`.
	// Then, declare all constants and functions.
	for declaration in &declarations
	{
		analyzer::declare(declaration, &mut analyzer);
	}

	let mut generator = generator::Generator::new(filename).unwrap();

	// Type, analyze, lint and resolve the program one declaration at
	// a time, and generate IR for structures and constants in advance.
	// This works because the declarations are sorted by container depth.
	declarations
		.into_iter()
		.fold(Ok(Vec::new()), |acc, declaration| {
			let declaration = typer::analyze(declaration, &mut typer);
			let declaration = analyzer::analyze(declaration, &mut analyzer);
			// No linting.
			let resolved = resolver::resolve(declaration);
			match (acc, resolved)
			{
				(Ok(mut declarations), Ok(declaration)) =>
				{
					generator::declare(&declaration, &mut generator).unwrap();
					declarations.push(declaration);
					Ok(declarations)
				}
				(Ok(_), Err(errors)) => Err(errors),
				(Err(errors), Ok(_)) => Err(errors),
				(Err(errors), Err(more)) => Err(errors.combined_with(more)),
			}
		})
}
