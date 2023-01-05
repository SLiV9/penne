//
// Part of penne
// Copyright (c) 2020 Sander in 't Veld
// License: MIT
//

use penne::analyzer;
use penne::common::Declaration;
use penne::common::DeclarationFlag;
use penne::generator;
use penne::lexer;
use penne::linter;
use penne::parser;
use penne::resolver;
use penne::scoper;
use penne::typer;

use std::collections::HashMap;
use std::io::Write;

use anyhow::anyhow;
use anyhow::Context;
use clap::Parser;
use enumset::EnumSet;
use serde::Deserialize;

#[cfg(feature = "logging")]
use env_logger;

#[derive(Debug, clap::Parser)]
#[clap(version, propagate_version = true)]
#[clap(args_conflicts_with_subcommands = true)]
struct Cli
{
	#[clap(subcommand)]
	sub: Option<Subcommand>,

	#[clap(flatten)]
	build: BuildArgs,
}

#[derive(Debug, Deserialize, clap::Subcommand)]
enum Subcommand
{
	/// Compile one or more Penne source files into an executable (default)
	Build(BuildArgs),
	/// Generate LLVM IR from a Penne source file and execute it using lli
	Run(RunArgs),
	/// Generate LLVM IR from Penne source files
	Emit(EmitArgs),
}

#[derive(Debug, Default, Deserialize, clap::Args)]
#[serde(default, deny_unknown_fields)]
struct BuildArgs
{
	/// One or more Penne source files
	#[clap(value_parser, required(true))]
	filepaths: Vec<std::path::PathBuf>,

	/// Which LLVM executable to pipe the generated IR into (default: 'clang')
	#[clap(short, long)]
	backend: Option<std::path::PathBuf>,

	/// Pass one or more arguments, separated by spaces, to the backend
	#[clap(long, value_delimiter(' '))]
	#[serde(deserialize_with = "deserialize_space_separated_string")]
	backend_args: Option<Vec<String>>,

	/// Load additional build options from TOML file
	#[clap(long)]
	config: Option<std::path::PathBuf>,

	/// Pass one or more arguments, separated by spaces, to the linker
	#[clap(long, value_delimiter(' '))]
	#[serde(deserialize_with = "deserialize_space_separated_string")]
	link_args: Option<Vec<String>>,

	/// Write binary output to this file
	#[clap(short)]
	output_filepath: Option<std::path::PathBuf>,

	/// Write binary output and generated IR to this directory
	#[clap(long)]
	out_dir: Option<std::path::PathBuf>,

	/// Show a lot of intermediate output
	#[clap(short, long)]
	verbose: bool,

	/// Set target to 'wasm32-unknown-wasi'
	#[clap(short, long)]
	wasm: bool,
}

#[derive(Debug, Default, Deserialize, clap::Args)]
#[serde(default, deny_unknown_fields)]
struct RunArgs
{
	/// One or more Penne source files
	#[clap(value_parser, required(true))]
	filepaths: Vec<std::path::PathBuf>,

	/// Which LLVM executable to pipe the generated IR into (default: 'lli')
	#[clap(short, long)]
	backend: Option<std::path::PathBuf>,

	/// Pass one or more arguments, separated by spaces, to the backend
	#[clap(long, value_delimiter(' '))]
	#[serde(deserialize_with = "deserialize_space_separated_string")]
	backend_args: Option<Vec<String>>,

	/// Write binary output and generated IR to this directory
	#[clap(long)]
	out_dir: Option<std::path::PathBuf>,

	/// Show a lot of intermediate output
	#[clap(short, long)]
	verbose: bool,
}

#[derive(Debug, Default, Deserialize, clap::Args)]
#[serde(default, deny_unknown_fields)]
struct EmitArgs
{
	/// One or more Penne source files
	#[clap(value_parser, required(true))]
	filepaths: Vec<std::path::PathBuf>,

	/// Write binary output and generated IR to this directory
	#[clap(long)]
	out_dir: Option<std::path::PathBuf>,

	/// Show a lot of intermediate output
	#[clap(short, long)]
	verbose: bool,

	/// Set target to 'wasm32-unknown-wasi'
	#[clap(short, long)]
	wasm: bool,
}

fn main() -> Result<(), anyhow::Error>
{
	let result = do_main();
	if result.is_err()
	{
		let mut stdout = penne::stdout::StdOut::new(false);
		stdout.prepare_for_errors()?;
	}
	result
}

struct MainArgs
{
	out_dir: Option<std::path::PathBuf>,
	verbose: bool,
	filepaths: Vec<std::path::PathBuf>,
	is_lli: bool,
	skip_backend: bool,
	backend: String,
	backend_args: Option<Vec<String>>,
	link_args: Option<Vec<String>>,
	output_filepath: Option<std::path::PathBuf>,
	wasm: bool,
}

impl TryFrom<Cli> for MainArgs
{
	type Error = anyhow::Error;

	fn try_from(cli: Cli) -> Result<Self, Self::Error>
	{
		match cli
		{
			Cli {
				sub: None,
				build: args,
			}
			| Cli {
				sub: Some(Subcommand::Build(args)),
				build: _,
			} =>
			{
				let config: BuildArgs = if let Some(filename) = args.config
				{
					let raw = std::fs::read_to_string(&filename)?;
					toml::from_str(&raw).with_context(|| {
						format!(
							"failed to parse '{}'",
							filename.to_string_lossy()
						)
					})?
				}
				else
				{
					Default::default()
				};
				let backend = get_backend(
					args.backend,
					"PENNE_BACKEND",
					config.backend,
					"clang",
				)?;
				let backend_args = args.backend_args.or(config.backend_args);
				let link_args = args.link_args.or(config.link_args);
				let wasm = args.wasm || config.wasm;
				Ok(MainArgs {
					out_dir: args.out_dir,
					verbose: args.verbose,
					filepaths: args.filepaths,
					is_lli: false,
					skip_backend: false,
					backend,
					backend_args,
					link_args,
					output_filepath: args.output_filepath,
					wasm,
				})
			}
			Cli {
				sub: Some(Subcommand::Run(args)),
				build: _,
			} =>
			{
				let backend =
					get_backend(args.backend, "PENNE_LLI", None, "lli")?;
				Ok(MainArgs {
					out_dir: args.out_dir,
					verbose: args.verbose,
					filepaths: args.filepaths,
					is_lli: true,
					skip_backend: false,
					backend,
					backend_args: args.backend_args,
					link_args: None,
					output_filepath: None,
					wasm: false,
				})
			}
			Cli {
				sub: Some(Subcommand::Emit(args)),
				build: _,
			} => Ok(MainArgs {
				out_dir: args.out_dir,
				verbose: args.verbose,
				filepaths: args.filepaths,
				is_lli: true,
				skip_backend: true,
				backend: String::new(),
				backend_args: None,
				link_args: None,
				output_filepath: None,
				wasm: args.wasm,
			}),
		}
	}
}

fn do_main() -> Result<(), anyhow::Error>
{
	let args = Cli::parse().try_into()?;
	let MainArgs {
		out_dir,
		verbose,
		filepaths,
		is_lli,
		skip_backend,
		backend,
		backend_args,
		link_args,
		output_filepath,
		wasm,
	} = args;

	let output_filepath = output_filepath.or_else(|| {
		filepaths
			.iter()
			.last()
			.and_then(|filepath| filepath.file_name())
			.map(|filename| {
				let mut path = out_dir.clone().unwrap_or_default();
				path.push(filename);
				if wasm
				{
					path.set_extension("wasm");
				}
				else
				{
					path.set_extension(get_exe_extension());
				}
				path
			})
	});

	let mut stdout = penne::stdout::StdOut::new(verbose);

	let mut sources = Vec::new();
	let mut modules = HashMap::new();
	let mut backend_source = BackendSource::None;

	for filepath in filepaths
	{
		let filename = filepath.to_string_lossy().to_string();
		stdout.newline()?;
		let program = std::fs::read_to_string(&filepath)?;
		stdout.header("Lexing", &filename)?;
		let tokens = lexer::lex(&program, &filename);
		stdout.dump_tokens(&tokens)?;
		sources.push((filename.clone(), program));

		stdout.header("Parsing", &filename)?;
		let declarations = parser::parse(tokens);
		stdout.dump_code(&filename, &declarations)?;

		stdout.header("Preprocessing", &filename)?;
		let declarations = preprocess(declarations, &filepath, &modules)?;
		stdout.dump_code(&filename, &declarations)?;

		stdout.header("Scoping", &filename)?;
		let declarations = scoper::analyze(declarations);
		stdout.dump_code(&filename, &declarations)?;

		stdout.header("Typing", &filename)?;
		let declarations = typer::analyze(declarations);
		stdout.dump_code(&filename, &declarations)?;

		stdout.header("Analyzing", &filename)?;
		let declarations = analyzer::analyze(declarations);
		stdout.dump_code(&filename, &declarations)?;

		let stored_declarations = declarations.clone();

		stdout.header("Linting", &filename)?;
		let lints = linter::lint(&declarations);
		// We show the lints after resolution, if there are no errors.
		let linting_was_successful = lints.is_empty();
		stdout.linting(linting_was_successful)?;

		stdout.header("Resolving", &filename)?;
		let declarations = match resolver::resolve(declarations)
		{
			Ok(declarations) => declarations,
			Err(errors) =>
			{
				stdout.prepare_for_errors()?;
				for error in errors.into_iter()
				{
					error.report().eprint(ariadne::sources(sources.clone()))?;
					stdout.newline()?;
				}
				Err(anyhow!("compilation failed",))?;
				Vec::new()
			}
		};
		if !lints.is_empty()
		{
			for lint in lints
			{
				stdout.newline()?;
				lint.report().eprint(ariadne::sources(sources.clone()))?;
			}
			stdout.newline()?;
		}
		stdout.dump_resolved(&filename, &declarations)?;

		stdout.header("Generating IR for", &filename)?;
		stdout.prepare_for_errors()?;
		let ir = generator::generate(&declarations, &filename, wasm)?;
		stdout.dump_text(&ir)?;

		if let Some(out_dir) = &out_dir
		{
			let outputpath = {
				let mut path = out_dir.clone();
				path.push(filepath.clone());
				path.set_extension("pn.ll");
				path
			};
			let dirname = outputpath.parent().context("invalid output dir")?;
			std::fs::create_dir_all(dirname)?;
			backend_source = BackendSource::File(outputpath.clone());
			stdout.header("Writing to", &outputpath.to_string_lossy())?;
			std::fs::write(outputpath, ir)?;
		}
		else
		{
			backend_source = BackendSource::Stdin(ir);
		}
		// Store the declarations for later use.
		modules.insert(filepath, stored_declarations);
	}

	if let Some(output_filepath) = output_filepath.filter(|_x| !skip_backend)
	{
		stdout.basic_header("Building")?;
		stdout.prepare_for_errors()?;

		let mut cmd = std::process::Command::new(&backend);
		if let Some(backend_args) = backend_args
		{
			for arg in backend_args
			{
				cmd.arg(arg);
			}
		}
		if let Some(link_args) = link_args
		{
			for arg in link_args
			{
				cmd.arg(format!("-Wl,{}", arg));
			}
		}
		match &backend_source
		{
			BackendSource::File(filepath) =>
			{
				cmd.arg(filepath);
			}
			BackendSource::Stdin(_) =>
			{
				if !is_lli
				{
					cmd.arg("-x");
					cmd.arg("ir");
				}
				cmd.arg("-");
				cmd.stdin(std::process::Stdio::piped());
			}
			BackendSource::None => (),
		}
		cmd.arg("-o");
		cmd.arg(output_filepath);
		let mut cmd = cmd.spawn()?;
		match backend_source
		{
			BackendSource::Stdin(full_ir) =>
			{
				cmd.stdin
					.as_mut()
					.context("failed to pipe")?
					.write_all(full_ir.as_bytes())?;
			}
			_ => (),
		}
		let status = cmd.wait()?;
		if is_lli
		{
			let exitcode = status.code().context("no exitcode")?;
			stdout.output(exitcode)?;
		}
		else
		{
			status
				.success()
				.then_some(Some(()))
				.context("compilation unsuccessful")?;
		}
	}

	stdout.done()?;
	Ok(())
}

fn export(declaration: &Declaration) -> Option<Declaration>
{
	match declaration
	{
		Declaration::Constant {
			name,
			value,
			value_type,
			flags,
			location_of_declaration,
			location_of_type,
		} => extract_public(flags).map(|flags| Declaration::Constant {
			name: name.clone(),
			value: value.clone(),
			value_type: value_type.clone(),
			flags,
			location_of_declaration: location_of_declaration.clone(),
			location_of_type: location_of_type.clone(),
		}),
		Declaration::Function {
			name,
			parameters,
			return_type,
			body: _,
			flags,
			location_of_declaration,
			location_of_return_type,
		} => extract_public(flags).map(|flags| Declaration::FunctionHead {
			name: name.clone(),
			parameters: parameters.clone(),
			return_type: return_type.clone(),
			flags,
			location_of_declaration: location_of_declaration.clone(),
			location_of_return_type: location_of_return_type.clone(),
		}),
		Declaration::FunctionHead {
			name,
			parameters,
			return_type,
			flags,
			location_of_declaration,
			location_of_return_type,
		} => extract_public(flags).map(|flags| Declaration::FunctionHead {
			name: name.clone(),
			parameters: parameters.clone(),
			return_type: return_type.clone(),
			flags,
			location_of_declaration: location_of_declaration.clone(),
			location_of_return_type: location_of_return_type.clone(),
		}),
		Declaration::Structure {
			name,
			members,
			structural_type,
			depth,
			flags,
			location_of_declaration,
		} => extract_public(flags).map(|flags| Declaration::Structure {
			name: name.clone(),
			members: members.clone(),
			structural_type: structural_type.clone(),
			depth: depth.clone(),
			flags,
			location_of_declaration: location_of_declaration.clone(),
		}),
		Declaration::Import { .. } => None,
		Declaration::Poison(_) => None,
	}
}

fn extract_public(
	flags: &EnumSet<DeclarationFlag>,
) -> Option<EnumSet<DeclarationFlag>>
{
	let mut flags = flags.clone();
	if flags.remove(DeclarationFlag::Public)
	{
		Some(flags)
	}
	else
	{
		None
	}
}

fn preprocess(
	mut declarations: Vec<Declaration>,
	relative_path: &std::path::Path,
	modules: &HashMap<std::path::PathBuf, Vec<Declaration>>,
) -> Result<Vec<Declaration>, anyhow::Error>
{
	for declaration in declarations.iter_mut()
	{
		match declaration
		{
			Declaration::Import {
				filename,
				includes_definitions,
				location,
			} =>
			{
				if let Some(module) = relative_path
					.ancestors()
					.skip(1)
					.filter_map(|path| {
						let combined = path.join(filename.clone());
						modules.get(&combined)
					})
					.next()
				{
					let imported_declarations = module.to_vec();
					// TODO
					let _ = imported_declarations;
				}
				else
				{
					return Err(anyhow!("failed to resolve {:?}", filename))
						.context(location.format())
						.context("failed to preprocess");
				}
			}
			_ => (),
		}
	}
	Ok(declarations)
}

fn get_backend(
	arg_backend: Option<std::path::PathBuf>,
	env_backend_var: &str,
	config_backend: Option<std::path::PathBuf>,
	default: &str,
) -> Result<String, anyhow::Error>
{
	arg_backend
		.map(|x| x.to_str().map(|x| x.to_string()).context("invalid backend"))
		.or_else(|| match std::env::var(env_backend_var)
		{
			Ok(value) => Some(Ok(value)),
			Err(std::env::VarError::NotPresent) => None,
			Err(e) => Some(Err(e.into())),
		})
		.or_else(|| {
			config_backend.map(|x| {
				x.to_str()
					.map(|x| x.to_string())
					.context("invalid backend in config")
			})
		})
		.unwrap_or_else(|| Ok(default.to_string()))
}

enum BackendSource
{
	File(std::path::PathBuf),
	Stdin(String),
	None,
}

fn get_exe_extension() -> &'static str
{
	if cfg!(windows)
	{
		"exe"
	}
	else
	{
		std::env::consts::ARCH
	}
}

fn deserialize_space_separated_string<'de, D>(
	deserializer: D,
) -> Result<Option<Vec<String>>, D::Error>
where
	D: serde::Deserializer<'de>,
{
	let buffer = Option::<String>::deserialize(deserializer)?;
	if let Some(buffer) = buffer
	{
		let parts = buffer.split(' ').map(|x| x.to_string()).collect();
		Ok(Some(parts))
	}
	else
	{
		Ok(None)
	}
}
