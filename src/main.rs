//
// Part of penne
// Copyright (c) 2020 Sander in 't Veld
// License: MIT
//

use penne::analyzer;
use penne::expander;
use penne::generator;
use penne::lexer;
use penne::linter;
use penne::parser;
use penne::resolver;
use penne::scoper;
use penne::typer;

use std::io::Write;

use anyhow::anyhow;
use anyhow::Context;
use clap::Parser;
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
			.get(0)
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
	let mut modules = Vec::new();

	for filepath in filepaths
	{
		let filename = filepath.to_string_lossy().to_string();
		stdout.newline()?;
		let source = std::fs::read_to_string(&filepath)?;
		stdout.header("Lexing", &filename)?;
		let tokens = lexer::lex(&source, &filename);
		stdout.dump_tokens(&tokens)?;

		stdout.header("Parsing", &filename)?;
		let declarations = parser::parse(tokens);
		stdout.dump_code(&filename, &declarations)?;

		sources.push((filename, source));
		modules.push((filepath, declarations));
	}

	stdout.basic_header("Expanding imports")?;
	expander::expand(&mut modules);
	for (filepath, declarations) in &modules
	{
		let filename = filepath.to_string_lossy().to_string();
		stdout.header("Expanding", &filename)?;
		stdout.dump_code(&filename, &declarations)?;
	}

	let mut source_cache = ariadne::sources(sources);
	let mut generated_ir_files = Vec::new();

	for (filepath, declarations) in modules
	{
		let filename = filepath.to_string_lossy().to_string();
		stdout.newline()?;
		stdout.header("Scoping", &filename)?;
		let declarations = scoper::analyze(declarations);
		stdout.dump_code(&filename, &declarations)?;

		stdout.header("Typing", &filename)?;
		let declarations = typer::analyze(declarations);
		stdout.dump_code(&filename, &declarations)?;

		stdout.header("Analyzing", &filename)?;
		let declarations = analyzer::analyze(declarations);
		stdout.dump_code(&filename, &declarations)?;

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
					error.report().eprint(&mut source_cache)?;
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
				lint.report().eprint(&mut source_cache)?;
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
			generated_ir_files.push(outputpath.clone());
			stdout.io_header("Writing to", &outputpath)?;
			std::fs::write(outputpath, ir)?;
		}
		else
		{
			// TODO stdin ir
		}
	}

	if let Some(output_filepath) = output_filepath.filter(|_x| !skip_backend)
	{
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
		for filepath in generated_ir_files
		{
			cmd.arg(filepath);
		}
		if false
		{
			if !is_lli
			{
				cmd.arg("-x");
				cmd.arg("ir");
			}
			cmd.arg("-");
			cmd.stdin(std::process::Stdio::piped());
		}
		if !is_lli
		{
			cmd.arg("-o");
			cmd.arg(output_filepath);
		}

		stdout.header("Running", &format!("{:?}", cmd))?;
		stdout.prepare_for_errors()?;
		let mut cmd = cmd.spawn()?;
		if false
		{
			let full_ir = "";
			cmd.stdin
				.as_mut()
				.context("failed to pipe")?
				.write_all(full_ir.as_bytes())?;
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
