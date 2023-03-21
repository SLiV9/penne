//
// Part of penne
// Copyright (c) 2020 Sander in 't Veld
// License: MIT
//

use penne::analyzer;
use penne::expander;
use penne::generator;
use penne::included;
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

	/// Set target to 'wasm32-unknown-wasi'
	#[clap(short, long)]
	wasm: bool,

	#[clap(flatten)]
	stdout_options: penne::stdout::Options,
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

	#[clap(flatten)]
	stdout_options: penne::stdout::Options,
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

	/// Set target to 'wasm32-unknown-wasi'
	#[clap(short, long)]
	wasm: bool,

	#[clap(flatten)]
	stdout_options: penne::stdout::Options,
}

fn main() -> Result<(), anyhow::Error>
{
	let result = do_main();
	if result.is_err()
	{
		let mut stdout = penne::stdout::StdOut::new(Default::default());
		stdout.prepare_for_errors()?;
	}
	result
}

struct MainArgs
{
	out_dir: Option<std::path::PathBuf>,
	stdout_options: penne::stdout::Options,
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
					stdout_options: args.stdout_options,
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
					stdout_options: args.stdout_options,
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
				stdout_options: args.stdout_options,
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
		stdout_options,
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

	let mut compilation_units = Vec::new();

	for filepath in filepaths
	{
		let filename = filepath.to_string_lossy();
		if let Some((entry, scheme_prefix)) = included::find(&filename)
		{
			match entry
			{
				include_dir::DirEntry::Dir(dir) =>
				{
					let found = dir.find("**/*.pn")?;
					for file in found.filter_map(|e| e.as_file())
					{
						let subpath = std::path::PathBuf::from(format!(
							"{}{}",
							scheme_prefix,
							file.path().to_string_lossy()
						));
						let source = std::str::from_utf8(file.contents())?;
						compilation_units.push((subpath, source.to_string()));
					}
				}
				include_dir::DirEntry::File(file) =>
				{
					let subpath = std::path::PathBuf::from(format!(
						"{}{}",
						scheme_prefix,
						file.path().to_string_lossy()
					));
					let source = std::str::from_utf8(file.contents())?;
					compilation_units.push((subpath, source.to_string()));
				}
			}
		}
		else
		{
			let source = std::fs::read_to_string(&filepath)
				.with_context(|| format!("Failed to open '{}'", filename))?;
			compilation_units.push((filepath, source));
		}
	}

	let mut stdout = penne::stdout::StdOut::new(stdout_options);

	let mut sources = Vec::new();
	let mut modules = Vec::new();

	for (filepath, source) in compilation_units
	{
		let filename = filepath.to_string_lossy().to_string();
		stdout.newline()?;
		stdout.header("Lexing", &filename)?;
		let tokens = lexer::lex(&source, &filename);
		stdout.dump_tokens(&tokens)?;

		stdout.header("Parsing", &filename)?;
		let declarations = parser::parse(tokens);
		stdout.dump_code(&filename, &declarations)?;

		let mut source = source;
		if source.is_empty()
		{
			source.push_str(" ");
		}
		sources.push((filename, source));
		modules.push((filepath, declarations));
	}

	let mut source_cache = ariadne::sources(sources);

	stdout.basic_header("Expanding imports")?;
	expander::expand(&mut modules);
	for (filepath, declarations) in &modules
	{
		let filename = filepath.to_string_lossy().to_string();
		stdout.header("Expanding", &filename)?;
		stdout.dump_code(&filename, &declarations)?;

		// If there are surface level errors during lexing or parsing,
		// or during import expansion, scoping errors are unhelpful.
		// So we abort now, after dumping the code at this stage.
		if let Err(errors) = resolver::check_surface_level_errors(&declarations)
		{
			stdout.prepare_for_errors()?;
			stdout.show_errors(errors, &mut source_cache)?;
			return Err(anyhow!("compilation failed"));
		}
	}

	let mut generated_ir = None;

	for (filepath, declarations) in modules
	{
		let filename = filepath.to_string_lossy().to_string();
		stdout.newline()?;
		stdout.header("Scoping", &filename)?;
		let mut declarations = scoper::analyze(declarations);
		stdout.dump_code(&filename, &declarations)?;

		// Sort the declarations so that the functions are at the end and
		// the constants and structures are declared in the right order.
		let max: u32 = declarations.len().try_into()?;
		declarations.sort_by_key(|x| scoper::get_container_depth(x, max));

		stdout.header("Aligning and declaring", &filename)?;
		let mut typer = typer::Typer::default();
		let mut analyzer = analyzer::Analyzer::default();
		// First, align all structures.
		for declaration in &mut declarations
		{
			typer::align_structure(declaration, &mut typer);
		}
		// Note the `collect()`.
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
		stdout.dump_code(&filename, &declarations)?;

		let mut generator = generator::Generator::new(&filename)?;
		if wasm
		{
			generator.for_wasm()?;
		}
		let mut linter = linter::Linter::default();

		stdout.header("Typing, analyzing and resolving", &filename)?;
		// Type, analyze, lint and resolve the program one declaration at
		// a time, and generate IR for structures and constants in advance.
		// This works because the declarations are sorted by container depth.
		let resolved = declarations.into_iter().try_fold(
			Ok(Vec::new()),
			|acc, declaration| {
				let declaration = typer::analyze(declaration, &mut typer);
				let declaration = analyzer::analyze(declaration, &mut analyzer);
				linter::lint(&declaration, &mut linter);
				let resolved = resolver::resolve(declaration);
				let acc = match (acc, resolved)
				{
					(Ok(mut declarations), Ok(declaration)) =>
					{
						generator::declare(&declaration, &mut generator)?;
						declarations.push(declaration);
						Ok(declarations)
					}
					(Ok(_), Err(errors)) => Err(errors),
					(Err(errors), Ok(_)) => Err(errors),
					(Err(errors), Err(more)) => Err(errors.combined_with(more)),
				};
				Ok::<Result<Vec<penne::Declaration>, _>, anyhow::Error>(acc)
			},
		)?;
		let declarations = match resolved
		{
			Ok(declarations) => declarations,
			Err(errors) =>
			{
				stdout.prepare_for_errors()?;
				stdout.show_errors(errors, &mut source_cache)?;
				return Err(anyhow!("compilation failed"));
			}
		};

		stdout.header("Linting", &filename)?;
		let lints: Vec<_> = linter.into();
		if !lints.is_empty()
		{
			stdout.show_errors(lints, &mut source_cache)?;
		}
		stdout.dump_resolved(&filename, &declarations)?;

		stdout.header("Generating IR for", &filename)?;
		stdout.prepare_for_errors()?;
		for declaration in &declarations
		{
			generator::generate(declaration, &mut generator)?;
		}
		generator.verify();
		let ir = generator.generate_ir()?;
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
			stdout.io_header("Writing to", &outputpath)?;
			std::fs::write(outputpath, ir)?;
		}
		else
		{
			generated_ir = Some(ir);
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
		if generated_ir.is_some()
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

		stdout.cmd_header("Running", format!("{:?}", cmd))?;
		stdout.prepare_for_errors()?;
		let mut cmd = cmd.spawn()?;
		if let Some(full_ir) = generated_ir
		{
			cmd.stdin
				.as_mut()
				.context("failed to pipe")?
				.write_all(full_ir.as_bytes())?;
		}
		let status = cmd.wait()?;
		if is_lli
		{
			let exitcode =
				status.code().ok_or_else(|| error_from_status(status))?;
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

fn error_from_status(status: std::process::ExitStatus) -> anyhow::Error
{
	let signal = if cfg!(unix)
	{
		use std::os::unix::process::ExitStatusExt;
		status.signal()
	}
	else
	{
		None
	};

	if signal == Some(libc::SIGSEGV)
	{
		anyhow!("segmentation fault")
	}
	else
	{
		anyhow!("no exitcode")
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
