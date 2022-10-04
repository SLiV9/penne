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
use penne::rebuilder;
use penne::resolver;
use penne::scoper;
use penne::typer;

use std::collections::HashMap;
use std::io::Write;

use anyhow::anyhow;
use anyhow::Context;
use clap::Parser;
use termcolor::{Color, ColorChoice, ColorSpec, StandardStream, WriteColor};

#[cfg(feature = "logging")]
use env_logger;

#[derive(clap::Parser)]
#[command(version)]
struct Args
{
	#[clap(value_parser, required(true))]
	filepaths: Vec<std::path::PathBuf>,

	/// Which LLVM executable to pipe the generated IR into (default: 'clang')
	#[clap(short, long)]
	backend: Option<std::path::PathBuf>,

	/// Write binary output to this file
	#[clap(short)]
	output_filepath: Option<std::path::PathBuf>,

	/// Write binary output and generated IR to this directory
	#[clap(long)]
	out_dir: Option<std::path::PathBuf>,

	/// Show the exitcode of the backend (useful when backend is 'lli')
	#[clap(long)]
	print_exitcode: bool,

	/// Show a lot of intermediate output
	#[clap(short, long)]
	verbose: bool,

	/// Set target to 'wasm32-unknown-wasi'
	#[clap(short, long)]
	wasm: bool,
}

fn main() -> Result<(), anyhow::Error>
{
	let args = Args::parse();

	let result = do_main(args);
	if result.is_err()
	{
		let mut stdout = StandardStream::stdout(ColorChoice::Auto);
		let colorspec_error = ColorSpec::new()
			.set_fg(Some(Color::Red))
			.set_bold(true)
			.to_owned();
		stdout.set_color(&colorspec_error)?;
		writeln!(stdout)?;
	}
	result
}

fn do_main(args: Args) -> Result<(), anyhow::Error>
{
	let Args {
		filepaths,
		backend: arg_backend,
		output_filepath,
		out_dir: arg_out_dir,
		wasm,
		verbose,
		print_exitcode,
	} = args;
	let config_backend = None;
	let config_out_dir = None;
	let backend = get_backend(arg_backend, config_backend)?;
	let out_dir = arg_out_dir.or(config_out_dir);

	let output_filepath = output_filepath.or_else(|| {
		filepaths
			.iter()
			.last()
			.and_then(|filepath| filepath.file_name())
			.and_then(|filename| {
				let mut path =
					out_dir.clone().unwrap_or(std::path::PathBuf::new());
				path.push(filename);
				if wasm
				{
					path.set_extension("wasm");
				}
				else
				{
					path.set_extension(get_exe_extension());
				}
				Some(path)
			})
	});

	let mut stdout = StandardStream::stdout(ColorChoice::Auto);
	let colorspec_header = ColorSpec::new().to_owned();
	let colorspec_dump = ColorSpec::new().set_dimmed(true).to_owned();
	let colorspec_error = ColorSpec::new()
		.set_fg(Some(Color::Red))
		.set_bold(true)
		.to_owned();
	let colorspec_warning = ColorSpec::new()
		.set_fg(Some(Color::Yellow))
		.set_bold(true)
		.to_owned();
	let colorspec_success =
		ColorSpec::new().set_fg(Some(Color::Green)).to_owned();

	let mut modules = HashMap::new();
	let mut backend_source = BackendSource::None;

	for filepath in filepaths
	{
		let filename = filepath.to_string_lossy().to_string();
		writeln!(stdout)?;
		let program = std::fs::read_to_string(&filepath)?;
		if verbose
		{
			stdout.set_color(&colorspec_header)?;
			writeln!(stdout, "Lexing {}...", filename)?;
		}
		let tokens = lexer::lex(&program, &filename);
		if verbose
		{
			stdout.set_color(&colorspec_dump)?;
			writeln!(stdout, "{:?}", tokens)?;
			writeln!(stdout)?;
			for token in &tokens
			{
				match &token.result
				{
					Result::Ok(token) => write!(stdout, "{:?}   ", token)?,
					Result::Err(_) => write!(stdout, "ERROR   ")?,
				}
			}
			writeln!(stdout)?;
			writeln!(stdout)?;
		}
		if verbose
		{
			stdout.set_color(&colorspec_header)?;
			writeln!(stdout, "Parsing {}...", filename)?;
		}
		let declarations = parser::parse(tokens)?;
		if verbose
		{
			stdout.set_color(&colorspec_dump)?;
			writeln!(stdout, "{:?}", declarations)?;
			writeln!(stdout)?;
		}
		if verbose
		{
			stdout.set_color(&colorspec_header)?;
			writeln!(stdout, "Preprocessing {}...", filename)?;
		}
		let declarations = preprocess(declarations, &filepath, &modules)?;
		if verbose
		{
			stdout.set_color(&colorspec_dump)?;
			writeln!(stdout, "{:?}", declarations)?;
			writeln!(stdout)?;
		}
		if verbose
		{
			stdout.set_color(&colorspec_header)?;
			writeln!(stdout, "Rebuilding {}...", filename)?;
			let indentation = rebuilder::Indentation {
				value: "\u{00a6}   ",
				amount: 1,
			};
			let code = rebuilder::rebuild(&declarations, &indentation)?;
			stdout.set_color(&colorspec_dump)?;
			writeln!(stdout, "{}", code)?;
		}
		if verbose
		{
			stdout.set_color(&colorspec_header)?;
			writeln!(stdout, "Scoping {}...", filename)?;
		}
		let declarations = scoper::analyze(declarations)?;
		if verbose
		{
			stdout.set_color(&colorspec_dump)?;
			writeln!(stdout, "{:?}", declarations)?;
			writeln!(stdout)?;
		}
		if verbose
		{
			stdout.set_color(&colorspec_header)?;
			writeln!(stdout, "Typing {}...", filename)?;
		}
		let declarations = typer::analyze(declarations)?;
		if verbose
		{
			stdout.set_color(&colorspec_dump)?;
			writeln!(stdout, "{:?}", declarations)?;
			writeln!(stdout)?;
		}
		if verbose
		{
			stdout.set_color(&colorspec_header)?;
			writeln!(stdout, "Rebuilding {}...", filename)?;
			let indentation = rebuilder::Indentation {
				value: "\u{00a6}   ",
				amount: 1,
			};
			let code = rebuilder::rebuild(&declarations, &indentation)?;
			stdout.set_color(&colorspec_dump)?;
			writeln!(stdout, "{}", code)?;
			writeln!(stdout)?;
		}
		if verbose
		{
			stdout.set_color(&colorspec_header)?;
			writeln!(stdout, "Analyzing {}...", filename)?;
		}
		analyzer::analyze(&declarations)?;
		if verbose
		{
			stdout.set_color(&colorspec_success)?;
			writeln!(stdout, "Analysis complete.")?;
			writeln!(stdout)?;
		}
		let forward_declarations = declarations
			.iter()
			.flat_map(|x| create_forward_declaration(x))
			.collect();
		if verbose
		{
			stdout.set_color(&colorspec_header)?;
			writeln!(stdout, "Linting {}...", filename)?;
		}
		let lints = linter::lint(&declarations);
		if !lints.is_empty()
		{
			stdout.set_color(&colorspec_warning)?;
			for lint in lints
			{
				writeln!(stdout, "Warning: {:?}", lint)?;
			}
			writeln!(stdout)?;
		}
		else if verbose
		{
			stdout.set_color(&colorspec_success)?;
			writeln!(stdout, "Linting complete.")?;
			writeln!(stdout)?;
		}
		if verbose
		{
			stdout.set_color(&colorspec_header)?;
			writeln!(stdout, "Resolving {}...", filename)?;
		}
		let declarations = resolver::resolve(declarations)?;
		if verbose
		{
			stdout.set_color(&colorspec_dump)?;
			writeln!(stdout, "{:?}", declarations)?;
			writeln!(stdout)?;
		}
		if verbose
		{
			stdout.set_color(&colorspec_header)?;
			write!(stdout, "Generating IR for {}...", filename)?;
		}
		stdout.set_color(&colorspec_error)?;
		writeln!(stdout)?;
		let ir = generator::generate(&declarations, &filename, wasm)?;
		if verbose
		{
			stdout.set_color(&colorspec_dump)?;
			writeln!(stdout, "{}", ir)?;
		}
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
			stdout.set_color(&colorspec_header)?;
			writeln!(stdout, "Writing to {}...", outputpath.to_string_lossy())?;
			std::fs::write(outputpath, ir)?;
		}
		else
		{
			backend_source = BackendSource::Stdin(ir);
		}
		// Store the declarations for later use.
		modules.insert(filepath, forward_declarations);
	}

	if let Some(output_filepath) = output_filepath
	{
		if verbose
		{
			stdout.set_color(&colorspec_header)?;
			write!(stdout, "Compiling...")?;
		}
		stdout.set_color(&colorspec_error)?;
		writeln!(stdout)?;

		let mut cmd = std::process::Command::new(&backend);
		match &backend_source
		{
			BackendSource::File(filepath) =>
			{
				cmd.arg(filepath);
			}
			BackendSource::Stdin(_) =>
			{
				cmd.arg("-x");
				cmd.arg("ir");
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
		if print_exitcode
		{
			let exitcode = status.code().context("no exitcode")?;
			stdout.reset()?;
			writeln!(stdout, "Output: {}", exitcode)?;
		}
		else
		{
			status
				.success()
				.then(|| Some(()))
				.context("compilation unsuccessful")?;
		}
	}

	stdout.reset()?;
	writeln!(stdout, "Done.")?;

	Ok(())
}

fn preprocess(
	mut declarations: Vec<Declaration>,
	relative_path: &std::path::Path,
	modules: &HashMap<std::path::PathBuf, Vec<Declaration>>,
) -> Result<Vec<Declaration>, anyhow::Error>
{
	while let Some(i) = declarations.iter().position(|d| is_directive(d))
	{
		let (directive, location) = match declarations.remove(i)
		{
			Declaration::PreprocessorDirective {
				directive,
				location,
			} => (directive, location),
			_ => unreachable!(),
		};
		let mut is_resolved = false;
		let mut resolutions = relative_path.ancestors().skip(1);
		while let Some(path) = resolutions.next()
		{
			let resolved = path.join(directive.clone());
			match modules.get(&resolved)
			{
				Some(result) =>
				{
					let imported_declarations = result.to_vec();
					is_resolved = true;
					declarations.splice(i..i, imported_declarations);
					break;
				}
				None => (),
			}
		}
		if !is_resolved
		{
			return Err(anyhow!("failed to resolve {:?}", directive))
				.context(location.format())
				.context("failed to preprocess");
		}
	}
	Ok(declarations)
}

fn is_directive(declaration: &Declaration) -> bool
{
	match declaration
	{
		Declaration::PreprocessorDirective { .. } => true,
		_ => false,
	}
}

fn create_forward_declaration(declaration: &Declaration)
	-> Option<Declaration>
{
	match declaration
	{
		Declaration::Constant {
			name,
			value,
			value_type,
			flags,
		} =>
		{
			if flags.contains(DeclarationFlag::Public)
			{
				Some(Declaration::Constant {
					name: name.clone(),
					value: value.clone(),
					value_type: value_type.clone(),
					flags: *flags,
				})
			}
			else
			{
				None
			}
		}
		Declaration::Function {
			name,
			parameters,
			body: _,
			return_type,
			flags,
		}
		| Declaration::FunctionHead {
			name,
			parameters,
			return_type,
			flags,
		} =>
		{
			if flags.contains(DeclarationFlag::Public)
			{
				Some(Declaration::FunctionHead {
					name: name.clone(),
					parameters: parameters.clone(),
					return_type: return_type.clone(),
					flags: *flags,
				})
			}
			else
			{
				None
			}
		}
		Declaration::PreprocessorDirective { .. } => unreachable!(),
	}
}

fn get_backend(
	arg_backend: Option<std::path::PathBuf>,
	config_backend: Option<std::path::PathBuf>,
) -> Result<String, anyhow::Error>
{
	arg_backend
		.map(|x| x.to_str().map(|x| x.to_string()).context("invalid backend"))
		.or_else(|| match std::env::var("PENNEC_BACKEND")
		{
			Ok(value) => Some(Ok(value.into())),
			Err(std::env::VarError::NotPresent) => None,
			Err(e) => return Some(Err(e.into())),
		})
		.or(config_backend.map(|x| {
			x.to_str()
				.map(|x| x.to_string())
				.context("invalid backend in config")
		}))
		.unwrap_or_else(|| Ok("clang".to_string()))
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
