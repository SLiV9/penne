//
// Part of penne
// Copyright (c) 2020 Sander in 't Veld
// License: MIT
//

use penne::alpha::expander;
use penne::alpha::included;
use penne::alpha::lexer;
use penne::alpha::parser;
use penne::alpha::resolver;
use penne::alpha::scoper;
use penne::alpha::stdout;
use penne::alpha::Compiler;
use penne::delta::fuzzer;

use std::io::Write;

use anyhow::anyhow;
use anyhow::Context;
use clap::Parser;
use serde::Deserialize;

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

impl Cli
{
	fn get_fuzz_args(&self) -> Option<&FuzzArgs>
	{
		match self.sub.as_ref()
		{
			Some(Subcommand::Fuzz(args)) => Some(args),
			_ => None,
		}
	}
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
	/// Generate random source files
	Fuzz(FuzzArgs),
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
	stdout_options: stdout::Options,
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
	stdout_options: stdout::Options,
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
	stdout_options: stdout::Options,
}

#[derive(Debug, Default, Deserialize, clap::Args)]
#[serde(default, deny_unknown_fields)]
struct FuzzArgs
{
	/// Generate random source file using the provided fuzzing strategy.
	#[clap(value_parser, required(true))]
	strategy: fuzzer::FuzzingStrategy,

	/// How many KB of random source file to generate.
	#[clap(long, required(true))]
	kb: usize,

	/// How many random mistakes to inject into the source file.
	#[clap(long)]
	mistakes: Option<usize>,

	/// Write generated source files to this directory
	#[clap(long)]
	out_dir: Option<std::path::PathBuf>,

	#[clap(flatten)]
	stdout_options: stdout::Options,
}

fn main() -> Result<(), anyhow::Error>
{
	let result = do_main();
	if result.is_err()
	{
		stdout::StdOut::new(Default::default());
		println!();
		println!();
	}
	result
}

struct MainArgs
{
	out_dir: Option<std::path::PathBuf>,
	stdout_options: stdout::Options,
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
			Cli {
				sub: Some(Subcommand::Fuzz(_)),
				build: _,
			} => unimplemented!(),
		}
	}
}

fn do_main() -> Result<(), anyhow::Error>
{
	let args = Cli::parse();
	if let Some(fuzz_args) = args.get_fuzz_args()
	{
		return do_fuzzing(fuzz_args);
	}

	let args = args.try_into()?;
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

	let mut stdout = stdout::StdOut::new(stdout_options);

	let output_filepath = output_filepath
		.or_else(|| derive_output_filepath(&filepaths, out_dir.as_ref(), wasm));
	let compilation_units = gather_compilation_units(filepaths)?;

	let generated_ir = if cfg!(feature = "delta")
	{
		compile_to_ir_using_delta(
			compilation_units,
			out_dir.as_ref(),
			wasm,
			&mut stdout,
		)?
	}
	else
	{
		compile_to_ir_using_alpha(
			compilation_units,
			out_dir.as_ref(),
			wasm,
			&mut stdout,
		)?
	};

	if let Some(output_filepath) = output_filepath.filter(|_x| !skip_backend)
	{
		generate_output(
			output_filepath,
			Some(generated_ir),
			is_lli,
			backend,
			backend_args,
			link_args,
			&mut stdout,
		)?;
	}

	stdout.done()?;
	Ok(())
}

fn derive_output_filepath(
	filepaths: &[std::path::PathBuf],
	out_dir: Option<&std::path::PathBuf>,
	for_wasm: bool,
) -> Option<std::path::PathBuf>
{
	filepaths
		.get(0)
		.and_then(|filepath| filepath.file_name())
		.map(|filename| {
			let mut path = out_dir.map(|x| x.to_path_buf()).unwrap_or_default();
			path.push(filename);
			if for_wasm
			{
				path.set_extension("wasm");
			}
			else
			{
				path.set_extension(get_exe_extension());
			}
			path
		})
}

fn gather_compilation_units(
	filepaths: Vec<std::path::PathBuf>,
) -> Result<Vec<(std::path::PathBuf, String)>, anyhow::Error>
{
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

	Ok(compilation_units)
}

fn compile_to_ir_using_alpha(
	compilation_units: Vec<(std::path::PathBuf, String)>,
	out_dir: Option<&std::path::PathBuf>,
	for_wasm: bool,
	stdout: &mut stdout::StdOut,
) -> Result<String, anyhow::Error>
{
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

	let mut compiler = Compiler::default();
	if for_wasm
	{
		compiler.for_wasm()?;
	}

	for (filepath, declarations) in modules
	{
		let filename = filepath.to_string_lossy().to_string();
		stdout.newline()?;
		stdout.header("Scoping", &filename)?;
		let declarations = scoper::analyze(declarations);
		stdout.dump_code(&filename, &declarations)?;

		compiler.add_module(&filename)?;

		stdout.header("Typing, analyzing and resolving", &filename)?;
		stdout.prepare_for_errors()?;
		let resolved = compiler.analyze_and_resolve(declarations)?;
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
		stdout.dump_resolved(&filename, &declarations)?;

		stdout.header("Linting", &filename)?;
		let lints = compiler.take_lints();
		stdout.linting(lints.is_empty())?;
		if !lints.is_empty()
		{
			stdout.show_errors(lints, &mut source_cache)?;
		}
		stdout.newline()?;

		stdout.header("Generating IR for", &filename)?;
		stdout.prepare_for_errors()?;
		compiler.compile(&declarations)?;
		let ir = compiler.generate_ir()?;
		stdout.dump_text(&ir)?;

		if let Some(out_dir) = &out_dir
		{
			let outputpath = {
				let mut path = out_dir.to_path_buf();
				path.push(filepath.clone());
				path.set_extension("pn.ll");
				path
			};
			let dirname = outputpath.parent().context("invalid output dir")?;
			std::fs::create_dir_all(dirname)?;
			stdout.io_header("Writing to", &outputpath)?;
			std::fs::write(outputpath, ir)?;
		}
	}

	stdout.newline()?;
	stdout.basic_header("Linking modules")?;
	stdout.prepare_for_errors()?;
	compiler.link_modules()?;
	let full_ir = compiler.generate_ir()?;
	stdout.dump_text(&full_ir)?;

	Ok(full_ir)
}

fn compile_to_ir_using_delta(
	compilation_units: Vec<(std::path::PathBuf, String)>,
	out_dir: Option<&std::path::PathBuf>,
	for_wasm: bool,
	stdout: &mut stdout::StdOut,
) -> Result<String, anyhow::Error>
{
	let mut sources = Vec::new();
	let mut modules = Vec::new();

	for (filepath, source) in compilation_units
	{
		let filename = filepath.to_string_lossy().to_string();
		stdout.newline()?;
		stdout.header("Lexing", &filename)?;
		let tokens = penne::delta::lexer::lex(&source, &filename);
		stdout.dump_delta_tokens(&tokens)?;

		let mut source = source;
		if source.is_empty()
		{
			source.push_str(" ");
		}
		sources.push((filename, source));

		if let Some(errors) = tokens.errors()
		{
			let mut source_cache = ariadne::sources(sources);
			stdout.prepare_for_errors()?;
			stdout.show_errors(errors, &mut source_cache)?;
			return Err(anyhow!("compilation failed"));
		}

		modules.push((filepath, tokens));

		// TODO parse
	}

	// TODO finish
	let _ = for_wasm;
	let _ = out_dir;
	Err(anyhow!("unfinished"))
}

fn generate_output(
	output_filepath: std::path::PathBuf,
	generated_ir: Option<String>,
	is_lli: bool,
	backend: String,
	backend_args: Option<Vec<String>>,
	link_args: Option<Vec<String>>,
	stdout: &mut stdout::StdOut,
) -> Result<(), anyhow::Error>
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

fn do_fuzzing(args: &FuzzArgs) -> Result<(), anyhow::Error>
{
	let FuzzArgs {
		out_dir,
		strategy,
		kb,
		mistakes,
		stdout_options,
	} = args;
	let strategy = *strategy;
	let kb = *kb;
	let mistakes = mistakes.clone().unwrap_or(0);

	let mut stdout = stdout::StdOut::new(stdout_options.clone());

	let output_filepath = out_dir.as_ref().map(|out_dir| {
		let mut path = out_dir.to_path_buf();
		path.push(format!("fuzzed_{strategy}"));
		path.set_extension("pn");
		path
	});

	match strategy
	{
		fuzzer::FuzzingStrategy::Tokens =>
		{
			stdout.basic_header("Fuzzing tokens")?;
			let capacity = kb * 1096;
			let mut buffer = String::with_capacity(capacity);
			fuzzer::fill_to_capacity_with_tokens(95, &mut buffer, mistakes)?;
			stdout.dump_text(&format!("Fuzzed {} bytes", buffer.len()))?;

			if let Some(output_filepath) = output_filepath
			{
				stdout.io_header("Writing to", &output_filepath)?;
				std::fs::write(output_filepath, buffer)?;
			}
			else
			{
				stdout.basic_header("Dumping")?;
				stdout.dump_text(&buffer)?;
			}
		}
	}

	stdout.done()?;
	Ok(())
}
