/**/

use penne::analyzer;
use penne::common::Declaration;
use penne::generator;
use penne::lexer;
use penne::linter;
use penne::parser;
use penne::rebuilder;
use penne::scoper;
use penne::typer;

use std::collections::HashMap;
use std::io::Write;

use anyhow::anyhow;
use anyhow::Context;
use termcolor::{Color, ColorChoice, ColorSpec, StandardStream, WriteColor};

fn main() -> Result<(), anyhow::Error>
{
	let result = do_main();
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

fn do_main() -> Result<(), anyhow::Error>
{
	let mut stdout = StandardStream::stdout(ColorChoice::Auto);

	let filenames: Vec<String> = if std::env::args().len() > 1
	{
		std::env::args().skip(1).collect()
	}
	else
	{
		vec!["src/samples/five.pn".to_string()]
	};

	let mut modules = HashMap::new();

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

	for filename in filenames
	{
		writeln!(stdout)?;
		let program = std::fs::read_to_string(&filename)?;
		stdout.set_color(&colorspec_header)?;
		writeln!(stdout, "Lexing {}...", filename)?;
		let tokens = lexer::lex(&program, &filename);
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
		stdout.set_color(&colorspec_header)?;
		writeln!(stdout, "Parsing {}...", filename)?;
		let declarations = parser::parse(tokens)?;
		stdout.set_color(&colorspec_dump)?;
		writeln!(stdout, "{:?}", declarations)?;
		writeln!(stdout)?;
		stdout.set_color(&colorspec_header)?;
		writeln!(stdout, "Preprocessing {}...", filename)?;
		let declarations = preprocess(declarations, &filename, &modules)?;
		stdout.set_color(&colorspec_dump)?;
		writeln!(stdout, "{:?}", declarations)?;
		writeln!(stdout)?;
		stdout.set_color(&colorspec_header)?;
		writeln!(stdout, "Rebuilding {}...", filename)?;
		let indentation = rebuilder::Indentation {
			value: "\u{00a6}   ",
			amount: 1,
		};
		let code = rebuilder::rebuild(&declarations, &indentation)?;
		stdout.set_color(&colorspec_dump)?;
		writeln!(stdout, "{}", code)?;
		stdout.set_color(&colorspec_header)?;
		writeln!(stdout, "Scoping {:?}...", filename)?;
		let declarations = scoper::analyze(declarations)?;
		stdout.set_color(&colorspec_dump)?;
		writeln!(stdout, "{:?}", declarations)?;
		writeln!(stdout)?;
		stdout.set_color(&colorspec_header)?;
		writeln!(stdout, "Typing {:?}...", filename)?;
		let declarations = typer::analyze(declarations)?;
		stdout.set_color(&colorspec_dump)?;
		writeln!(stdout, "{:?}", declarations)?;
		writeln!(stdout)?;
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
		stdout.set_color(&colorspec_header)?;
		writeln!(stdout, "Analyzing {:?}...", filename)?;
		analyzer::analyze(&declarations)?;
		stdout.set_color(&colorspec_success)?;
		writeln!(stdout, "Analysis complete.")?;
		writeln!(stdout)?;
		stdout.set_color(&colorspec_header)?;
		writeln!(stdout, "Linting {:?}...", filename)?;
		let lints = linter::lint(&declarations);
		if !lints.is_empty()
		{
			stdout.set_color(&colorspec_warning)?;
			for lint in lints
			{
				writeln!(stdout, "Warning: {:?}", lint)?;
			}
		}
		else
		{
			stdout.set_color(&colorspec_success)?;
			writeln!(stdout, "Linting complete.")?;
		}
		writeln!(stdout)?;
		stdout.set_color(&colorspec_header)?;
		write!(stdout, "Generating IR for {}...", filename)?;
		stdout.set_color(&colorspec_error)?;
		writeln!(stdout)?;
		let ir = generator::generate(&declarations, &filename)?;
		stdout.set_color(&colorspec_dump)?;
		writeln!(stdout, "{}", ir)?;
		let outputfilename = format!("bin/{}.ll", filename);
		let dirname = std::path::Path::new(&outputfilename).parent().unwrap();
		std::fs::create_dir_all(dirname)?;
		stdout.set_color(&colorspec_header)?;
		writeln!(stdout, "Writing to {}...", outputfilename)?;
		std::fs::write(outputfilename, ir)?;
		// Store the declarations for later use.
		modules.insert(filename, declarations);
	}

	stdout.reset()?;
	writeln!(stdout, "Done.")?;

	Ok(())
}

fn preprocess(
	mut declarations: Vec<Declaration>,
	filename: &str,
	modules: &HashMap<String, Vec<Declaration>>,
) -> Result<Vec<Declaration>, anyhow::Error>
{
	let relative_path = std::path::Path::new(filename);

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
			match resolved.to_str().map(|x| modules.get(x)).flatten()
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
