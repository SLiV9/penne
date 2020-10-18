/**/

use penne::analyzer;
use penne::generator;
use penne::lexer;
use penne::parser;
use penne::rebuilder;
use penne::typer;

fn main() -> Result<(), anyhow::Error>
{
	let filenames: Vec<String> = if std::env::args().len() > 1
	{
		std::env::args().skip(1).collect()
	}
	else
	{
		vec!["src/samples/five.pn".to_string()]
	};

	for filename in filenames
	{
		println!();
		let program = std::fs::read_to_string(&filename)?;
		println!("Lexing {}...", filename);
		let tokens = lexer::lex(&program, &filename);
		println!("{:?}", tokens);
		println!();
		println!("Parsing {}...", filename);
		let declarations = parser::parse(tokens)?;
		println!("{:?}", declarations);
		println!();
		println!("Rebuilding {}...", filename);
		let indentation = rebuilder::Indentation {
			value: "\u{00a6}   ",
			amount: 1,
		};
		let code = rebuilder::rebuild(&declarations, &indentation)?;
		println!("{}", code);
		println!("Typing {:?}...", filename);
		let declarations = typer::analyze(declarations)?;
		println!("{:?}", declarations);
		println!("Analyzing {:?}...", filename);
		analyzer::analyze(&declarations)?;
		println!("Analysis complete.");
		println!();
		println!("Generating IR for {}...", filename);
		let ir = generator::generate(&declarations, &filename)?;
		println!("{}", ir);
		let outputfilename = format!("bin/{}.ll", filename);
		let dirname = std::path::Path::new(&outputfilename).parent().unwrap();
		std::fs::create_dir_all(dirname)?;
		println!("Writing to {}...", outputfilename);
		std::fs::write(outputfilename, ir)?;
	}
	println!("Done.");

	Ok(())
}
