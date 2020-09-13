/**/

use penne::lexer;
use penne::parser;

fn main() -> Result<(), anyhow::Error>
{
	let filenames: Vec<String> = if std::env::args().len() > 1
	{
		std::env::args().skip(1).collect()
	}
	else
	{
		vec!["src/samples/goto_end.pn".to_string()]
	};

	for filename in filenames
	{
		println!();
		let program = std::fs::read_to_string(&filename)?;
		println!("Lexing {}...", filename);
		let tokens = lexer::lex(&program)?;
		println!("{:?}", tokens);
		println!();
		println!("Parsing {}...", filename);
		let declarations = parser::parse(tokens)?;
		println!("{:?}", declarations);
		println!();
	}

	Ok(())
}
