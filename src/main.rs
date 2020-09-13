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
		vec!["src/samples/collatz.pn".to_string()]
	};

	for filename in filenames
	{
		println!("{}:::", filename);
		let program = std::fs::read_to_string(filename)?;
		let tokens = lexer::lex(&program)?;
		println!("{:?}", tokens);
		let declarations = parser::parse(tokens)?;
		println!("{:?}", declarations);
		println!();
	}

	Ok(())
}
