/**/

use penne::lexer;
use penne::parser;

fn main() -> Result<(), anyhow::Error>
{
	let program = include_str!("samples/two_plus_seven.pn");
	let tokens = lexer::lex(program)?;
	println!("{:?}", tokens);
	let declarations = parser::parse(tokens)?;
	println!("{:?}", declarations);
	Ok(())
}
