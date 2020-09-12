/**/

use penne::lexer;
use penne::parser;

fn main() -> Result<(), anyhow::Error>
{
	let program = include_str!("samples/do_nothing.pn");
	let tokens = lexer::lex(program)?;
	println!("{:?}", tokens);
	let declarations = parser::parse(tokens)?;
	println!("{:?}", declarations);
	Ok(())
}
