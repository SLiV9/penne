/**/

use penne::lexer;

fn main() -> Result<(), anyhow::Error>
{
	let program = include_str!("samples/do_nothing.pn");
	let tokens = lexer::lex(program)?;
	println!("{:?}", tokens);
	Ok(())
}
