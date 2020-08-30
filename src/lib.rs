/**/

pub mod lexer;

#[cfg(test)]
mod tests
{
	use super::*;

	#[test]
	fn parse_do_nothing() -> Result<(), anyhow::Error>
	{
		let program = include_str!("samples/do_nothing.pn");
		let _tokens = lexer::lex(program)?;
		Ok(())
	}
}
