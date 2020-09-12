/**/

pub mod lexer;
pub mod parser;

#[cfg(test)]
mod tests
{
	use super::*;

	#[test]
	fn parse_do_nothing() -> Result<(), anyhow::Error>
	{
		let source = include_str!("samples/do_nothing.pn");
		let tokens = lexer::lex(source)?;
		let declarations = parser::parse(tokens)?;
		assert_eq!(
			declarations,
			vec![parser::Declaration::Function {
				name: "main".to_string(),
				body: vec![]
			}]
		);
		Ok(())
	}
}
