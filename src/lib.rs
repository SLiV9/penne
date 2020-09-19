/**/

pub mod analyzer;
pub mod generator;
pub mod lexer;
pub mod parser;
pub mod rebuilder;

#[cfg(test)]
mod tests
{
	use super::*;
	use std::io::Write;

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
				body: parser::Block {
					statements: vec![],
					value: parser::Expression::Void
				}
			}]
		);
		Ok(())
	}

	#[test]
	fn rebuild_foo_bar() -> Result<(), anyhow::Error>
	{
		let source = include_str!("samples/foo_bar.pn");
		let tokens = lexer::lex(source)?;
		let declarations = parser::parse(tokens)?;
		let indentation = rebuilder::Indentation {
			value: "\t",
			amount: 0,
		};
		let code = rebuilder::rebuild(&declarations, &indentation)?;
		assert_eq!(code, source);
		Ok(())
	}

	#[test]
	fn execute_five() -> Result<(), anyhow::Error>
	{
		let source = include_str!("samples/five.pn");
		let tokens = lexer::lex(source)?;
		let declarations = parser::parse(tokens)?;
		let declarations = analyzer::analyze(declarations)?;
		let ir = generator::generate(&declarations, "samples/five.pn")?;
		let mut cmd = std::process::Command::new("lli")
			.stdin(std::process::Stdio::piped())
			.spawn()?;
		cmd.stdin.as_mut().unwrap().write_all(ir.as_bytes())?;
		let status = cmd.wait()?;
		let exitcode = status.code().unwrap();
		assert_eq!(exitcode, 5);
		Ok(())
	}
}
