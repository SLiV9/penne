/**/

pub mod generator;
pub mod lexer;
pub mod parser;
pub mod rebuilder;
pub mod typer;

#[cfg(test)]
mod tests
{
	use super::*;
	use anyhow::anyhow;
	use pretty_assertions::assert_eq;
	use std::io::Write;

	#[test]
	fn parse_do_nothing() -> Result<(), anyhow::Error>
	{
		let filename = "src/samples/do_nothing.pn";
		let source = std::fs::read_to_string(&filename)?;
		let tokens = lexer::lex(&source, filename);
		let declarations = parser::parse(tokens)?;
		assert_eq!(
			declarations,
			vec![parser::Declaration::Function {
				name: "main".to_string(),
				body: parser::FunctionBody {
					statements: vec![],
					return_value: None,
				},
			}],
		);
		Ok(())
	}

	#[test]
	fn fail_to_parse_invalid_character() -> Result<(), anyhow::Error>
	{
		let filename = "src/samples/invalid_character.pn";
		let source = std::fs::read_to_string(&filename)?;
		let tokens = lexer::lex(&source, filename);
		match parser::parse(tokens)
		{
			Ok(_) => Err(anyhow!("broken test")),
			Err(_) => Ok(()),
		}
	}

	#[test]
	fn fail_to_parse_integer_too_big() -> Result<(), anyhow::Error>
	{
		let filename = "src/samples/integer_too_big.pn";
		let source = std::fs::read_to_string(&filename)?;
		let tokens = lexer::lex(&source, filename);
		match parser::parse(tokens)
		{
			Ok(_) => Err(anyhow!("broken test")),
			Err(_) => Ok(()),
		}
	}

	#[test]
	fn rebuild_foo_bar() -> Result<(), anyhow::Error>
	{
		let filename = "src/samples/foo_bar.pn";
		let source = std::fs::read_to_string(&filename)?;
		let tokens = lexer::lex(&source, filename);
		let declarations = parser::parse(tokens)?;
		let indentation = rebuilder::Indentation {
			value: "\t",
			amount: 0,
		};
		let code = rebuilder::rebuild(&declarations, &indentation)?;
		let code_lines: Vec<&str> = code.lines().collect();
		let source_lines: Vec<&str> = source.lines().collect();
		assert_eq!(code_lines, source_lines);
		Ok(())
	}

	#[test]
	fn execute_five() -> Result<(), anyhow::Error>
	{
		let result = execute_calculation("src/samples/five.pn")?;
		assert_eq!(result, 5);
		Ok(())
	}

	#[test]
	fn execute_two_plus_seven() -> Result<(), anyhow::Error>
	{
		let result = execute_calculation("src/samples/two_plus_seven.pn")?;
		assert_eq!(result, 9);
		Ok(())
	}

	#[test]
	fn execute_addition() -> Result<(), anyhow::Error>
	{
		let result = execute_calculation("src/samples/addition.pn")?;
		assert_eq!(result, 10);
		Ok(())
	}

	fn execute_calculation(filename: &str) -> Result<i32, anyhow::Error>
	{
		let source = std::fs::read_to_string(&filename)?;
		let tokens = lexer::lex(&source, filename);
		let declarations = parser::parse(tokens)?;
		let declarations = typer::analyze(declarations)?;
		let ir = generator::generate(&declarations, filename)?;
		let mut cmd = std::process::Command::new("lli")
			.stdin(std::process::Stdio::piped())
			.spawn()?;
		cmd.stdin.as_mut().unwrap().write_all(ir.as_bytes())?;
		let status = cmd.wait()?;
		let exitcode = status.code().unwrap();
		Ok(exitcode)
	}
}
