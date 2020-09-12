/**/

use crate::lexer::Token;

use std::collections::VecDeque;

use anyhow::anyhow;

#[derive(Debug, PartialEq, Eq)]
pub enum Declaration
{
	Function
	{
		name: String,
		//parameters: Vec<Parameter>,
		body: Vec<Statement>,
	},
}

#[derive(Debug, PartialEq, Eq)]
pub enum Statement {}

pub fn parse(tokens: Vec<Token>) -> Result<Vec<Declaration>, anyhow::Error>
{
	let mut declarations = Vec::new();

	let mut tokens = VecDeque::from(tokens);
	while !tokens.is_empty()
	{
		let declaration = parse_declaration(&mut tokens)?;
		declarations.push(declaration);
	}

	Ok(declarations)
}

fn parse_declaration(
	tokens: &mut VecDeque<Token>,
) -> Result<Declaration, anyhow::Error>
{
	match tokens.pop_front()
	{
		Some(Token::Fn) => (),
		Some(token) =>
		{
			return Err(anyhow!("invalid top-level declaration '{:?}'", token));
		}
		None =>
		{
			return Err(anyhow!(
				"unexpected end of tokens, expected declaration"
			));
		}
	}

	let name = match tokens.pop_front()
	{
		Some(Token::Identifier(name)) => name,
		Some(token) =>
		{
			return Err(anyhow!(
				"unexpected '{:?}', expected identifier",
				token
			));
		}
		None =>
		{
			return Err(anyhow!(
				"unexpected end of tokens, expected identifier",
			));
		}
	};

	match tokens.pop_front()
	{
		Some(Token::ParenLeft) => (),
		Some(token) =>
		{
			return Err(anyhow!(
				"unexpected '{:?}', expected left paren",
				token
			));
		}
		None =>
		{
			return Err(anyhow!(
				"unexpected end of tokens, expected declaration"
			));
		}
	}

	match tokens.pop_front()
	{
		Some(Token::ParenRight) => (),
		Some(token) =>
		{
			return Err(anyhow!(
				"unexpected '{:?}', expected left paren",
				token
			));
		}
		None =>
		{
			return Err(anyhow!(
				"unexpected end of tokens, expected declaration"
			));
		}
	}

	let function = Declaration::Function {
		name,
		body: parse_body(tokens)?,
	};

	Ok(function)
}

fn parse_body(
	tokens: &mut VecDeque<Token>,
) -> Result<Vec<Statement>, anyhow::Error>
{
	match tokens.pop_front()
	{
		Some(Token::BraceLeft) => (),
		Some(token) =>
		{
			return Err(anyhow!(
				"unexpected '{:?}', expected left paren",
				token
			));
		}
		None =>
		{
			return Err(anyhow!(
				"unexpected end of tokens, expected declaration"
			));
		}
	}

	let mut statements = Vec::new();

	while tokens.front().is_some()
	{
		if let Some(Token::BraceRight) = tokens.front()
		{
			tokens.pop_front();
			break;
		}

		let statement = parse_statement(tokens)?;
		statements.push(statement);
	}

	Ok(statements)
}

fn parse_statement(
	_tokens: &mut VecDeque<Token>,
) -> Result<Statement, anyhow::Error>
{
	Err(anyhow!("there are no statements yet"))
}
