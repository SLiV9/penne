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
		body: Block,
	},
}

#[derive(Debug, PartialEq, Eq)]
pub struct Block
{
	pub statements: Vec<Statement>,
	pub value: Expression,
}

#[derive(Debug, PartialEq, Eq)]
pub enum Statement {}

#[derive(Debug, PartialEq, Eq)]
pub enum Expression
{
	Binary
	{
		op: BinaryOp,
		left: Box<Expression>,
		right: Box<Expression>,
	},
	Literal(Literal),
	Void,
}

#[derive(Debug, PartialEq, Eq)]
pub enum BinaryOp
{
	Add,
	Subtract,
}

#[derive(Debug, PartialEq, Eq)]
pub enum Literal
{
	Int32(i32),
}

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

	let body = parse_block(tokens)?;

	let function = Declaration::Function { name, body };

	Ok(function)
}

fn parse_block(tokens: &mut VecDeque<Token>) -> Result<Block, anyhow::Error>
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

	loop
	{
		if let Some(Token::BraceRight) = tokens.front()
		{
			tokens.pop_front();

			let block = Block {
				statements,
				value: Expression::Void,
			};
			return Ok(block);
		}

		let result = parse_statement_or_final_expression(tokens)?;
		match result
		{
			StatementOrFinalExpression::Statement(statement) =>
			{
				statements.push(statement);
			}
			StatementOrFinalExpression::FinalExpression(expression) =>
			{
				if let Some(Token::BraceRight) = tokens.pop_front()
				{
					let block = Block {
						statements,
						value: expression,
					};
					return Ok(block);
				}
				else
				{
					return Err(anyhow!("expected closing brace"));
				}
			}
		}
	}
}

#[derive(Debug)]
enum StatementOrFinalExpression
{
	Statement(Statement),
	FinalExpression(Expression),
}

fn parse_statement_or_final_expression(
	tokens: &mut VecDeque<Token>,
) -> Result<StatementOrFinalExpression, anyhow::Error>
{
	let expression = parse_expression(tokens)?;

	Ok(StatementOrFinalExpression::FinalExpression(expression))
}

fn parse_expression(
	tokens: &mut VecDeque<Token>,
) -> Result<Expression, anyhow::Error>
{
	parse_addition(tokens)
}

fn parse_addition(
	tokens: &mut VecDeque<Token>,
) -> Result<Expression, anyhow::Error>
{
	let mut expression = parse_unary_expression(tokens)?;

	loop
	{
		let op = match tokens.front()
		{
			Some(Token::Plus) => BinaryOp::Add,
			Some(Token::Minus) => BinaryOp::Subtract,
			_ =>
			{
				return Ok(expression);
			}
		};
		tokens.pop_front();

		let right = parse_unary_expression(tokens)?;

		expression = Expression::Binary {
			op,
			left: Box::new(expression),
			right: Box::new(right),
		};
	}
}

fn parse_unary_expression(
	tokens: &mut VecDeque<Token>,
) -> Result<Expression, anyhow::Error>
{
	parse_primary_expression(tokens)
}

fn parse_primary_expression(
	tokens: &mut VecDeque<Token>,
) -> Result<Expression, anyhow::Error>
{
	match tokens.front()
	{
		Some(Token::Int32(literal)) =>
		{
			let value: i32 = *literal;
			tokens.pop_front();
			Ok(Expression::Literal(Literal::Int32(value)))
		}
		_ =>
		{
			return Ok(Expression::Void);
		}
	}
}
