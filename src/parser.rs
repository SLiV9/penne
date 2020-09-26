/**/

use crate::lexer::{LexedToken, Location, Token};

use std::collections::VecDeque;

use anyhow::anyhow;
use anyhow::Context;

#[derive(Debug, PartialEq, Eq)]
pub enum Declaration
{
	Function
	{
		name: String,
		//parameters: Vec<Parameter>,
		body: FunctionBody,
	},
}

#[derive(Debug, PartialEq, Eq)]
pub struct FunctionBody
{
	pub statements: Vec<Statement>,
	pub return_value: Option<Expression>,
}

#[derive(Debug, PartialEq, Eq)]
pub struct Block
{
	pub statements: Vec<Statement>,
}

#[derive(Debug, PartialEq, Eq)]
pub enum Statement
{
	Declaration
	{
		name: String,
		value: Option<Expression>,
	},
	Assignment
	{
		name: String,
		value: Expression,
	},
	Loop,
	Goto
	{
		label: String,
	},
	Label
	{
		label: String,
	},
	If
	{
		condition: Comparison,
		then_branch: Box<Statement>,
		else_branch: Option<Box<Statement>>,
	},
	Block(Block),
}

#[derive(Debug, PartialEq, Eq)]
pub struct Comparison
{
	pub op: ComparisonOp,
	pub left: Expression,
	pub right: Expression,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ComparisonOp
{
	Equals,
}

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
	Variable(String),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BinaryOp
{
	Add,
	Subtract,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Literal
{
	Int32(i32),
	Bool(bool),
}

pub fn parse(tokens: Vec<LexedToken>)
	-> Result<Vec<Declaration>, anyhow::Error>
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

fn peek(tokens: &mut VecDeque<LexedToken>) -> Option<&Token>
{
	match tokens.front()
	{
		Some(LexedToken {
			result: Ok(token),
			location: _,
		}) => Some(token),
		Some(LexedToken {
			result: Err(_),
			location: _,
		}) => None,
		None => None,
	}
}

fn consume(
	expected_token: Token,
	tokens: &mut VecDeque<LexedToken>,
) -> Result<(), anyhow::Error>
{
	match tokens.pop_front()
	{
		Some(LexedToken {
			result: Ok(token),
			location: _,
		}) if token == expected_token => Ok(()),
		Some(LexedToken {
			result: Ok(token),
			location,
		}) => Err(anyhow!("got {:?}", token)).context(location.format()),
		Some(LexedToken {
			result: Err(error),
			location,
		}) => Err(error).context(location.format()),
		None => Err(anyhow!("unexpected end of file")),
	}
}

fn extract_identifier(
	tokens: &mut VecDeque<LexedToken>,
) -> Result<String, anyhow::Error>
{
	match tokens.pop_front()
	{
		Some(LexedToken {
			result: Ok(Token::Identifier(name)),
			location: _,
		}) => Ok(name),
		Some(LexedToken {
			result: Ok(token),
			location,
		}) => Err(anyhow!("got {:?}", token)).context(location.format()),
		Some(LexedToken {
			result: Err(error),
			location,
		}) => Err(error).context(location.format()),
		None => Err(anyhow!("unexpected end of file")),
	}
}

fn extract(
	tokens: &mut VecDeque<LexedToken>,
) -> Result<(Token, Location), anyhow::Error>
{
	match tokens.pop_front()
	{
		Some(LexedToken {
			result: Ok(token),
			location,
		}) => Ok((token, location)),
		Some(LexedToken {
			result: Err(error),
			location,
		}) => Err(error).context(location.format()),
		None => Err(anyhow!("unexpected end of file")),
	}
}

fn parse_declaration(
	tokens: &mut VecDeque<LexedToken>,
) -> Result<Declaration, anyhow::Error>
{
	consume(Token::Fn, tokens).context("expected top-level declaration")?;

	let name = extract_identifier(tokens).context("expected function name")?;

	consume(Token::ParenLeft, tokens).context("expected left parenthesis")?;
	consume(Token::ParenRight, tokens).context("expected right parenthesis")?;

	let body = parse_function_body(tokens)?;

	let function = Declaration::Function { name, body };

	Ok(function)
}

fn parse_function_body(
	tokens: &mut VecDeque<LexedToken>,
) -> Result<FunctionBody, anyhow::Error>
{
	consume(Token::BraceLeft, tokens).context("expected function body")?;

	let mut statements = Vec::new();

	loop
	{
		if let Some(Token::BraceRight) = peek(tokens)
		{
			tokens.pop_front();

			let body = FunctionBody {
				statements,
				return_value: None,
			};
			return Ok(body);
		}

		let statement = parse_statement(tokens)?;

		let is_return = match &statement
		{
			Statement::Label { label } => label == "return",
			_ => false,
		};
		statements.push(statement);
		if is_return
		{
			break;
		}
	}

	let return_value = parse_expression(tokens)?;

	consume(Token::BraceRight, tokens).context("expected closing brace")?;

	let body = FunctionBody {
		statements,
		return_value: Some(return_value),
	};
	Ok(body)
}

#[allow(dead_code)]
fn parse_block(
	tokens: &mut VecDeque<LexedToken>,
) -> Result<Block, anyhow::Error>
{
	consume(Token::BraceLeft, tokens).context("expected block")?;

	parse_rest_of_block(tokens)
}

fn parse_rest_of_block(
	tokens: &mut VecDeque<LexedToken>,
) -> Result<Block, anyhow::Error>
{
	let mut statements = Vec::new();

	loop
	{
		if let Some(Token::BraceRight) = peek(tokens)
		{
			tokens.pop_front();

			let block = Block { statements };
			return Ok(block);
		}

		let statement = parse_statement(tokens)?;
		statements.push(statement);
	}
}

fn parse_statement(
	tokens: &mut VecDeque<LexedToken>,
) -> Result<Statement, anyhow::Error>
{
	let (token, location) = extract(tokens).context("expected statement")?;

	match token
	{
		Token::BraceLeft =>
		{
			let block = parse_rest_of_block(tokens)?;
			let statement = Statement::Block(block);
			Ok(statement)
		}
		Token::If =>
		{
			let condition = parse_comparison(tokens)?;
			let then_stmt = parse_statement(tokens)?;
			let then_branch = Box::new(then_stmt);

			if let Some(Token::Else) = peek(tokens)
			{
				tokens.pop_front();

				let else_stmt = parse_statement(tokens)?;
				let else_branch = Some(Box::new(else_stmt));
				let statement = Statement::If {
					condition,
					then_branch,
					else_branch,
				};
				Ok(statement)
			}
			else
			{
				let statement = Statement::If {
					condition,
					then_branch,
					else_branch: None,
				};
				Ok(statement)
			}
		}
		Token::Loop =>
		{
			consume(Token::Semicolon, tokens).context("expected semicolon")?;
			Ok(Statement::Loop)
		}
		Token::Goto =>
		{
			let label = extract_identifier(tokens).context("expected label")?;
			consume(Token::Semicolon, tokens).context("expected semicolon")?;
			let statement = Statement::Goto { label };
			Ok(statement)
		}
		Token::Var =>
		{
			let name =
				extract_identifier(tokens).context("expected variable name")?;

			let value = if let Some(Token::Assignment) = peek(tokens)
			{
				tokens.pop_front();
				let expression = parse_expression(tokens)?;
				Some(expression)
			}
			else
			{
				None
			};

			consume(Token::Semicolon, tokens).context("expected semicolon")?;

			let statement = Statement::Declaration { name, value };
			Ok(statement)
		}
		Token::Identifier(x) =>
		{
			if let Some(Token::Colon) = peek(tokens)
			{
				tokens.pop_front();
				let statement = Statement::Label { label: x };
				return Ok(statement);
			}

			consume(Token::Assignment, tokens)
				.context("expected assignment")?;
			let expression = parse_expression(tokens)?;
			consume(Token::Semicolon, tokens).context("expected semicolon")?;

			let statement = Statement::Assignment {
				name: x,
				value: expression,
			};
			Ok(statement)
		}
		other => Err(anyhow!("got {:?}", other))
			.context(location.format())
			.context("expected literal or identifier"),
	}
}

fn parse_comparison(
	tokens: &mut VecDeque<LexedToken>,
) -> Result<Comparison, anyhow::Error>
{
	let left = parse_expression(tokens)?;

	let (token, location) =
		extract(tokens).context("expected comparison operator")?;
	let op = match token
	{
		Token::Equals => ComparisonOp::Equals,
		token =>
		{
			return Err(anyhow!("got {:?}", token))
				.context(location.format())
				.context("expected comparison operator")
		}
	};

	let right = parse_expression(tokens)?;

	Ok(Comparison { op, left, right })
}

fn parse_expression(
	tokens: &mut VecDeque<LexedToken>,
) -> Result<Expression, anyhow::Error>
{
	parse_addition(tokens)
}

fn parse_addition(
	tokens: &mut VecDeque<LexedToken>,
) -> Result<Expression, anyhow::Error>
{
	let mut expression = parse_unary_expression(tokens)?;

	loop
	{
		let op = match peek(tokens)
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
	tokens: &mut VecDeque<LexedToken>,
) -> Result<Expression, anyhow::Error>
{
	parse_primary_expression(tokens)
}

fn parse_primary_expression(
	tokens: &mut VecDeque<LexedToken>,
) -> Result<Expression, anyhow::Error>
{
	let (token, location) =
		extract(tokens).context("expected literal or identifier")?;
	match token
	{
		Token::Int32(value) => Ok(Expression::Literal(Literal::Int32(value))),
		Token::Bool(value) => Ok(Expression::Literal(Literal::Bool(value))),
		Token::Identifier(name) => Ok(Expression::Variable(name)),
		other => Err(anyhow!("got {:?}", other))
			.context(location.format())
			.context("expected literal or identifier"),
	}
}
