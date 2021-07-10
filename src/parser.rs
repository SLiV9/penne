/**/

use crate::common::*;
use crate::lexer::{LexedToken, Token};

use std::collections::VecDeque;

use anyhow::anyhow;
use anyhow::Context;

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
) -> Result<Identifier, anyhow::Error>
{
	match tokens.pop_front()
	{
		Some(LexedToken {
			result: Ok(Token::Identifier(name)),
			location,
		}) => Ok(Identifier { name, location }),
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

	let mut parameters = Vec::new();
	loop
	{
		if let Some(Token::ParenRight) = peek(tokens)
		{
			break;
		}

		let parameter = parse_parameter(tokens)?;
		parameters.push(parameter);

		if let Some(Token::Comma) = peek(tokens)
		{
			tokens.pop_front();
		}
		else
		{
			break;
		}
	}

	consume(Token::ParenRight, tokens).context("expected right parenthesis")?;

	let return_type = if let Some(Token::Arrow) = peek(tokens)
	{
		tokens.pop_front();

		let value_type = parse_type(tokens)?;
		Some(value_type)
	}
	else
	{
		None
	};

	let body = parse_function_body(tokens)?;

	let function = Declaration::Function {
		name,
		parameters,
		body,
		return_type,
	};

	Ok(function)
}

fn parse_parameter(
	tokens: &mut VecDeque<LexedToken>,
) -> Result<Parameter, anyhow::Error>
{
	let name = extract_identifier(tokens).context("expected parameter name")?;

	let value_type = if let Some(Token::Colon) = peek(tokens)
	{
		tokens.pop_front();

		let value_type = parse_type(tokens)?;
		Some(value_type)
	}
	else
	{
		None
	};

	Ok(Parameter {
		name,
		value_type,
		is_mutable: false,
	})
}

fn parse_type(
	tokens: &mut VecDeque<LexedToken>,
) -> Result<ValueType, anyhow::Error>
{
	let (token, location) = extract(tokens).context("expected type keyword")?;
	match token
	{
		Token::Type(value_type) => Ok(value_type),
		token => Err(anyhow!("got {:?}", token))
			.context(location.format())
			.context("expected type keyword"),
	}
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
			Statement::Label { label, .. } => label.name == "return",
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
			let (_, location) = extract(tokens).unwrap();

			let block = Block {
				statements,
				location,
			};
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
					location,
				};
				Ok(statement)
			}
			else
			{
				let statement = Statement::If {
					condition,
					then_branch,
					else_branch: None,
					location,
				};
				Ok(statement)
			}
		}
		Token::Loop =>
		{
			consume(Token::Semicolon, tokens).context("expected semicolon")?;
			Ok(Statement::Loop { location })
		}
		Token::Goto =>
		{
			let label = extract_identifier(tokens).context("expected label")?;
			consume(Token::Semicolon, tokens).context("expected semicolon")?;
			let statement = Statement::Goto { label, location };
			Ok(statement)
		}
		Token::Var =>
		{
			let name =
				extract_identifier(tokens).context("expected variable name")?;

			let value_type = if let Some(Token::Colon) = peek(tokens)
			{
				tokens.pop_front();
				let value_type = parse_type(tokens)?;
				Some(value_type)
			}
			else
			{
				None
			};

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

			let statement = Statement::Declaration {
				name,
				value,
				value_type,
				location,
			};
			Ok(statement)
		}
		Token::Identifier(x) =>
		{
			if let Some(Token::Colon) = peek(tokens)
			{
				tokens.pop_front();
				let statement = Statement::Label {
					label: Identifier {
						name: x,
						location: location.clone(),
					},
					location,
				};
				return Ok(statement);
			}

			consume(Token::Assignment, tokens)
				.context("expected assignment")?;
			let expression = parse_expression(tokens)?;
			consume(Token::Semicolon, tokens).context("expected semicolon")?;

			let statement = Statement::Assignment {
				name: Identifier {
					name: x,
					location: location.clone(),
				},
				value: expression,
				location,
			};
			Ok(statement)
		}
		other => Err(anyhow!("got {:?}", other))
			.context(location.format())
			.context("expected keyword, identifier or opening brace"),
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

	Ok(Comparison {
		op,
		left,
		right,
		location,
	})
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
		let (_, location) = extract(tokens).unwrap();

		let right = parse_unary_expression(tokens)?;

		expression = Expression::Binary {
			op,
			left: Box::new(expression),
			right: Box::new(right),
			location,
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
		Token::StringLiteral(mut value) =>
		{
			while let Some(next_token) = peek(tokens)
			{
				match next_token
				{
					Token::StringLiteral(extra_value) =>
					{
						value = value + extra_value;
						let _ = extract(tokens)?;
					}
					_ => break,
				}
			}
			Ok(Expression::Literal(Literal::StringLiteral(value)))
		}
		Token::Identifier(name) =>
		{
			let identifier = Identifier { name, location };
			if let Some(Token::ParenLeft) = peek(tokens)
			{
				let arguments = parse_arguments(tokens)?;
				Ok(Expression::FunctionCall {
					name: identifier,
					arguments,
					return_type: None,
				})
			}
			else
			{
				Ok(Expression::Variable {
					name: identifier,
					value_type: None,
				})
			}
		}
		other => Err(anyhow!("got {:?}", other))
			.context(location.format())
			.context("expected literal or identifier"),
	}
}

fn parse_arguments(
	tokens: &mut VecDeque<LexedToken>,
) -> Result<Vec<Expression>, anyhow::Error>
{
	consume(Token::ParenLeft, tokens).context("expected argument list")?;

	if let Some(Token::ParenRight) = peek(tokens)
	{
		let _ = extract(tokens);
		return Ok(Vec::new());
	}

	let mut arguments = Vec::new();

	loop
	{
		let expression = parse_expression(tokens)?;
		arguments.push(expression);

		if let Some(Token::ParenRight) = peek(tokens)
		{
			let _ = extract(tokens);
			return Ok(arguments);
		}

		consume(Token::Comma, tokens)
			.context("expected comma or right parenthesis")?;
	}
}
