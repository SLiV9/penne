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
			return Err(anyhow!("unexpected end of tokens, left paren"));
		}
	}

	match tokens.pop_front()
	{
		Some(Token::ParenRight) => (),
		Some(token) =>
		{
			return Err(anyhow!(
				"unexpected '{:?}', expected right paren",
				token
			));
		}
		None =>
		{
			return Err(anyhow!(
				"unexpected end of tokens, expected right paren"
			));
		}
	}

	let body = parse_function_body(tokens)?;

	let function = Declaration::Function { name, body };

	Ok(function)
}

fn parse_function_body(
	tokens: &mut VecDeque<Token>,
) -> Result<FunctionBody, anyhow::Error>
{
	match tokens.pop_front()
	{
		Some(Token::BraceLeft) => (),
		Some(token) =>
		{
			return Err(anyhow!(
				"unexpected '{:?}', expected left brace",
				token
			));
		}
		None =>
		{
			return Err(anyhow!(
				"unexpected end of tokens, expected left brace"
			));
		}
	}

	let mut statements = Vec::new();

	loop
	{
		if let Some(Token::BraceRight) = tokens.front()
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

	if let Some(Token::BraceRight) = tokens.front()
	{
		tokens.pop_front();
		let body = FunctionBody {
			statements,
			return_value: Some(return_value),
		};
		return Ok(body);
	}
	else
	{
		return Err(anyhow!(
			"unexpected '{:?}', expected closing brace",
			tokens.front()
		));
	}
}

fn parse_block(tokens: &mut VecDeque<Token>) -> Result<Block, anyhow::Error>
{
	match tokens.pop_front()
	{
		Some(Token::BraceLeft) => (),
		Some(token) =>
		{
			return Err(anyhow!(
				"unexpected '{:?}', expected left brace",
				token
			));
		}
		None =>
		{
			return Err(anyhow!(
				"unexpected end of tokens, expected left brace"
			));
		}
	}

	let mut statements = Vec::new();

	loop
	{
		if let Some(Token::BraceRight) = tokens.front()
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
	tokens: &mut VecDeque<Token>,
) -> Result<Statement, anyhow::Error>
{
	match tokens.pop_front()
	{
		Some(Token::BraceLeft) =>
		{
			// Oops.
			// TODO avoid pushing to the front of the VecDeque
			tokens.push_front(Token::BraceLeft);

			let block = parse_block(tokens)?;
			let statement = Statement::Block(block);
			Ok(statement)
		}
		Some(Token::If) =>
		{
			let condition = parse_comparison(tokens)?;
			let then_stmt = parse_statement(tokens)?;
			let then_branch = Box::new(then_stmt);

			match tokens.front()
			{
				Some(Token::Else) =>
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
				_ =>
				{
					let statement = Statement::If {
						condition,
						then_branch,
						else_branch: None,
					};
					Ok(statement)
				}
			}
		}
		Some(Token::Loop) => match tokens.pop_front()
		{
			Some(Token::Semicolon) => Ok(Statement::Loop),
			Some(token) =>
			{
				Err(anyhow!("unexpected '{:?}', expected semicolon", token))
			}
			None => Err(anyhow!("unexpected end of tokens")),
		},
		Some(Token::Goto) =>
		{
			let label = match tokens.pop_front()
			{
				Some(Token::Identifier(x)) => x,
				Some(token) =>
				{
					return Err(anyhow!(
						"unexpected '{:?}', expected identifier",
						token
					))
				}
				None => return Err(anyhow!("unexpected end of tokens")),
			};

			match tokens.pop_front()
			{
				Some(Token::Semicolon) =>
				{
					let statement = Statement::Goto { label };
					Ok(statement)
				}
				Some(token) =>
				{
					Err(anyhow!("unexpected '{:?}', expected semicolon", token))
				}
				None => Err(anyhow!("unexpected end of tokens")),
			}
		}
		Some(Token::Var) =>
		{
			let name = match tokens.pop_front()
			{
				Some(Token::Identifier(x)) => x,
				Some(token) =>
				{
					return Err(anyhow!(
						"unexpected '{:?}', expected identifier",
						token
					))
				}
				None => return Err(anyhow!("unexpected end of tokens")),
			};

			match tokens.pop_front()
			{
				Some(Token::Assignment) =>
				{
					let expression = parse_expression(tokens)?;

					match tokens.pop_front()
					{
						Some(Token::Semicolon) =>
						{
							let statement = Statement::Declaration {
								name,
								value: Some(expression),
							};
							Ok(statement)
						}
						Some(token) => Err(anyhow!(
							"unexpected '{:?}', expected semicolon",
							token
						)),
						None => Err(anyhow!("unexpected end of tokens")),
					}
				}
				Some(Token::Semicolon) =>
				{
					let statement =
						Statement::Declaration { name, value: None };
					Ok(statement)
				}
				Some(token) => Err(anyhow!(
					"unexpected '{:?}', expected semicolon or assignment",
					token
				)),
				None => Err(anyhow!("unexpected end of tokens")),
			}
		}
		Some(Token::Identifier(x)) => match tokens.pop_front()
		{
			Some(Token::Assignment) =>
			{
				let expression = parse_expression(tokens)?;

				match tokens.pop_front()
				{
					Some(Token::Semicolon) =>
					{
						let statement = Statement::Assignment {
							name: x,
							value: expression,
						};
						Ok(statement)
					}
					Some(token) => Err(anyhow!(
						"unexpected '{:?}', expected semicolon",
						token
					)),
					None => Err(anyhow!("unexpected end of tokens")),
				}
			}
			Some(Token::Colon) =>
			{
				let statement = Statement::Label { label: x };
				Ok(statement)
			}
			Some(token) => Err(anyhow!(
				"unexpected '{:?}' after identifier in statement",
				token
			)),
			None => Err(anyhow!("unexpected end of tokens")),
		},
		Some(other) =>
		{
			Err(anyhow!("unexpected '{:?}', expected statement", other))
		}
		None => Err(anyhow!("unexpected end of tokens, expected statement")),
	}
}

fn parse_comparison(
	tokens: &mut VecDeque<Token>,
) -> Result<Comparison, anyhow::Error>
{
	let left = parse_expression(tokens)?;

	loop
	{
		let op = match tokens.front()
		{
			Some(Token::Equals) => ComparisonOp::Equals,
			_ =>
			{
				return Err(anyhow!("expected comparison"));
			}
		};
		tokens.pop_front();

		let right = parse_expression(tokens)?;

		return Ok(Comparison { op, left, right });
	}
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
	match tokens.pop_front()
	{
		Some(Token::Int32(literal)) =>
		{
			let value: i32 = literal;
			Ok(Expression::Literal(Literal::Int32(value)))
		}
		Some(Token::Bool(literal)) =>
		{
			let value: bool = literal;
			Ok(Expression::Literal(Literal::Bool(value)))
		}
		Some(Token::Identifier(name)) => Ok(Expression::Variable(name)),
		Some(other) => Err(anyhow!("unexpected '{:?}' in expression", other)),
		None => Err(anyhow!("unexpected end of tokens in expression")),
	}
}
