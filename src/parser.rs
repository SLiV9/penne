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

#[derive(Debug, PartialEq, Eq)]
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
				if let Some(Token::BraceRight) = tokens.front()
				{
					tokens.pop_front();
					let block = Block {
						statements,
						value: expression,
					};
					return Ok(block);
				}
				else
				{
					return Err(anyhow!(
						"unexpected '{:?}', expected closing brace",
						tokens.front()
					));
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
	match tokens.pop_front()
	{
		Some(Token::BraceLeft) =>
		{
			// Oops.
			// TODO avoid pushing to the front of the VecDeque
			tokens.push_front(Token::BraceLeft);

			let block = parse_block(tokens)?;
			let statement = Statement::Block(block);
			Ok(StatementOrFinalExpression::Statement(statement))
		}
		Some(Token::If) =>
		{
			let condition = parse_comparison(tokens)?;
			let statement = parse_statement_or_final_expression(tokens)?;
			let then_stmt = match statement
			{
				StatementOrFinalExpression::Statement(statement) => statement,
				StatementOrFinalExpression::FinalExpression(expression) =>
				{
					return Err(anyhow!(
						"unexpected expression '{:?}'",
						expression
					));
				}
			};
			let then_branch = Box::new(then_stmt);

			match tokens.front()
			{
				Some(Token::Else) =>
				{
					tokens.pop_front();

					let statement =
						parse_statement_or_final_expression(tokens)?;
					let else_stmt = match statement
					{
						StatementOrFinalExpression::Statement(statement) =>
						{
							statement
						}
						StatementOrFinalExpression::FinalExpression(
							expression,
						) =>
						{
							return Err(anyhow!(
								"unexpected expression '{:?}'",
								expression
							));
						}
					};
					let else_branch = Some(Box::new(else_stmt));
					let statement = Statement::If {
						condition,
						then_branch,
						else_branch,
					};
					Ok(StatementOrFinalExpression::Statement(statement))
				}
				_ =>
				{
					let statement = Statement::If {
						condition,
						then_branch,
						else_branch: None,
					};
					Ok(StatementOrFinalExpression::Statement(statement))
				}
			}
		}
		Some(Token::Loop) => match tokens.pop_front()
		{
			Some(Token::Semicolon) =>
			{
				Ok(StatementOrFinalExpression::Statement(Statement::Loop))
			}
			Some(token) =>
			{
				return Err(anyhow!(
					"unexpected '{:?}', expected semicolon",
					token
				));
			}
			None =>
			{
				return Err(anyhow!("unexpected end of tokens"));
			}
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
					));
				}
				None =>
				{
					return Err(anyhow!("unexpected end of tokens"));
				}
			};

			match tokens.pop_front()
			{
				Some(Token::Semicolon) =>
				{
					let statement = Statement::Goto { label };
					Ok(StatementOrFinalExpression::Statement(statement))
				}
				Some(token) =>
				{
					return Err(anyhow!(
						"unexpected '{:?}', expected semicolon",
						token
					));
				}
				None =>
				{
					return Err(anyhow!("unexpected end of tokens"));
				}
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
					));
				}
				None =>
				{
					return Err(anyhow!("unexpected end of tokens"));
				}
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
							Ok(StatementOrFinalExpression::Statement(statement))
						}
						Some(token) =>
						{
							return Err(anyhow!(
								"unexpected '{:?}', expected semicolon",
								token
							));
						}
						None =>
						{
							return Err(anyhow!("unexpected end of tokens"));
						}
					}
				}
				Some(Token::Semicolon) =>
				{
					let statement =
						Statement::Declaration { name, value: None };
					Ok(StatementOrFinalExpression::Statement(statement))
				}
				Some(token) =>
				{
					return Err(anyhow!(
						"unexpected '{:?}', expected semicolon or assignment",
						token
					));
				}
				None =>
				{
					return Err(anyhow!("unexpected end of tokens"));
				}
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
						Ok(StatementOrFinalExpression::Statement(statement))
					}
					Some(token) =>
					{
						return Err(anyhow!(
							"unexpected '{:?}', expected semicolon",
							token
						));
					}
					None =>
					{
						return Err(anyhow!("unexpected end of tokens"));
					}
				}
			}
			Some(Token::Colon) =>
			{
				let statement = Statement::Label { label: x };
				Ok(StatementOrFinalExpression::Statement(statement))
			}
			Some(token) =>
			{
				// Oops.
				// TODO avoid pushing to the front of the VecDeque
				tokens.push_front(token);
				tokens.push_front(Token::Identifier(x));

				let expression = parse_expression(tokens)?;

				Ok(StatementOrFinalExpression::FinalExpression(expression))
			}
			None =>
			{
				return Err(anyhow!("unexpected end of tokens"));
			}
		},
		Some(other) =>
		{
			// Oops.
			// TODO avoid pushing to the front of the VecDeque
			tokens.push_front(other);

			let expression = parse_expression(tokens)?;

			Ok(StatementOrFinalExpression::FinalExpression(expression))
		}
		None =>
		{
			let expression = parse_expression(tokens)?;

			Ok(StatementOrFinalExpression::FinalExpression(expression))
		}
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
		Some(other) =>
		{
			// Oops.
			// TODO avoid pushing to the front of the VecDeque
			tokens.push_front(other);
			return Ok(Expression::Void);
		}
		None =>
		{
			return Ok(Expression::Void);
		}
	}
}
