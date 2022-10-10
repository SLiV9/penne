//
// Part of penne
// Copyright (c) 2020 Sander in 't Veld
// License: MIT
//

use crate::common::*;
use crate::error::Error;
use crate::lexer::{LexedToken, Token};

use std::collections::VecDeque;

use enumset::EnumSet;

pub const MAX_ADDRESS_DEPTH: u8 = 127;
pub const MAX_REFERENCE_DEPTH: usize = 127;

pub fn parse(tokens: Vec<LexedToken>) -> Vec<Declaration>
{
	let mut declarations = Vec::new();

	let mut tokens = Tokens::from(tokens);
	while !tokens.is_empty()
	{
		match parse_declaration(&mut tokens)
		{
			Ok(declaration) =>
			{
				declarations.push(declaration);
			}
			Err(error) =>
			{
				let poison = Poison::Error {
					error,
					partial: None,
				};
				let declaration = Declaration::Poison(poison);
				declarations.push(declaration);
				skip_until_next_declaration(&mut tokens);
			}
		}
	}

	declarations
}

struct Tokens
{
	tokens: VecDeque<LexedToken>,
	last_location: Location,
}

impl From<Vec<LexedToken>> for Tokens
{
	fn from(tokens: Vec<LexedToken>) -> Tokens
	{
		let last_location = match tokens.iter().next()
		{
			Some(LexedToken {
				result: _,
				location,
			}) => location.clone(),
			None => unreachable!(),
		};
		Tokens {
			tokens: VecDeque::from(tokens),
			last_location,
		}
	}
}

impl Tokens
{
	fn is_empty(&self) -> bool
	{
		self.tokens.is_empty()
	}

	fn pop_front(&mut self) -> Option<LexedToken>
	{
		let popped = self.tokens.pop_front();
		match &popped
		{
			Some(LexedToken {
				result: _,
				location,
			}) =>
			{
				self.last_location = location.clone();
			}
			None => (),
		}
		popped
	}

	fn get_next_location(&self) -> Option<Location>
	{
		match self.tokens.front()
		{
			Some(LexedToken {
				result: _,
				location,
			}) => Some(location.clone()),
			None => None,
		}
	}
}

fn peek(tokens: &mut Tokens) -> Option<&Token>
{
	match tokens.tokens.front()
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
	expectation: &str,
	tokens: &mut Tokens,
) -> Result<(), Error>
{
	match tokens.pop_front()
	{
		Some(LexedToken {
			result: Ok(token),
			location: _,
		}) if token == expected_token => Ok(()),
		Some(LexedToken {
			result: Ok(_),
			location,
		}) => Err(Error::UnexpectedToken {
			expectation: expectation.to_string(),
			location,
		}),
		Some(LexedToken {
			result: Err(error),
			location,
		}) => Err(Error::Lexical {
			error,
			expectation: expectation.to_string(),
			location,
		}),
		None => Err(Error::UnexpectedEndOfFile {
			expectation: expectation.to_string(),
			last_location: tokens.last_location.clone(),
		}),
	}
}

fn extract_identifier(
	expectation: &str,
	tokens: &mut Tokens,
) -> Result<Identifier, Error>
{
	match tokens.pop_front()
	{
		Some(LexedToken {
			result: Ok(Token::Identifier(name)),
			location,
		}) => Ok(Identifier {
			name,
			location,
			resolution_id: 0,
		}),
		Some(LexedToken {
			result: Ok(_),
			location,
		}) => Err(Error::UnexpectedToken {
			expectation: expectation.to_string(),
			location,
		}),
		Some(LexedToken {
			result: Err(error),
			location,
		}) => Err(Error::Lexical {
			error,
			expectation: expectation.to_string(),
			location,
		}),
		None => Err(Error::UnexpectedEndOfFile {
			expectation: expectation.to_string(),
			last_location: tokens.last_location.clone(),
		}),
	}
}

fn extract(
	expectation: &str,
	tokens: &mut Tokens,
) -> Result<(Token, Location), Error>
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
		}) => Err(Error::Lexical {
			error,
			expectation: expectation.to_string(),
			location,
		}),
		None => Err(Error::UnexpectedEndOfFile {
			expectation: expectation.to_string(),
			last_location: tokens.last_location.clone(),
		}),
	}
}

fn can_start_declaration(token: &Token) -> bool
{
	match token
	{
		Token::Pub => true,
		Token::Extern => true,
		Token::Const => true,
		Token::Fn => true,
		Token::DebugDollar => true,
		_ => false,
	}
}

fn skip_until_next_declaration(tokens: &mut Tokens)
{
	while let Some(token) = peek(tokens)
	{
		if can_start_declaration(token)
		{
			return;
		}
		tokens.pop_front();
	}
}

fn parse_declaration(tokens: &mut Tokens) -> Result<Declaration, Error>
{
	let mut flags = EnumSet::new();
	let mut location_of_declaration = None;
	if let Some(Token::Pub) = peek(tokens)
	{
		tokens.pop_front();
		location_of_declaration = Some(tokens.last_location.clone());
		flags.insert(DeclarationFlag::Public);
	}
	if let Some(Token::Extern) = peek(tokens)
	{
		tokens.pop_front();
		location_of_declaration = Some(tokens.last_location.clone());
		flags.insert(DeclarationFlag::External);
	}

	let (token, location) = extract("expected top-level declaration", tokens)?;
	let location_of_declaration =
		location_of_declaration.unwrap_or_else(|| location.clone());
	match token
	{
		Token::Const =>
		{
			parse_constant_declaration(flags, location_of_declaration, tokens)
		}
		Token::Fn =>
		{
			parse_function_declaration(flags, location_of_declaration, tokens)
		}
		Token::DebugDollar =>
		{
			let (token, location) = extract("expected path", tokens)?;
			match token
			{
				Token::StringLiteral { bytes, value_type } => match value_type
				{
					None | Some(ValueType::String) =>
					{
						let directive =
							String::from_utf8_lossy(&bytes).to_string();
						Ok(Declaration::PreprocessorDirective {
							directive,
							location,
						})
					}
					Some(_) => Err(Error::UnexpectedToken {
						location,
						expectation: "expected UTF-8 encoded include path"
							.to_string(),
					}),
				},
				_ => Err(Error::UnexpectedToken {
					location,
					expectation: "expected include path".to_string(),
				}),
			}
		}
		_ => Err(Error::UnexpectedToken {
			location,
			expectation: "expected top-level declaration".to_string(),
		}),
	}
}

fn parse_constant_declaration(
	flags: EnumSet<DeclarationFlag>,
	location_of_declaration: Location,
	tokens: &mut Tokens,
) -> Result<Declaration, Error>
{
	let name = extract_identifier("expected constant name", tokens)?;
	let location_of_declaration =
		location_of_declaration.combined_with(&tokens.last_location);
	let value_type =
		parse_colon_and_type(&flags, &location_of_declaration, tokens);

	let expression = match parse_assignment_and_expression(tokens)
	{
		Ok(expression) => expression,
		Err(Poison::Error { error, partial }) =>
		{
			Expression::Poison(Poison::Error {
				error,
				partial: partial.map(|x| Box::new(x)),
			})
		}
		Err(Poison::Poisoned) => Expression::Poison(Poison::Poisoned),
	};

	consume(Token::Semicolon, "expected semicolon", tokens)?;

	let declaration = Declaration::Constant {
		name,
		value: expression,
		value_type,
		flags,
	};
	Ok(declaration)
}

fn parse_assignment_and_expression(
	tokens: &mut Tokens,
) -> Poisonable<Expression>
{
	consume(Token::Assignment, "expected assignment", tokens)?;
	let expression = parse_expression(tokens)?;
	Ok(expression)
}

fn parse_function_declaration(
	flags: EnumSet<DeclarationFlag>,
	location_of_declaration: Location,
	tokens: &mut Tokens,
) -> Result<Declaration, Error>
{
	let name = extract_identifier("expected function name", tokens)?;
	consume(Token::ParenLeft, "expected left parenthesis", tokens)?;
	let (parameters, return_type, recoverable_error) =
		parse_rest_of_function_signature(
			&flags,
			location_of_declaration,
			tokens,
		);
	if recoverable_error.is_some()
	{
		skip_rest_of_function_signature(tokens);
	}

	let declaration = if peek(tokens) == Some(&Token::Semicolon)
	{
		tokens.pop_front();
		Declaration::FunctionHead {
			name,
			parameters,
			return_type,
			flags,
		}
	}
	else
	{
		let body = match parse_function_body(&name.name, tokens)
		{
			Ok(body) => Ok(body),
			Err(poison) =>
			{
				skip_until_next_declaration(tokens);
				Err(poison)
			}
		};
		Declaration::Function {
			name,
			parameters,
			body,
			return_type,
			flags,
		}
	};
	match recoverable_error
	{
		None => Ok(declaration),
		Some(error) => Ok(Declaration::Poison(Poison::Error {
			error,
			partial: Some(Box::new(declaration)),
		})),
	}
}

fn parse_rest_of_function_signature(
	flags: &EnumSet<DeclarationFlag>,
	location_of_declaration: Location,
	tokens: &mut Tokens,
) -> (Vec<Parameter>, Option<ValueType>, Option<Error>)
{
	let mut recoverable_error = None;

	let mut parameters = Vec::new();
	loop
	{
		if let Some(Token::ParenRight) = peek(tokens)
		{
			break;
		}

		match parse_parameter(flags, &location_of_declaration, tokens)
		{
			Ok(parameter) =>
			{
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
			Err(error) =>
			{
				skip_rest_of_parameters(tokens);
				recoverable_error = Some(error);
				break;
			}
		}
	}

	match consume(Token::ParenRight, "expected right parenthesis", tokens)
	{
		Ok(()) => (),
		Err(error) =>
		{
			recoverable_error.get_or_insert(error);
		}
	}

	let return_type = if let Some(Token::Arrow) = peek(tokens)
	{
		tokens.pop_front();

		match parse_type(tokens)
		{
			Ok(value_type) => Some(value_type),
			Err(error) =>
			{
				recoverable_error.get_or_insert(error);
				None
			}
		}
	}
	else
	{
		None
	};

	(parameters, return_type, recoverable_error)
}

fn skip_rest_of_function_signature(tokens: &mut Tokens)
{
	while let Some(token) = peek(tokens)
	{
		match token
		{
			token if can_start_declaration(token) => return,
			Token::BraceLeft => return,
			Token::BraceRight => return,
			Token::Semicolon => return,
			_ => (),
		}
		tokens.pop_front();
	}
}

fn parse_parameter(
	flags: &EnumSet<DeclarationFlag>,
	location_of_declaration: &Location,
	tokens: &mut Tokens,
) -> Result<Parameter, Error>
{
	let name = extract_identifier("expected parameter name", tokens)?;
	let value_type =
		parse_colon_and_type(flags, location_of_declaration, tokens);

	Ok(Parameter {
		name: Ok(name),
		value_type,
	})
}

fn skip_rest_of_parameters(tokens: &mut Tokens)
{
	while let Some(token) = peek(tokens)
	{
		match token
		{
			Token::ParenRight => return,
			token if can_start_declaration(token) => return,
			Token::BraceLeft => return,
			Token::BraceRight => return,
			Token::Arrow => return,
			Token::Semicolon => return,
			_ => (),
		}
		tokens.pop_front();
	}
}

fn parse_colon_and_type(
	flags: &EnumSet<DeclarationFlag>,
	location_of_declaration: &Location,
	tokens: &mut Tokens,
) -> Poisonable<ValueType>
{
	consume(Token::Colon, "expected colon", tokens)?;
	let value_type = parse_type(tokens)?;
	let value_type = fix_type_for_flags(
		value_type,
		&flags,
		&tokens.last_location,
		location_of_declaration,
	)?;
	Ok(value_type)
}

fn parse_type(tokens: &mut Tokens) -> Result<ValueType, Error>
{
	let (token, location) = extract("expected type keyword", tokens)?;
	match token
	{
		Token::Type(value_type) => Ok(value_type),
		Token::Ampersand =>
		{
			let deref_type = parse_type(tokens)?;
			Ok(ValueType::Pointer {
				deref_type: Box::new(deref_type),
			})
		}
		Token::ParenLeft =>
		{
			let deref_type = parse_type(tokens)?;
			consume(Token::ParenRight, "expected right parenthesis", tokens)?;
			Ok(ValueType::View {
				deref_type: Box::new(deref_type),
			})
		}
		Token::BracketLeft => match peek(tokens)
		{
			Some(Token::BracketRight) | None =>
			{
				consume(Token::BracketRight, "expected right bracket", tokens)?;
				let element_type = parse_type(tokens)?;
				Ok(ValueType::Slice {
					element_type: Box::new(element_type),
				})
			}
			Some(Token::NakedInteger(x)) if *x > 0 =>
			{
				let length = *x as usize;
				tokens.pop_front();
				consume(Token::BracketRight, "expected right bracket", tokens)?;
				let element_type = parse_type(tokens)?;
				Ok(ValueType::Array {
					element_type: Box::new(element_type),
					length,
				})
			}
			Some(Token::Usize(x)) if *x > 0 =>
			{
				let length = *x;
				tokens.pop_front();
				consume(Token::BracketRight, "expected right bracket", tokens)?;
				let element_type = parse_type(tokens)?;
				Ok(ValueType::Array {
					element_type: Box::new(element_type),
					length,
				})
			}
			Some(_) => Err(Error::UnexpectedToken {
				expectation: "expected right bracket".to_string(),
				location,
			}),
		},
		_ => Err(Error::UnexpectedToken {
			expectation: "expected type keyword".to_string(),
			location,
		}),
	}
}

fn parse_function_body(
	function_name: &str,
	tokens: &mut Tokens,
) -> Poisonable<FunctionBody>
{
	consume(Token::BraceLeft, "expected function body", tokens)?;

	let mut statements = Vec::new();

	loop
	{
		if let Some(Token::BraceRight) = peek(tokens)
		{
			tokens.pop_front();

			let body = FunctionBody {
				statements,
				return_value: None,
				return_value_identifier: Identifier {
					name: function_name.to_string(),
					location: tokens.last_location.clone(),
					resolution_id: 0,
				},
			};
			return Ok(body);
		}

		let statement = match parse_statement(tokens)
		{
			Ok(statement) => statement,
			Err(error) =>
			{
				let body = FunctionBody {
					statements,
					return_value: None,
					return_value_identifier: Identifier {
						name: function_name.to_string(),
						location: tokens.last_location.clone(),
						resolution_id: 0,
					},
				};
				return Err(Poison::Error {
					error,
					partial: Some(body),
				});
			}
		};

		let is_return = match &statement
		{
			Statement::Label { label, .. } => label.name == "return",
			_ => false,
		};
		if is_return
		{
			if let Some(Token::BraceRight) = peek(tokens)
			{
				tokens.pop_front();
				let return_value_location = tokens.last_location.clone();
				let return_statement_location = statement.location().clone();
				statements.push(statement);
				let return_value = Expression::Poison(Poison::Error {
					error: Error::MissingReturnValueAfterStatement {
						location: return_value_location.clone(),
						after: return_statement_location,
					},
					partial: None,
				});
				let body = FunctionBody {
					statements,
					return_value: Some(return_value),
					return_value_identifier: Identifier {
						name: function_name.to_string(),
						location: return_value_location,
						resolution_id: 0,
					},
				};
				return Ok(body);
			}
			else
			{
				statements.push(statement);

				let return_value = parse_expression(tokens)?;
				if let Some(Token::Semicolon) = peek(tokens)
				{
					tokens.pop_front();
					let error = Error::UnexpectedSemicolonAfterReturnValue {
						location: tokens.last_location.clone(),
						after: return_value.location().clone(),
					};
					return Err(error.into());
				}
				consume(
					Token::BraceRight,
					"expected closing brace after return value",
					tokens,
				)?;
				let return_value_location = return_value.location().clone();
				let body = FunctionBody {
					statements,
					return_value: Some(return_value),
					return_value_identifier: Identifier {
						name: function_name.to_string(),
						location: return_value_location,
						resolution_id: 0,
					},
				};
				return Ok(body);
			};
		}
		else
		{
			statements.push(statement);
		}
	}
}

fn parse_rest_of_block(
	mut block: Block,
	tokens: &mut Tokens,
) -> Result<Block, Error>
{
	loop
	{
		if let Some(Token::BraceRight) = peek(tokens)
		{
			tokens.pop_front();
			block.location =
				block.location.combined_with(&tokens.last_location);
			return Ok(block);
		}

		let statement = parse_statement(tokens)?;
		block.statements.push(statement);
	}
}

fn parse_statement(tokens: &mut Tokens) -> Result<Statement, Error>
{
	let (token, location) = extract("expected statement", tokens)?;

	match token
	{
		Token::BraceLeft =>
		{
			let block = Block {
				statements: Vec::new(),
				location,
			};
			let block = parse_rest_of_block(block, tokens)?;
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
				let location_of_else = tokens.last_location.clone();

				let else_stmt = parse_statement(tokens)?;
				let else_branch = Some(Else {
					branch: Box::new(else_stmt),
					location_of_else,
				});
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
			let statement = Statement::Loop { location };
			consume(Token::Semicolon, "expected semicolon", tokens)?;
			Ok(statement)
		}
		Token::Goto =>
		{
			let label = extract_identifier("expected label", tokens)?;
			let statement = Statement::Goto { label, location };
			consume(Token::Semicolon, "expected semicolon", tokens)?;
			Ok(statement)
		}
		Token::Var =>
		{
			let name = extract_identifier("expected variable name", tokens)?;

			let value_type = if let Some(Token::Colon) = peek(tokens)
			{
				tokens.pop_front();
				let value_type = parse_type(tokens)?;
				Some(Ok(value_type))
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

			let statement = Statement::Declaration {
				name,
				value,
				value_type,
				location,
			};
			consume(Token::Semicolon, "expected semicolon", tokens)?;
			Ok(statement)
		}
		Token::Identifier(x) =>
		{
			if let Some(Token::Colon) = peek(tokens)
			{
				tokens.pop_front();
				let location = location.combined_with(&tokens.last_location);
				let statement = Statement::Label {
					label: Identifier {
						name: x,
						location: location.clone(),
						resolution_id: 0,
					},
					location,
				};
				return Ok(statement);
			}
			else if let Some(Token::ParenLeft) = peek(tokens)
			{
				let identifier = Identifier {
					name: x,
					location,
					resolution_id: 0,
				};
				let arguments = parse_arguments(tokens)?;
				let statement = Statement::MethodCall {
					name: identifier,
					arguments,
				};
				consume(Token::Semicolon, "expected semicolon", tokens)?;
				return Ok(statement);
			}

			let reference =
				parse_rest_of_reference(x, location.clone(), tokens)?;

			if let Some(Token::Semicolon) = peek(tokens)
			{
				tokens.pop_front();
				return Err(Error::UnexpectedSemicolonAfterIdentifier {
					location: tokens.last_location.clone(),
					after: reference.location.clone(),
				});
			}
			consume(Token::Assignment, "expected assignment", tokens)?;
			let expression = parse_expression(tokens)?;

			let statement = Statement::Assignment {
				reference,
				value: expression,
				location,
			};
			consume(Token::Semicolon, "expected semicolon", tokens)?;
			Ok(statement)
		}
		Token::Ampersand =>
		{
			let assignment_location = location.clone();
			let reference = parse_addressed_reference(location, tokens)?;

			if let Some(Token::Semicolon) = peek(tokens)
			{
				tokens.pop_front();
				return Err(Error::UnexpectedSemicolonAfterIdentifier {
					location: tokens.last_location.clone(),
					after: reference.location.clone(),
				});
			}
			consume(Token::Assignment, "expected assignment", tokens)?;
			let expression = parse_expression(tokens)?;

			let statement = Statement::Assignment {
				reference,
				value: expression,
				location: assignment_location,
			};
			consume(Token::Semicolon, "expected semicolon", tokens)?;
			Ok(statement)
		}
		_ => Err(Error::UnexpectedToken {
			location,
			expectation: "expected statement".to_string(),
		}),
	}
}

fn parse_comparison(tokens: &mut Tokens) -> Result<Comparison, Error>
{
	let left = parse_expression(tokens)?;

	let (token, location_of_op) =
		extract("expected comparison operator", tokens)?;
	let op = match token
	{
		Token::Equals => ComparisonOp::Equals,
		Token::DoesNotEqual => ComparisonOp::DoesNotEqual,
		Token::AngleLeft => ComparisonOp::IsLess,
		Token::AngleRight => ComparisonOp::IsGreater,
		Token::IsGE => ComparisonOp::IsGE,
		Token::IsLE => ComparisonOp::IsLE,
		_ =>
		{
			return Err(Error::UnexpectedToken {
				location: location_of_op,
				expectation: "expected comparison operator".to_string(),
			});
		}
	};

	let right = parse_expression(tokens)?;
	let location = left.location().clone().combined_with(right.location());

	Ok(Comparison {
		op,
		left,
		right,
		location,
		location_of_op,
	})
}

fn parse_expression(tokens: &mut Tokens) -> Result<Expression, Error>
{
	parse_addition(tokens)
}

fn parse_addition(tokens: &mut Tokens) -> Result<Expression, Error>
{
	let mut expression = parse_multiplication(tokens)?;
	let mut location = expression.location().clone();

	loop
	{
		let op = match peek(tokens)
		{
			Some(Token::Ampersand) | Some(Token::Pipe) | Some(Token::Caret) =>
			{
				return parse_rest_of_bitwise_expression(expression, tokens);
			}
			Some(Token::ShiftLeft) | Some(Token::ShiftRight) =>
			{
				return parse_rest_of_bitshift_operation(expression, tokens);
			}
			Some(Token::Plus) => BinaryOp::Add,
			Some(Token::Minus) => BinaryOp::Subtract,
			_ =>
			{
				return Ok(expression);
			}
		};
		tokens.pop_front();
		let location_of_op = tokens.last_location.clone();

		let right = parse_multiplication(tokens)?;
		location = location.combined_with(right.location());

		expression = Expression::Binary {
			op,
			left: Box::new(expression),
			right: Box::new(right),
			location: location.clone(),
			location_of_op,
		};
	}
}

fn parse_rest_of_bitwise_expression(
	mut expression: Expression,
	tokens: &mut Tokens,
) -> Result<Expression, Error>
{
	let (op_token, location_of_op) =
		extract("expected bitwise operator", tokens)?;
	let op = match op_token
	{
		Token::Ampersand => BinaryOp::BitwiseAnd,
		Token::Pipe => BinaryOp::BitwiseOr,
		Token::Caret => BinaryOp::BitwiseXor,
		_ =>
		{
			return Err(Error::UnexpectedToken {
				location: location_of_op,
				expectation: "expected bitwise operator".to_string(),
			});
		}
	};

	// Do not allow different binary operations inside a bitwise expression.
	match expression
	{
		Expression::Binary { .. } =>
		{
			return Err(Error::UnexpectedToken {
				location: location_of_op,
				expectation: "bitwise operator is not allowed here".to_string(),
			});
		}
		_ => (),
	}

	let mut location = expression.location().clone();
	let mut location_of_op = location_of_op;
	loop
	{
		let right = parse_unary_expression(tokens)?;
		location = location.combined_with(right.location());

		expression = Expression::Binary {
			op,
			left: Box::new(expression),
			right: Box::new(right),
			location: location.clone(),
			location_of_op,
		};

		match peek(tokens)
		{
			Some(token) if token == &op_token =>
			{
				tokens.pop_front();
				location_of_op = tokens.last_location.clone();
			}
			_ =>
			{
				return Ok(expression);
			}
		}
	}
}

fn parse_rest_of_bitshift_operation(
	mut expression: Expression,
	tokens: &mut Tokens,
) -> Result<Expression, Error>
{
	let (op_token, location_of_op) =
		extract("expected bitshift operator", tokens)?;
	let op = match op_token
	{
		Token::ShiftLeft => BinaryOp::ShiftLeft,
		Token::ShiftRight => BinaryOp::ShiftRight,
		_ =>
		{
			return Err(Error::UnexpectedToken {
				location: location_of_op,
				expectation: "expected bitshift operator".to_string(),
			});
		}
	};

	// Do not allow different binary operations inside a bitshift expression.
	match expression
	{
		Expression::Binary { .. } =>
		{
			return Err(Error::UnexpectedToken {
				location: location_of_op,
				expectation: "bitshift operator is not allowed here"
					.to_string(),
			});
		}
		_ => (),
	}

	let right = parse_unary_expression(tokens)?;
	let location = expression
		.location()
		.clone()
		.combined_with(right.location());

	expression = Expression::Binary {
		op,
		left: Box::new(expression),
		right: Box::new(right),
		location,
		location_of_op,
	};
	Ok(expression)
}

fn parse_multiplication(tokens: &mut Tokens) -> Result<Expression, Error>
{
	let mut expression = parse_singular_expression(tokens)?;
	let mut location = expression.location().clone();

	loop
	{
		let op = match peek(tokens)
		{
			Some(Token::Times) => BinaryOp::Multiply,
			Some(Token::Divide) => BinaryOp::Divide,
			Some(Token::Modulo) => BinaryOp::Modulo,
			_ =>
			{
				return Ok(expression);
			}
		};
		tokens.pop_front();
		let location_of_op = tokens.last_location.clone();

		let right = parse_singular_expression(tokens)?;
		location = location.combined_with(right.location());

		expression = Expression::Binary {
			op,
			left: Box::new(expression),
			right: Box::new(right),
			location: location.clone(),
			location_of_op,
		};
	}
}

fn parse_singular_expression(tokens: &mut Tokens) -> Result<Expression, Error>
{
	let mut expression = parse_unary_expression(tokens)?;
	let mut location = expression.location().clone();

	loop
	{
		match peek(tokens)
		{
			Some(Token::As) => (),
			_ =>
			{
				return Ok(expression);
			}
		};
		tokens.pop_front();

		let location_of_type = tokens.get_next_location();
		let coerced_type = parse_type(tokens)?;

		let location_of_type = match location_of_type
		{
			Some(location) => location.combined_with(&tokens.last_location),
			None => tokens.last_location.clone(),
		};
		location = location.combined_with(&location_of_type);

		expression = Expression::PrimitiveCast {
			expression: Box::new(expression),
			coerced_type,
			location: location.clone(),
			location_of_type,
		};
	}
}

fn parse_unary_expression(tokens: &mut Tokens) -> Result<Expression, Error>
{
	match peek(tokens)
	{
		Some(Token::Pipe) =>
		{
			tokens.pop_front();
			let reference = parse_reference(tokens)?;
			consume(Token::Pipe, "expected pipe", tokens)?;
			let expression = Expression::LengthOfArray { reference };
			Ok(expression)
		}
		Some(Token::Exclamation) =>
		{
			tokens.pop_front();
			let location_of_op = tokens.last_location.clone();
			let expr = parse_primary_expression(tokens)?;
			let location =
				location_of_op.clone().combined_with(expr.location());
			let expression = Expression::Unary {
				op: UnaryOp::BitwiseComplement,
				expression: Box::new(expr),
				location,
				location_of_op,
			};
			Ok(expression)
		}
		Some(Token::Minus) =>
		{
			tokens.pop_front();
			let location_of_op = tokens.last_location.clone();
			let expr = parse_primary_expression(tokens)?;
			let location =
				location_of_op.clone().combined_with(expr.location());
			match expr
			{
				Expression::NakedIntegerLiteral {
					value,
					value_type,
					location: _,
				} => Ok(Expression::NakedIntegerLiteral {
					value: -value,
					value_type,
					location,
				}),
				Expression::PrimitiveLiteral {
					literal: PrimitiveLiteral::Int8(value),
					location: _,
				} => Ok(Expression::PrimitiveLiteral {
					literal: PrimitiveLiteral::Int8(-value),
					location,
				}),
				Expression::PrimitiveLiteral {
					literal: PrimitiveLiteral::Int16(value),
					location: _,
				} => Ok(Expression::PrimitiveLiteral {
					literal: PrimitiveLiteral::Int16(-value),
					location,
				}),
				Expression::PrimitiveLiteral {
					literal: PrimitiveLiteral::Int32(value),
					location: _,
				} => Ok(Expression::PrimitiveLiteral {
					literal: PrimitiveLiteral::Int32(-value),
					location,
				}),
				Expression::PrimitiveLiteral {
					literal: PrimitiveLiteral::Int64(value),
					location: _,
				} => Ok(Expression::PrimitiveLiteral {
					literal: PrimitiveLiteral::Int64(-value),
					location,
				}),
				Expression::PrimitiveLiteral {
					literal: PrimitiveLiteral::Int128(value),
					location: _,
				} => Ok(Expression::PrimitiveLiteral {
					literal: PrimitiveLiteral::Int128(-value),
					location,
				}),
				expr =>
				{
					let expression = Expression::Unary {
						op: UnaryOp::Negative,
						expression: Box::new(expr),
						location,
						location_of_op,
					};
					Ok(expression)
				}
			}
		}
		_ => parse_primary_expression(tokens),
	}
}

fn parse_primary_expression(tokens: &mut Tokens) -> Result<Expression, Error>
{
	let (token, location) = extract("expected literal or identifier", tokens)?;
	match token
	{
		Token::NakedInteger(value) => Ok(Expression::NakedIntegerLiteral {
			value,
			value_type: None,
			location,
		}),
		Token::BitInteger(value) => Ok(Expression::BitIntegerLiteral {
			value,
			value_type: None,
			location,
		}),
		Token::Int8(value) =>
		{
			let literal = PrimitiveLiteral::Int8(value);
			Ok(Expression::PrimitiveLiteral { literal, location })
		}
		Token::Int16(value) =>
		{
			let literal = PrimitiveLiteral::Int16(value);
			Ok(Expression::PrimitiveLiteral { literal, location })
		}
		Token::Int32(value) =>
		{
			let literal = PrimitiveLiteral::Int32(value);
			Ok(Expression::PrimitiveLiteral { literal, location })
		}
		Token::Int64(value) =>
		{
			let literal = PrimitiveLiteral::Int64(value);
			Ok(Expression::PrimitiveLiteral { literal, location })
		}
		Token::Int128(value) =>
		{
			let literal = PrimitiveLiteral::Int128(value);
			Ok(Expression::PrimitiveLiteral { literal, location })
		}
		Token::Uint8(value) =>
		{
			let literal = PrimitiveLiteral::Uint8(value);
			Ok(Expression::PrimitiveLiteral { literal, location })
		}
		Token::Uint16(value) =>
		{
			let literal = PrimitiveLiteral::Uint16(value);
			Ok(Expression::PrimitiveLiteral { literal, location })
		}
		Token::Uint32(value) =>
		{
			let literal = PrimitiveLiteral::Uint32(value);
			Ok(Expression::PrimitiveLiteral { literal, location })
		}
		Token::Uint64(value) =>
		{
			let literal = PrimitiveLiteral::Uint64(value);
			Ok(Expression::PrimitiveLiteral { literal, location })
		}
		Token::Uint128(value) =>
		{
			let literal = PrimitiveLiteral::Uint128(value);
			Ok(Expression::PrimitiveLiteral { literal, location })
		}
		Token::Usize(value) =>
		{
			let literal = PrimitiveLiteral::Usize(value);
			Ok(Expression::PrimitiveLiteral { literal, location })
		}
		Token::Bool(value) =>
		{
			let literal = PrimitiveLiteral::Bool(value);
			Ok(Expression::PrimitiveLiteral { literal, location })
		}
		Token::StringLiteral { bytes, value_type } =>
		{
			let mut bytes = bytes;
			let mut value_type = value_type;
			let mut location = location;
			while let Some(next_token) = peek(tokens)
			{
				match next_token
				{
					Token::StringLiteral { .. } =>
					{}
					_ => break,
				}

				let (token, extra_location) = extract("", tokens)?;
				match token
				{
					Token::StringLiteral {
						bytes: mut extra_bytes,
						value_type: other_value_type,
					} =>
					{
						if other_value_type.is_some()
						{
							if value_type.is_none()
							{
								value_type = other_value_type;
							}
							else if value_type != other_value_type
							{
								break;
							}
						}
						bytes.append(&mut extra_bytes);
					}
					_ => unreachable!(),
				}
				location = location.combined_with(&extra_location);
			}
			Ok(Expression::StringLiteral {
				bytes,
				value_type: value_type.map(|x| Ok(x)),
				location,
			})
		}
		Token::Identifier(name) =>
		{
			if let Some(Token::ParenLeft) = peek(tokens)
			{
				let identifier = Identifier {
					name,
					location,
					resolution_id: 0,
				};
				let arguments = parse_arguments(tokens)?;
				Ok(Expression::FunctionCall {
					name: identifier,
					arguments,
					return_type: None,
				})
			}
			else
			{
				let reference =
					parse_rest_of_reference(name, location, tokens)?;
				Ok(Expression::Deref {
					reference,
					deref_type: None,
				})
			}
		}
		Token::Ampersand =>
		{
			let reference = parse_addressed_reference(location, tokens)?;
			Ok(Expression::Deref {
				reference,
				deref_type: None,
			})
		}
		Token::BracketLeft =>
		{
			let array = Array {
				elements: Vec::new(),
				location,
				resolution_id: 0,
			};
			let array = parse_rest_of_array(array, tokens)?;
			let expression = Expression::ArrayLiteral {
				array,
				element_type: None,
			};
			Ok(expression)
		}
		_ => Err(Error::UnexpectedToken {
			location,
			expectation: "expected literal or identifier".to_string(),
		}),
	}
}

fn parse_arguments(tokens: &mut Tokens) -> Result<Vec<Expression>, Error>
{
	consume(Token::ParenLeft, "expected argument list", tokens)?;

	if let Some(Token::ParenRight) = peek(tokens)
	{
		tokens.pop_front();
		return Ok(Vec::new());
	}

	let mut arguments = Vec::new();

	loop
	{
		let expression = parse_expression(tokens)?;
		arguments.push(expression);

		if let Some(Token::Comma) = peek(tokens)
		{
			tokens.pop_front();
		}
		else
		{
			break;
		}
	}

	consume(
		Token::ParenRight,
		"expect comma or right parenthesis",
		tokens,
	)?;

	Ok(arguments)
}

fn parse_rest_of_array(
	mut array: Array,
	tokens: &mut Tokens,
) -> Result<Array, Error>
{
	loop
	{
		if let Some(Token::BracketRight) = peek(tokens)
		{
			break;
		}

		let element = parse_expression(tokens)?;
		array.elements.push(element);

		if let Some(Token::Comma) = peek(tokens)
		{
			tokens.pop_front();
		}
		else
		{
			break;
		}
	}

	consume(
		Token::BracketRight,
		"expected comma or right bracket",
		tokens,
	)?;
	array.location = array.location.combined_with(&tokens.last_location);

	Ok(array)
}

fn parse_addressed_reference(
	location: Location,
	tokens: &mut Tokens,
) -> Result<Reference, Error>
{
	let Reference {
		base,
		steps,
		address_depth,
		location: _,
	} = parse_reference(tokens)?;
	if address_depth + 1 > MAX_ADDRESS_DEPTH
	{
		return Err(Error::MaximumParseDepthExceeded {
			location: location.combined_with(&tokens.last_location),
		});
	}
	let reference = Reference {
		base,
		steps,
		address_depth: address_depth + 1,
		location,
	};
	Ok(reference)
}

fn parse_reference(tokens: &mut Tokens) -> Result<Reference, Error>
{
	let (token, location) = extract("expected identifier", tokens)?;
	match token
	{
		Token::Ampersand =>
		{
			let mut address_depth: u8 = 1;
			while let Some(Token::Ampersand) = peek(tokens)
			{
				tokens.pop_front();
				let location_of_address = tokens.last_location.clone();
				address_depth += 1;
				if address_depth > MAX_ADDRESS_DEPTH
				{
					return Err(Error::MaximumParseDepthExceeded {
						location: location.combined_with(&location_of_address),
					});
				}
			}
			let reference = parse_reference(tokens)?;
			Ok(Reference {
				base: reference.base,
				steps: reference.steps,
				address_depth,
				location,
			})
		}
		Token::Identifier(name) =>
		{
			parse_rest_of_reference(name, location, tokens)
		}
		_ => Err(Error::UnexpectedToken {
			location,
			expectation: "expected identifier".to_string(),
		}),
	}
}

fn parse_rest_of_reference(
	name: String,
	location: Location,
	tokens: &mut Tokens,
) -> Result<Reference, Error>
{
	let base = Identifier {
		name,
		location: location.clone(),
		resolution_id: 0,
	};
	let mut steps = Vec::new();
	while let Some(Token::BracketLeft) = peek(tokens)
	{
		tokens.pop_front();
		let argument = parse_expression(tokens)?;
		consume(Token::BracketRight, "expected right bracket", tokens)?;
		let step = ReferenceStep::Element {
			argument: Box::new(argument),
		};
		steps.push(step);
		if steps.len() > MAX_REFERENCE_DEPTH
		{
			return Err(Error::MaximumParseDepthExceeded {
				location: location.combined_with(&tokens.last_location),
			});
		}
	}
	Ok(Reference {
		base: Ok(base),
		steps,
		address_depth: 0,
		location,
	})
}

fn fix_type_for_flags(
	value_type: ValueType,
	flags: &EnumSet<DeclarationFlag>,
	location_of_type: &Location,
	location_of_declaration: &Location,
) -> Poisonable<ValueType>
{
	if flags.contains(DeclarationFlag::External)
	{
		match value_type
		{
			ValueType::Array {
				element_type,
				length: _,
			}
			| ValueType::Slice { element_type } =>
			{
				let element_type = externalize_type(
					*element_type,
					location_of_type,
					location_of_declaration,
				)?;
				Ok(ValueType::View {
					deref_type: Box::new(ValueType::ExtArray {
						element_type: Box::new(element_type),
					}),
				})
			}
			_ => externalize_type(
				value_type,
				location_of_type,
				location_of_declaration,
			),
		}
	}
	else
	{
		Ok(value_type)
	}
}

fn externalize_type(
	value_type: ValueType,
	location_of_type: &Location,
	location_of_declaration: &Location,
) -> Poisonable<ValueType>
{
	match value_type
	{
		ValueType::Array {
			element_type,
			length: _,
		}
		| ValueType::Slice { element_type }
		| ValueType::ExtArray { element_type } =>
		{
			let element_type = externalize_type(
				*element_type,
				location_of_type,
				location_of_declaration,
			)?;
			Ok(ValueType::ExtArray {
				element_type: Box::new(element_type),
			})
		}
		ValueType::Pointer { deref_type } =>
		{
			let deref_type = externalize_type(
				*deref_type,
				location_of_type,
				location_of_declaration,
			)?;
			Ok(ValueType::Pointer {
				deref_type: Box::new(deref_type),
			})
		}
		ValueType::View { deref_type } =>
		{
			let deref_type = externalize_type(
				*deref_type,
				location_of_type,
				location_of_declaration,
			)?;
			Ok(ValueType::View {
				deref_type: Box::new(deref_type),
			})
		}
		ValueType::Int8 => Ok(value_type),
		ValueType::Int16 => Ok(value_type),
		ValueType::Int32 => Ok(value_type),
		ValueType::Int64 => Ok(value_type),
		ValueType::Int128 => Ok(value_type),
		ValueType::Uint8 => Ok(value_type),
		ValueType::Uint16 => Ok(value_type),
		ValueType::Uint32 => Ok(value_type),
		ValueType::Uint64 => Ok(value_type),
		ValueType::Uint128 => Ok(value_type),
		ValueType::Usize => Ok(value_type),
		ValueType::Bool => Ok(value_type),
		_ => Err(Poison::Error {
			error: Error::TypeNotAllowedInExtern {
				value_type: value_type.clone(),
				location_of_type: location_of_type.clone(),
				location_of_declaration: location_of_declaration.clone(),
			},
			partial: Some(value_type),
		}),
	}
}
