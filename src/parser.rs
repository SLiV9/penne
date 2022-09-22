/**/

use crate::common::*;
use crate::lexer::{LexedToken, Token};

use std::collections::VecDeque;

use anyhow::anyhow;
use anyhow::Context;
use enumset::EnumSet;

pub const MAX_ADDRESS_DEPTH: u8 = 127;
pub const MAX_REFERENCE_DEPTH: usize = 127;

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

fn peek_location(
	tokens: &mut VecDeque<LexedToken>,
) -> Result<Location, anyhow::Error>
{
	match tokens.front()
	{
		Some(LexedToken {
			result: _,
			location,
		}) => Ok(location.clone()),
		None => Err(anyhow!("unexpected end of file")),
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
		}) => Ok(Identifier {
			name,
			location,
			resolution_id: 0,
		}),
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
	let mut flags = EnumSet::new();
	if let Some(Token::Pub) = peek(tokens)
	{
		tokens.pop_front();
		flags.insert(DeclarationFlag::Public);
	}
	if let Some(Token::Extern) = peek(tokens)
	{
		tokens.pop_front();
		flags.insert(DeclarationFlag::External);
	}

	let (token, location) =
		extract(tokens).context("expected top-level declaration")?;
	match token
	{
		Token::Const =>
		{
			let name =
				extract_identifier(tokens).context("expected constant name")?;

			consume(Token::Colon, tokens).context("expected colon")?;
			let value_type = parse_type(tokens)?;
			let value_type = fix_type_for_flags(value_type, &flags)
				.with_context(|| location.format())
				.with_context(|| "malformed constant declaration")?;

			consume(Token::Assignment, tokens)
				.context("expected assignment")?;
			let expression = parse_expression(tokens)?;

			consume(Token::Semicolon, tokens).context("expected semicolon")?;

			let declaration = Declaration::Constant {
				name,
				value: expression,
				value_type,
				flags,
			};
			Ok(declaration)
		}
		Token::Fn => parse_function_declaration(flags, tokens),
		token => Err(anyhow!("got {:?}", token))
			.context(location.format())
			.context("expected top-level declaration"),
	}
}

fn parse_function_declaration(
	flags: EnumSet<DeclarationFlag>,
	tokens: &mut VecDeque<LexedToken>,
) -> Result<Declaration, anyhow::Error>
{
	let name = extract_identifier(tokens).context("expected function name")?;

	consume(Token::ParenLeft, tokens).context("expected left parenthesis")?;

	let mut parameters = Vec::new();
	loop
	{
		if let Some(Token::ParenRight) = peek(tokens)
		{
			break;
		}

		let parameter = parse_parameter(&flags, tokens)?;
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

	if flags.contains(DeclarationFlag::External)
		&& peek(tokens) == Some(&Token::Semicolon)
	{
		tokens.pop_front();

		Ok(Declaration::FunctionHead {
			name,
			parameters,
			return_type,
			flags,
		})
	}
	else
	{
		let body = parse_function_body(&name.name, tokens)?;
		let function = Declaration::Function {
			name,
			parameters,
			body,
			return_type,
			flags,
		};
		Ok(function)
	}
}

fn parse_parameter(
	flags: &EnumSet<DeclarationFlag>,
	tokens: &mut VecDeque<LexedToken>,
) -> Result<Parameter, anyhow::Error>
{
	let name = extract_identifier(tokens).context("expected parameter name")?;

	let value_type = if let Some(Token::Colon) = peek(tokens)
	{
		tokens.pop_front();

		let value_type = parse_type(tokens)?;
		let value_type = fix_type_for_flags(value_type, &flags)
			.with_context(|| name.location.format())
			.with_context(|| "malformed function declaration")?;
		Some(value_type)
	}
	else
	{
		None
	};

	Ok(Parameter { name, value_type })
}

fn parse_type(
	tokens: &mut VecDeque<LexedToken>,
) -> Result<ValueType, anyhow::Error>
{
	let (token, location) = extract(tokens).context("expected type keyword")?;
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
			consume(Token::ParenRight, tokens)
				.context("expected right parenthesis")?;
			Ok(ValueType::View {
				deref_type: Box::new(deref_type),
			})
		}
		Token::BracketLeft => match peek(tokens)
		{
			Some(Token::BracketRight) | None =>
			{
				consume(Token::BracketRight, tokens)
					.context("expected right bracket")?;
				let element_type = parse_type(tokens)?;
				Ok(ValueType::Slice {
					element_type: Box::new(element_type),
				})
			}
			Some(Token::NakedInteger(x)) if *x > 0 =>
			{
				let length = *x as usize;
				tokens.pop_front();
				consume(Token::BracketRight, tokens)
					.context("expected right bracket")?;
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
				consume(Token::BracketRight, tokens)
					.context("expected right bracket")?;
				let element_type = parse_type(tokens)?;
				Ok(ValueType::Array {
					element_type: Box::new(element_type),
					length,
				})
			}
			Some(token) => Err(anyhow!("got {:?}", token))
				.context(location.format())
				.context("expected array length or right bracket"),
		},
		token => Err(anyhow!("got {:?}", token))
			.context(location.format())
			.context("expected type keyword"),
	}
}

fn parse_function_body(
	function_name: &str,
	tokens: &mut VecDeque<LexedToken>,
) -> Result<FunctionBody, anyhow::Error>
{
	consume(Token::BraceLeft, tokens).context("expected function body")?;

	let mut statements = Vec::new();

	loop
	{
		if let Some(Token::BraceRight) = peek(tokens)
		{
			let (_token, location) = extract(tokens)?;

			let body = FunctionBody {
				statements,
				return_value: None,
				return_value_identifier: Identifier {
					name: function_name.to_string(),
					location,
					resolution_id: 0,
				},
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

	let location = peek_location(tokens)
		.ok()
		.context("expected closing brace")?;
	let return_value = parse_expression(tokens)?;

	consume(Token::BraceRight, tokens).context("expected closing brace")?;

	let body = FunctionBody {
		statements,
		return_value: Some(return_value),
		return_value_identifier: Identifier {
			name: function_name.to_string(),
			location,
			resolution_id: 0,
		},
	};
	Ok(body)
}

fn parse_rest_of_block(
	mut block: Block,
	tokens: &mut VecDeque<LexedToken>,
) -> Result<Block, anyhow::Error>
{
	loop
	{
		if let Some(Token::BraceRight) = peek(tokens)
		{
			let _ = extract(tokens)?;

			return Ok(block);
		}

		let statement = parse_statement(tokens)?;
		block.statements.push(statement);
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
				consume(Token::Semicolon, tokens)
					.context("expected semicolon")?;
				let statement = Statement::MethodCall {
					name: identifier,
					arguments,
				};
				return Ok(statement);
			}

			let reference =
				parse_rest_of_reference(x, location.clone(), tokens)?;

			consume(Token::Assignment, tokens)
				.context("expected assignment")?;
			let expression = parse_expression(tokens)?;
			consume(Token::Semicolon, tokens).context("expected semicolon")?;

			let statement = Statement::Assignment {
				reference,
				value: expression,
				location,
			};
			Ok(statement)
		}
		Token::Ampersand =>
		{
			let assignment_location = location.clone();
			let reference = parse_addressed_reference(location, tokens)?;

			consume(Token::Assignment, tokens)
				.context("expected assignment")?;
			let expression = parse_expression(tokens)?;
			consume(Token::Semicolon, tokens).context("expected semicolon")?;

			let statement = Statement::Assignment {
				reference,
				value: expression,
				location: assignment_location,
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
	match peek(tokens)
	{
		Some(Token::Pipe) =>
		{
			tokens.pop_front();
			let reference = parse_reference(tokens)?;
			consume(Token::Pipe, tokens).context("expected pipe")?;
			let expression = Expression::LengthOfArray { reference };
			Ok(expression)
		}
		_ => parse_primary_expression(tokens),
	}
}

fn parse_primary_expression(
	tokens: &mut VecDeque<LexedToken>,
) -> Result<Expression, anyhow::Error>
{
	let (token, location) =
		extract(tokens).context("expected literal or identifier")?;
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
			Ok(Expression::PrimitiveLiteral(PrimitiveLiteral::Int8(value)))
		}
		Token::Int16(value) =>
		{
			Ok(Expression::PrimitiveLiteral(PrimitiveLiteral::Int16(value)))
		}
		Token::Int32(value) =>
		{
			Ok(Expression::PrimitiveLiteral(PrimitiveLiteral::Int32(value)))
		}
		Token::Int64(value) =>
		{
			Ok(Expression::PrimitiveLiteral(PrimitiveLiteral::Int64(value)))
		}
		Token::Int128(value) => Ok(Expression::PrimitiveLiteral(
			PrimitiveLiteral::Int128(value),
		)),
		Token::Uint8(value) =>
		{
			Ok(Expression::PrimitiveLiteral(PrimitiveLiteral::Uint8(value)))
		}
		Token::Uint16(value) => Ok(Expression::PrimitiveLiteral(
			PrimitiveLiteral::Uint16(value),
		)),
		Token::Uint32(value) => Ok(Expression::PrimitiveLiteral(
			PrimitiveLiteral::Uint32(value),
		)),
		Token::Uint64(value) => Ok(Expression::PrimitiveLiteral(
			PrimitiveLiteral::Uint64(value),
		)),
		Token::Uint128(value) => Ok(Expression::PrimitiveLiteral(
			PrimitiveLiteral::Uint128(value),
		)),
		Token::Usize(value) =>
		{
			Ok(Expression::PrimitiveLiteral(PrimitiveLiteral::Usize(value)))
		}
		Token::Bool(value) =>
		{
			Ok(Expression::PrimitiveLiteral(PrimitiveLiteral::Bool(value)))
		}
		Token::StringLiteral { bytes, value_type } =>
		{
			let mut bytes = bytes;
			let mut value_type = value_type;
			while let Some(next_token) = peek(tokens)
			{
				match next_token
				{
					Token::StringLiteral { .. } =>
					{}
					_ => break,
				}

				let (token, _location) = extract(tokens)?;
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
			}
			Ok(Expression::StringLiteral {
				bytes,
				value_type,
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
		let _ = extract(tokens)?;
		return Ok(Vec::new());
	}

	let mut arguments = Vec::new();

	loop
	{
		let expression = parse_expression(tokens)?;
		arguments.push(expression);

		if let Some(Token::ParenRight) = peek(tokens)
		{
			let _ = extract(tokens)?;
			return Ok(arguments);
		}

		consume(Token::Comma, tokens)
			.context("expected comma or right parenthesis")?;
	}
}

fn parse_rest_of_array(
	mut array: Array,
	tokens: &mut VecDeque<LexedToken>,
) -> Result<Array, anyhow::Error>
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

	consume(Token::BracketRight, tokens).context("expected right bracket")?;

	return Ok(array);
}

fn parse_addressed_reference(
	location: Location,
	tokens: &mut VecDeque<LexedToken>,
) -> Result<Reference, anyhow::Error>
{
	let Reference {
		base,
		steps,
		address_depth,
		location: _,
	} = parse_reference(tokens)?;
	if address_depth + 1 > MAX_ADDRESS_DEPTH
	{
		return Err(anyhow!("max address depth exceeded"))
			.context(location.format())
			.context("too many & operators");
	}
	let reference = Reference {
		base,
		steps,
		address_depth: address_depth + 1,
		location,
	};
	Ok(reference)
}

fn parse_reference(
	tokens: &mut VecDeque<LexedToken>,
) -> Result<Reference, anyhow::Error>
{
	let (token, location) = extract(tokens).context("expected identifier")?;
	match token
	{
		Token::Ampersand =>
		{
			let mut address_depth: u8 = 1;
			while let Some(Token::Ampersand) = peek(tokens)
			{
				tokens.pop_front();
				address_depth += 1;
				if address_depth > MAX_ADDRESS_DEPTH
				{
					return Err(anyhow!("max address depth exceeded"))
						.context(location.format())
						.context("too many & operators");
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
		other => Err(anyhow!("got {:?}", other))
			.context(location.format())
			.context("expected identifier"),
	}
}

fn parse_rest_of_reference(
	name: String,
	location: Location,
	tokens: &mut VecDeque<LexedToken>,
) -> Result<Reference, anyhow::Error>
{
	let base = Identifier {
		name,
		location: location.clone(),
		resolution_id: 0,
	};
	let mut steps = Vec::new();
	while let Some(Token::BracketLeft) = peek(tokens)
	{
		let _ = extract(tokens)?;
		let argument = parse_expression(tokens)?;
		consume(Token::BracketRight, tokens)
			.context("expected right bracket")?;
		let step = ReferenceStep::Element {
			argument: Box::new(argument),
		};
		steps.push(step);
		if steps.len() > MAX_REFERENCE_DEPTH
		{
			return Err(anyhow!("max reference depth exceeded"))
				.context(location.format())
				.context("too many index operators");
		}
	}
	Ok(Reference {
		base,
		steps,
		address_depth: 0,
		location,
	})
}

fn fix_type_for_flags(
	value_type: ValueType,
	flags: &EnumSet<DeclarationFlag>,
) -> Result<ValueType, anyhow::Error>
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
				let element_type = externalize_type(*element_type)?;
				Ok(ValueType::View {
					deref_type: Box::new(ValueType::ExtArray {
						element_type: Box::new(element_type),
					}),
				})
			}
			_ => externalize_type(value_type),
		}
	}
	else
	{
		Ok(value_type)
	}
}

fn externalize_type(value_type: ValueType) -> Result<ValueType, anyhow::Error>
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
			let element_type = externalize_type(*element_type)?;
			Ok(ValueType::ExtArray {
				element_type: Box::new(element_type),
			})
		}
		ValueType::Pointer { deref_type } =>
		{
			let deref_type = externalize_type(*deref_type)?;
			Ok(ValueType::Pointer {
				deref_type: Box::new(deref_type),
			})
		}
		ValueType::View { deref_type } =>
		{
			let deref_type = externalize_type(*deref_type)?;
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
		_ => Err(anyhow!("type {:?} not allowed in extern", value_type)),
	}
}
