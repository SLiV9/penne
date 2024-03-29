//
// Part of penne
// Copyright (c) 2020 Sander in 't Veld
// License: MIT
//

//! The parser takes lexed tokens and builds the common AST.

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

	if !tokens.is_empty()
	{
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
					let poison = error.into();
					let declaration = Declaration::Poison(poison);
					declarations.push(declaration);
					skip_until_next_declaration(&mut tokens);
				}
			}
		}
	}

	declarations
}

#[derive(Debug)]
struct Tokens
{
	tokens: VecDeque<LexedToken>,
	last_location: Location,
	reserved_token: Option<Token>,
}

impl From<Vec<LexedToken>> for Tokens
{
	fn from(tokens: Vec<LexedToken>) -> Tokens
	{
		let last_location = match tokens.first()
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
			reserved_token: None,
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

	fn start_location_span(&self) -> Option<Location>
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

	fn location_of_span(&self, start: Option<Location>) -> Location
	{
		match start
		{
			Some(location) => location.combined_with(&self.last_location),
			None => self.last_location.clone(),
		}
	}

	fn with_reservation(&mut self, token: Token) -> TokenReservation
	{
		self.reserved_token.get_or_insert(token);
		TokenReservation(self)
	}
}

struct TokenReservation<'a>(&'a mut Tokens);

impl<'a> AsMut<Tokens> for TokenReservation<'a>
{
	fn as_mut(&mut self) -> &mut Tokens
	{
		self.0
	}
}

impl<'a> Drop for TokenReservation<'a>
{
	fn drop(&mut self)
	{
		self.0.reserved_token = None;
	}
}

fn peek(tokens: &mut Tokens) -> Option<&Token>
{
	match tokens.tokens.front()
	{
		Some(LexedToken {
			result: Ok(token),
			location: _,
		}) => match &tokens.reserved_token
		{
			Some(x) if x == token => None,
			Some(_) => Some(token),
			None => Some(token),
		},
		Some(LexedToken {
			result: Err(_),
			location: _,
		}) => None,
		None => None,
	}
}

fn consume(expected_token: Token, tokens: &mut Tokens) -> Result<(), Error>
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
			expectation: expected_token.expectation().to_string(),
			location,
		}),
		Some(LexedToken {
			result: Err(error),
			location,
		}) => Err(Error::Lexical {
			error,
			expectation: expected_token.expectation().to_string(),
			location,
		}),
		None => Err(Error::UnexpectedEndOfFile {
			expectation: expected_token.expectation().to_string(),
			last_location: tokens.last_location.clone(),
		}),
	}
}

impl Token
{
	fn expectation(&self) -> &'static str
	{
		match self
		{
			Token::Assignment => "Expected assignment.",
			Token::BraceLeft => "Expected opening brace.",
			Token::BraceRight => "Expected closing brace.",
			Token::BracketLeft => "Expected opening bracket.",
			Token::BracketRight => "Expected closing bracket.",
			Token::Dot => "Expected dot.",
			Token::ParenLeft => "Expected opening parenthesis.",
			Token::ParenRight => "Expected closing parenthesis.",
			Token::Pipe => "Expected pipe.",
			Token::Semicolon => "Expected semicolon.",
			_ => unreachable!(),
		}
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
			is_authoritative: false,
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
		Token::Import => true,
		Token::Pub => true,
		Token::Extern => true,
		Token::Const => true,
		Token::Fn => true,
		Token::Struct => true,
		Token::Word8 => true,
		Token::Word16 => true,
		Token::Word32 => true,
		Token::Word64 => true,
		Token::Word128 => true,
		_ => false,
	}
}

fn skip_until_next_declaration(tokens: &mut Tokens)
{
	loop
	{
		match tokens.tokens.front()
		{
			Some(LexedToken {
				result: Ok(token),
				location: _,
			}) =>
			{
				if can_start_declaration(token)
				{
					return;
				}
				else
				{
					tokens.pop_front();
				}
			}
			Some(LexedToken {
				result: Err(_),
				location: _,
			}) =>
			{
				tokens.pop_front();
			}
			None => return,
		}
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

	let (token, location) = extract("Expected top-level declaration.", tokens)?;
	let location_of_declaration =
		location_of_declaration.unwrap_or_else(|| location.clone());
	match token
	{
		Token::Import => parse_import(location_of_declaration, tokens),
		Token::Const =>
		{
			parse_constant_declaration(flags, location_of_declaration, tokens)
		}
		Token::Fn =>
		{
			parse_function_declaration(flags, location_of_declaration, tokens)
		}
		Token::Struct =>
		{
			parse_struct_declaration(flags, location_of_declaration, tokens)
		}
		Token::Word8 =>
		{
			parse_word_declaration(1, flags, location_of_declaration, tokens)
		}
		Token::Word16 =>
		{
			parse_word_declaration(2, flags, location_of_declaration, tokens)
		}
		Token::Word32 =>
		{
			parse_word_declaration(4, flags, location_of_declaration, tokens)
		}
		Token::Word64 =>
		{
			parse_word_declaration(8, flags, location_of_declaration, tokens)
		}
		Token::Word128 =>
		{
			parse_word_declaration(16, flags, location_of_declaration, tokens)
		}
		_ => Err(Error::UnexpectedToken {
			location,
			expectation: "Expected top-level declaration.".to_string(),
		}),
	}
}

fn parse_import(
	location_of_import: Location,
	tokens: &mut Tokens,
) -> Result<Declaration, Error>
{
	let filename = parse_quoted_path(tokens)?;
	consume(Token::Semicolon, tokens)?;
	let declaration = Declaration::Import {
		filename,
		location: location_of_import,
	};
	Ok(declaration)
}

fn parse_quoted_path(tokens: &mut Tokens) -> Result<String, Error>
{
	let (token, location) = extract("Expected path.", tokens)?;
	match token
	{
		Token::StringLiteral { bytes } => match String::from_utf8(bytes)
		{
			Ok(filename) => Ok(filename),
			Err(_error) => Err(Error::UnexpectedToken {
				location,
				expectation: "Expected UTF-8 encoded path.".to_string(),
			}),
		},
		_ => Err(Error::UnexpectedToken {
			location,
			expectation: "Expected path.".to_string(),
		}),
	}
}

fn parse_constant_declaration(
	flags: EnumSet<DeclarationFlag>,
	location_of_declaration: Location,
	tokens: &mut Tokens,
) -> Result<Declaration, Error>
{
	let name = extract_identifier("Expected constant name.", tokens)?;
	let location_of_declaration =
		location_of_declaration.combined_with(&tokens.last_location);

	let start = tokens.start_location_span();
	let value_type = if let Some(Token::Colon) = peek(tokens)
	{
		tokens.pop_front();
		let value_type = parse_wellformed_type(tokens)?;
		Ok(value_type)
	}
	else
	{
		Err(Error::MissingConstantType {
			location: name.location.clone(),
		})
	};
	let value_type = value_type.map_err(|e| e.into());
	let location_of_type = tokens.location_of_span(start);

	consume(Token::Assignment, tokens)?;
	let expression = parse_expression(tokens)?;
	consume(Token::Semicolon, tokens)?;

	let declaration = Declaration::Constant {
		name,
		value: expression,
		value_type,
		flags,
		depth: None,
		location_of_declaration,
		location_of_type,
	};
	Ok(declaration)
}

fn parse_word_declaration(
	size_in_bytes: usize,
	flags: EnumSet<DeclarationFlag>,
	location_of_declaration: Location,
	tokens: &mut Tokens,
) -> Result<Declaration, Error>
{
	let name = extract_identifier("Expected structure name.", tokens)?;
	let structural_type = Ok(ValueType::Word {
		identifier: name.clone(),
		size_in_bytes,
	});
	let members = parse_struct_members(tokens)?;
	Ok(Declaration::Structure {
		name,
		members,
		structural_type,
		flags,
		depth: None,
		location_of_declaration,
	})
}

fn parse_struct_declaration(
	mut flags: EnumSet<DeclarationFlag>,
	location_of_declaration: Location,
	tokens: &mut Tokens,
) -> Result<Declaration, Error>
{
	let name = extract_identifier("Expected structure name.", tokens)?;
	let structural_type = Ok(ValueType::Struct {
		identifier: name.clone(),
	});
	let members = if peek(tokens) == Some(&Token::Semicolon)
	{
		consume(Token::Semicolon, tokens)?;
		flags.insert(DeclarationFlag::OpaqueStruct);
		Vec::new()
	}
	else
	{
		let members = parse_struct_members(tokens)?;
		members
	};
	Ok(Declaration::Structure {
		name,
		members,
		structural_type,
		flags,
		depth: None,
		location_of_declaration,
	})
}

fn parse_struct_members(tokens: &mut Tokens) -> Result<Vec<Member>, Error>
{
	consume(Token::BraceLeft, tokens)?;

	let mut members = Vec::new();
	loop
	{
		if let Some(Token::BraceRight) = peek(tokens)
		{
			break;
		}

		match parse_member(tokens)
		{
			Ok(member) =>
			{
				members.push(member);

				if let Some(Token::Comma) = peek(tokens)
				{
					tokens.pop_front();
				}
				else
				{
					// Break now, we will try to consume a right brace
					// which will create an error.
					break;
				}
			}
			Err(error) =>
			{
				return Err(error);
			}
		}
	}

	consume(Token::BraceRight, tokens)?;
	Ok(members)
}

fn parse_function_declaration(
	flags: EnumSet<DeclarationFlag>,
	location_of_declaration: Location,
	tokens: &mut Tokens,
) -> Result<Declaration, Error>
{
	let name = extract_identifier("Expected function name.", tokens)?;
	consume(Token::ParenLeft, tokens)?;
	let signature = parse_rest_of_function_signature(tokens)?;
	let (parameters, return_type, location_of_return_type) = signature;
	let location_of_return_type =
		location_of_return_type.unwrap_or_else(|| tokens.last_location.clone());
	let return_type = Ok(return_type);

	if peek(tokens) == Some(&Token::Semicolon)
	{
		tokens.pop_front();
		Ok(Declaration::FunctionHead {
			name,
			parameters,
			return_type,
			flags,
			location_of_declaration,
			location_of_return_type,
		})
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
		Ok(Declaration::Function {
			name,
			parameters,
			body,
			return_type,
			flags,
			location_of_declaration,
			location_of_return_type,
		})
	}
}

fn parse_rest_of_function_signature(
	tokens: &mut Tokens,
) -> Result<(Vec<Parameter>, ValueType, Option<Location>), Error>
{
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

	consume(Token::ParenRight, tokens)?;

	if let Some(Token::Arrow) = peek(tokens)
	{
		tokens.pop_front();

		let start = tokens.start_location_span();
		let return_type = parse_wellformed_type(tokens)?;
		let location = tokens.location_of_span(start);
		Ok((parameters, return_type, Some(location)))
	}
	else
	{
		Ok((parameters, ValueType::Void, None))
	}
}

fn parse_member(tokens: &mut Tokens) -> Result<Member, Error>
{
	let name = extract_identifier("Expected member name.", tokens)?;

	let start = tokens.start_location_span();
	let value_type = if let Some(Token::Colon) = peek(tokens)
	{
		tokens.pop_front();
		let value_type = parse_wellformed_type(tokens)?;
		Ok(value_type)
	}
	else
	{
		Err(Error::MissingMemberType {
			location: name.location.clone(),
		})
	};
	let value_type = value_type.map_err(|e| e.into());
	let location_of_type = tokens.location_of_span(start);

	Ok(Member {
		name: Ok(name),
		value_type,
		location_of_type,
	})
}

fn parse_parameter(tokens: &mut Tokens) -> Result<Parameter, Error>
{
	let name = extract_identifier("Expected parameter name.", tokens)?;

	let start = tokens.start_location_span();
	let value_type = if let Some(Token::Colon) = peek(tokens)
	{
		tokens.pop_front();
		let value_type = parse_wellformed_type(tokens)?;
		Ok(value_type)
	}
	else
	{
		Err(Error::MissingParameterType {
			location: name.location.clone(),
		})
	};
	let value_type = value_type.map_err(|e| e.into());
	let location_of_type = tokens.location_of_span(start);

	Ok(Parameter {
		name: Ok(name),
		value_type,
		location_of_type,
	})
}

fn parse_wellformed_type(tokens: &mut Tokens) -> Result<ValueType, Error>
{
	let start = tokens.start_location_span();
	let value_type = parse_inner_type(tokens)?;
	if value_type.is_wellformed()
	{
		Ok(value_type)
	}
	else
	{
		let location = tokens.location_of_span(start);
		Err(Error::IllegalType {
			value_type,
			location,
		})
	}
}

fn parse_inner_type(tokens: &mut Tokens) -> Result<ValueType, Error>
{
	let (token, location) = extract("Expected type keyword.", tokens)?;
	match token
	{
		Token::Type(value_type) => Ok(value_type),
		Token::Identifier(name) =>
		{
			let identifier = Identifier {
				name,
				location,
				resolution_id: 0,
				is_authoritative: false,
			};
			Ok(ValueType::UnresolvedStructOrWord {
				identifier: Some(identifier),
			})
		}
		Token::Ampersand =>
		{
			let deref_type = parse_inner_type(tokens)?;
			Ok(ValueType::Pointer {
				deref_type: Box::new(deref_type),
			})
		}
		Token::ParenLeft =>
		{
			let deref_type = parse_inner_type(tokens)?;
			consume(Token::ParenRight, tokens)?;
			Ok(ValueType::View {
				deref_type: Box::new(deref_type),
			})
		}
		Token::BracketLeft => match peek(tokens)
		{
			Some(Token::Colon) =>
			{
				tokens.pop_front();
				consume(Token::BracketRight, tokens)?;
				let element_type = parse_inner_type(tokens)?;
				Ok(ValueType::Slice {
					element_type: Box::new(element_type),
				})
			}
			Some(Token::Dot) =>
			{
				tokens.pop_front();
				consume(Token::Dot, tokens)?;
				consume(Token::Dot, tokens)?;
				consume(Token::BracketRight, tokens)?;
				let element_type = parse_inner_type(tokens)?;
				Ok(ValueType::EndlessArray {
					element_type: Box::new(element_type),
				})
			}
			Some(Token::BracketRight) | None =>
			{
				consume(Token::BracketRight, tokens)?;
				let element_type = parse_inner_type(tokens)?;
				Ok(ValueType::Arraylike {
					element_type: Box::new(element_type),
				})
			}
			Some(Token::NakedDecimal(x)) =>
			{
				let length = *x as usize;
				tokens.pop_front();
				consume(Token::BracketRight, tokens)?;
				let element_type = parse_inner_type(tokens)?;
				Ok(ValueType::Array {
					element_type: Box::new(element_type),
					length,
				})
			}
			Some(_) =>
			{
				let named_length = extract_identifier(
					"Expected size literal or named constant.",
					tokens,
				)?;
				consume(Token::BracketRight, tokens)?;
				let element_type = parse_inner_type(tokens)?;
				Ok(ValueType::ArrayWithNamedLength {
					element_type: Box::new(element_type),
					named_length,
				})
			}
		},
		_ => Err(Error::UnexpectedToken {
			expectation: "Expected type keyword.".to_string(),
			location,
		}),
	}
}

fn parse_function_body(
	function_name: &str,
	tokens: &mut Tokens,
) -> Poisonable<FunctionBody>
{
	consume(Token::BraceLeft, tokens)?;

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
					is_authoritative: false,
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
		if is_return
		{
			if let Some(Token::BraceRight) = peek(tokens)
			{
				tokens.pop_front();
				let return_value_location = tokens.last_location.clone();
				let return_statement_location = statement.location().clone();
				statements.push(statement);
				let error = Error::MissingReturnValueAfterStatement {
					location: return_value_location.clone(),
					after: return_statement_location,
				};
				let return_value = Expression::Poison(Poison::Error(error));
				let body = FunctionBody {
					statements,
					return_value: Some(return_value),
					return_value_identifier: Identifier {
						name: function_name.to_string(),
						location: return_value_location,
						resolution_id: 0,
						is_authoritative: false,
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
				consume(Token::BraceRight, tokens)?;
				let return_value_location = return_value.location().clone();
				let body = FunctionBody {
					statements,
					return_value: Some(return_value),
					return_value_identifier: Identifier {
						name: function_name.to_string(),
						location: return_value_location,
						resolution_id: 0,
						is_authoritative: false,
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
	let (token, location) = extract("Expected statement.", tokens)?;

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
			let condition = {
				// Parse the comparison while preventing peek() lookahead from
				// seeing the opening brace, as this should be parsed by the
				// "then" statement instead.
				let mut tokens = tokens.with_reservation(Token::BraceLeft);
				parse_comparison(tokens.as_mut())?
			};
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
			consume(Token::Semicolon, tokens)?;
			Ok(statement)
		}
		Token::Goto =>
		{
			let label = extract_identifier("Expected label name.", tokens)?;
			let statement = Statement::Goto { label, location };
			consume(Token::Semicolon, tokens)?;
			Ok(statement)
		}
		Token::Var =>
		{
			let name = extract_identifier("Expected variable name.", tokens)?;

			let value_type = if let Some(Token::Colon) = peek(tokens)
			{
				tokens.pop_front();
				let value_type = parse_wellformed_type(tokens)?;
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
			consume(Token::Semicolon, tokens)?;
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
						is_authoritative: false,
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
					is_authoritative: false,
				};
				let arguments = parse_arguments(tokens)?;
				let statement = Statement::MethodCall {
					name: identifier,
					builtin: None,
					arguments,
				};
				consume(Token::Semicolon, tokens)?;
				return Ok(statement);
			}

			let reference =
				parse_rest_of_reference(x, location.clone(), tokens)?;

			if let Some(Token::Semicolon) = peek(tokens)
			{
				tokens.pop_front();
				return Err(Error::UnexpectedSemicolonAfterIdentifier {
					location: tokens.last_location.clone(),
					after: reference.location,
				});
			}
			consume(Token::Assignment, tokens)?;
			let expression = parse_expression(tokens)?;

			let statement = Statement::Assignment {
				reference,
				value: expression,
				location,
			};
			consume(Token::Semicolon, tokens)?;
			Ok(statement)
		}
		Token::Builtin(name) =>
		{
			let (name, builtin) = find_builtin(name);
			let identifier = Identifier {
				name,
				location,
				resolution_id: 0,
				is_authoritative: builtin.is_some(),
			};
			let arguments = parse_arguments(tokens)?;
			let statement = Statement::MethodCall {
				name: identifier,
				builtin,
				arguments,
			};
			consume(Token::Semicolon, tokens)?;
			return Ok(statement);
		}
		Token::Ampersand =>
		{
			let assignment_location = location.clone();
			let reference = parse_addressed_reference(location, tokens)?;

			consume(Token::Assignment, tokens)?;
			let expression = parse_expression(tokens)?;

			let statement = Statement::Assignment {
				reference,
				value: expression,
				location: assignment_location,
			};
			consume(Token::Semicolon, tokens)?;
			Ok(statement)
		}
		_ => Err(Error::UnexpectedToken {
			location,
			expectation: "Expected statement.".to_string(),
		}),
	}
}

fn parse_comparison(tokens: &mut Tokens) -> Result<Comparison, Error>
{
	let left = parse_expression(tokens)?;

	let (token, location_of_op) =
		extract("Expected comparison operator.", tokens)?;
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
				expectation: "Expected comparison operator.".to_string(),
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
		extract("Expected bitwise operator.", tokens)?;
	let op = match op_token
	{
		Token::Ampersand => BinaryOp::BitwiseAnd,
		Token::Pipe => BinaryOp::BitwiseOr,
		Token::Caret => BinaryOp::BitwiseXor,
		_ =>
		{
			return Err(Error::UnexpectedToken {
				location: location_of_op,
				expectation: "Expected bitwise operator.".to_string(),
			});
		}
	};

	// Do not a bitwise expression after an unparenthesized binary expression.
	match expression
	{
		Expression::Binary { .. } =>
		{
			return Err(Error::UnexpectedToken {
				location: location_of_op,
				expectation: "This operator is not allowed here.".to_string(),
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
		extract("Expected bitshift operator.", tokens)?;
	let op = match op_token
	{
		Token::ShiftLeft => BinaryOp::ShiftLeft,
		Token::ShiftRight => BinaryOp::ShiftRight,
		_ =>
		{
			return Err(Error::UnexpectedToken {
				location: location_of_op,
				expectation: "Expected bitshift operator.".to_string(),
			});
		}
	};

	// Do not a bitshift expression after an unparenthesized binary expression.
	match expression
	{
		Expression::Binary { .. } =>
		{
			return Err(Error::UnexpectedToken {
				location: location_of_op,
				expectation: "This operator is not allowed here.".to_string(),
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
	let location_of_bitcast = if let Some(Token::Cast) = peek(tokens)
	{
		let start = tokens.start_location_span();
		tokens.pop_front();
		Some(tokens.location_of_span(start))
	}
	else
	{
		None
	};

	let mut expression = parse_unary_expression(tokens)?;
	let mut location = expression.location().clone();

	if let Some(location_of_keyword) = location_of_bitcast
	{
		location = location.combined_with(&location_of_keyword);
		expression = Expression::BitCast {
			expression: Box::new(expression),
			coerced_type: None,
			location: location.clone(),
			location_of_keyword,
		}
	}

	loop
	{
		if let Some(Token::As) = peek(tokens)
		{
			tokens.pop_front();
		}
		else
		{
			return Ok(expression);
		}

		let start = tokens.start_location_span();
		let coerced_type = parse_wellformed_type(tokens)?;
		let location_of_type = tokens.location_of_span(start);
		location = location.combined_with(&location_of_type);

		expression = Expression::TypeCast {
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
		Some(Token::PipeForType) =>
		{
			let start = tokens.start_location_span();
			tokens.pop_front();
			let queried_type = parse_wellformed_type(tokens)?;
			consume(Token::Pipe, tokens)?;
			let location = tokens.location_of_span(start);
			let expression = Expression::SizeOf {
				queried_type,
				location,
			};
			Ok(expression)
		}
		Some(Token::Pipe) =>
		{
			let start = tokens.start_location_span();
			tokens.pop_front();
			let reference = parse_reference(tokens)?;
			consume(Token::Pipe, tokens)?;
			let location = tokens.location_of_span(start);
			let expression = Expression::LengthOfArray {
				reference,
				location,
			};
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
				Expression::SignedIntegerLiteral {
					value,
					value_type,
					location: _,
				} if value > 0 =>
				{
					// All values 1..=2^(N-1) are valid as negatives.
					Ok(Expression::SignedIntegerLiteral {
						value: -value,
						value_type,
						location,
					})
				}
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
	let (token, location) = extract("Expected literal or identifier.", tokens)?;
	match token
	{
		Token::NakedDecimal(value) if value <= i128::MAX as u128 =>
		{
			Ok(Expression::SignedIntegerLiteral {
				value: value as i128,
				value_type: None,
				location,
			})
		}
		Token::NakedDecimal(value) => Ok(Expression::BitIntegerLiteral {
			value,
			value_type: None,
			location,
		}),
		Token::BitInteger(value) => Ok(Expression::BitIntegerLiteral {
			value,
			value_type: None,
			location,
		}),
		Token::SuffixedInteger { value, suffix_type } =>
		{
			if suffix_type.is_signed() && value <= i128::MAX as u128
			{
				Ok(Expression::SignedIntegerLiteral {
					value: value as i128,
					value_type: Some(Ok(suffix_type)),
					location,
				})
			}
			else
			{
				Ok(Expression::BitIntegerLiteral {
					value,
					value_type: Some(Ok(suffix_type)),
					location,
				})
			}
		}
		Token::CharLiteral(value) => Ok(Expression::BitIntegerLiteral {
			value: u128::from(value),
			value_type: Some(Ok(ValueType::Char8)),
			location,
		}),
		Token::Bool(value) =>
		{
			Ok(Expression::BooleanLiteral { value, location })
		}
		Token::StringLiteral { bytes } =>
		{
			let mut bytes = bytes;
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
					} =>
					{
						bytes.append(&mut extra_bytes);
					}
					_ => unreachable!(),
				}
				location = location.combined_with(&extra_location);
			}
			Ok(Expression::StringLiteral { bytes, location })
		}
		Token::Identifier(name) =>
		{
			if let Some(Token::ParenLeft) = peek(tokens)
			{
				let identifier = Identifier {
					name,
					location,
					resolution_id: 0,
					is_authoritative: false,
				};
				let arguments = parse_arguments(tokens)?;
				Ok(Expression::FunctionCall {
					name: identifier,
					builtin: None,
					arguments,
					return_type: None,
				})
			}
			else if let Some(Token::BraceLeft) = peek(tokens)
			{
				let identifier = Identifier {
					name,
					location: location.clone(),
					resolution_id: 0,
					is_authoritative: false,
				};
				let structural_type = Ok(ValueType::UnresolvedStructOrWord {
					identifier: Some(identifier),
				});
				let members = parse_body_of_structural(tokens)?;
				let location = location.combined_with(&tokens.last_location);
				Ok(Expression::Structural {
					structural_type,
					members,
					location,
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
		Token::Builtin(name) =>
		{
			let (name, builtin) = find_builtin(name);
			let identifier = Identifier {
				name,
				location,
				resolution_id: 0,
				is_authoritative: builtin.is_some(),
			};
			let arguments = parse_arguments(tokens)?;
			Ok(Expression::FunctionCall {
				name: identifier,
				builtin,
				arguments,
				return_type: None,
			})
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
		Token::ParenLeft =>
		{
			let inner = parse_expression(tokens)?;
			consume(Token::ParenRight, tokens)?;
			let location = location.combined_with(&tokens.last_location);
			let expression = Expression::Parenthesized {
				inner: Box::new(inner),
				location,
			};
			Ok(expression)
		}
		_ => Err(Error::UnexpectedToken {
			location,
			expectation: "Expected literal or identifier.".to_string(),
		}),
	}
}

fn parse_arguments(tokens: &mut Tokens) -> Result<Vec<Expression>, Error>
{
	consume(Token::ParenLeft, tokens)?;

	let mut arguments = Vec::new();

	loop
	{
		if let Some(Token::ParenRight) = peek(tokens)
		{
			break;
		}

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

	consume(Token::ParenRight, tokens)?;

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

	consume(Token::BracketRight, tokens)?;
	array.location = array.location.combined_with(&tokens.last_location);

	Ok(array)
}

fn parse_body_of_structural(
	tokens: &mut Tokens,
) -> Result<Vec<MemberExpression>, Error>
{
	consume(Token::BraceLeft, tokens)?;

	let mut members = Vec::new();
	loop
	{
		if let Some(Token::BraceRight) = peek(tokens)
		{
			break;
		}

		let name = extract_identifier("Expected member name.", tokens)?;

		let expression = if let Some(Token::Colon) = peek(tokens)
		{
			tokens.pop_front();

			let expression = parse_expression(tokens)?;
			expression
		}
		else
		{
			// Field Init Shorthand
			let reference = Reference {
				base: Ok(name.clone()),
				steps: Vec::new(),
				address_depth: 0,
				location: name.location.clone(),
				location_of_unaddressed: name.location.clone(),
			};
			Expression::Deref {
				reference,
				deref_type: None,
			}
		};

		members.push(MemberExpression {
			name: Ok(name),
			offset: None,
			expression,
		});

		if let Some(Token::Comma) = peek(tokens)
		{
			tokens.pop_front();
		}
		else
		{
			break;
		}
	}

	consume(Token::BraceRight, tokens)?;
	Ok(members)
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
		location_of_unaddressed,
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
		location: location.combined_with(&tokens.last_location),
		location_of_unaddressed,
	};
	Ok(reference)
}

fn parse_reference(tokens: &mut Tokens) -> Result<Reference, Error>
{
	let (token, location) = extract("Expected identifier.", tokens)?;
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
				location: location.combined_with(&tokens.last_location),
				location_of_unaddressed: reference.location_of_unaddressed,
			})
		}
		Token::Identifier(name) =>
		{
			parse_rest_of_reference(name, location, tokens)
		}
		_ => Err(Error::UnexpectedToken {
			location,
			expectation: "Expected identifier.".to_string(),
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
		is_authoritative: false,
	};
	let mut steps = Vec::new();
	let mut location = location;
	loop
	{
		let step = match peek(tokens)
		{
			Some(Token::BracketLeft) =>
			{
				tokens.pop_front();
				let argument = parse_expression(tokens)?;
				consume(Token::BracketRight, tokens)?;
				ReferenceStep::Element {
					argument: Box::new(argument),
					is_endless: None,
				}
			}
			Some(Token::Dot) =>
			{
				tokens.pop_front();
				let member =
					extract_identifier("Expected member name.", tokens)?;
				ReferenceStep::Member {
					member,
					offset: None,
				}
			}
			_ => break,
		};
		steps.push(step);
		location = location.combined_with(&tokens.last_location);
		if steps.len() > MAX_REFERENCE_DEPTH
		{
			return Err(Error::MaximumParseDepthExceeded { location });
		}
	}
	let location_of_unaddressed = location.clone();
	Ok(Reference {
		base: Ok(base),
		steps,
		address_depth: 0,
		location,
		location_of_unaddressed,
	})
}

fn find_builtin(mut name: String) -> (String, Option<Builtin>)
{
	let builtin = serde_plain::from_str(&name).ok();
	if builtin.is_none()
	{
		name.push_str("!");
	}
	(name, builtin)
}
