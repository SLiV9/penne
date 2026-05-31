pub mod parse_node;
pub mod parse_tree;

use enumset::EnumSet;

use crate::alpha::common::BinaryOp;
use crate::alpha::common::ComparisonOp;
use crate::alpha::common::DeclarationFlag;
use crate::alpha::common::UnaryOp;
use crate::delta::lexer;
use crate::delta::lexer::BaseToken;
use crate::delta::lexer::ValueTypeKeyword;
use crate::delta::lexer::tokens;
use crate::delta::lexer::tokens::Span;
use crate::delta::lexer::tokens::Tokens;
use crate::delta::parser::parse_node::NodeId;
use crate::delta::parser::parse_node::ParseNode;
use crate::delta::parser::parse_tree::ParseBuffer;
use crate::delta::parser::parse_tree::ParseTree;

pub const MAX_ADDRESS_DEPTH: u8 = 127;
pub const MAX_REFERENCE_DEPTH: usize = 127;

#[derive(Debug, Clone, Copy)]
pub enum ParsingError
{
	UnexpectedToken
	{
		token: tokens::TokenId,
		expectation: &'static str,
	},
	UnexpectedSemicolonAfterIdentifier
	{
		semicolon: tokens::TokenId,
		identifier: tokens::TokenId,
	},
	UnexpectedSemicolonAfterReturnValue
	{
		semicolon: tokens::TokenId,
		return_value_start: tokens::TokenId,
		return_value_end: tokens::TokenId,
	},
	MissingReturnValueAfterStatement
	{
		unexpected_token: tokens::TokenId,
		return_statement_start: tokens::TokenId,
		return_statement_end: tokens::TokenId,
	},
	MissingConstantType
	{
		unexpected_token: tokens::TokenId
	},
	MissingParameterType
	{
		unexpected_token: tokens::TokenId
	},
	MissingMemberType
	{
		unexpected_token: tokens::TokenId
	},
	MaximumParseDepthExceeded
	{
		start: tokens::TokenId,
		end: tokens::TokenId,
	},
}

// struct Tokens<'a>
// {
// 	tokens: &'a lexer::tokens::Tokens,
// 	reserved_token: Option<BaseToken>,
// }

// impl<'a> From<&'a lexer::tokens::Tokens> for Tokens<'a>
// {
// 	fn from(tokens: &'a lexer::tokens::Tokens) -> Self
// 	{
// 		Self {
// 			tokens,
// 			reserved_token: None,
// 		}
// 	}
// }

pub fn parse(tokens: &lexer::tokens::Tokens) -> ParseTree
{
	// let mut tokens = Tokens::from(tokens);
	let num_declarations = tokens
		.base_tokens()
		.iter()
		.filter(|&&token| is_actual_declaration_keyword(token))
		.count();
	let mut parse_tree = ParseTree::empty(tokens, num_declarations);

	let mut buffer = parse_tree.buffer();
	let mut start_of_next_declaration = tokens.first_token_id();
	for _ in 0..(num_declarations + 1)
	{
		match tokens.get(start_of_next_declaration)
		{
			BaseToken::EndOfSource => break,
			_ => (),
		}
		let mut span = tokens.span_from(start_of_next_declaration);
		match parse_declaration(tokens, &mut buffer, &mut span)
		{
			Ok(node) => buffer.finish_declaration(node),
			Err(error) => buffer.store_error(error),
		}
		start_of_next_declaration =
			tokens.skip_until(starts_declaration, span.start);
	}
	assert_eq!(
		tokens.get(start_of_next_declaration),
		BaseToken::EndOfSource
	);
	// Safety: we are calling `set_nodes_len` with the result of
	// `into_num_initialized_nodes`.
	unsafe {
		let num_tokens = buffer.into_num_initialized_nodes();
		parse_tree.set_nodes_len(num_tokens);
	}

	parse_tree
}

fn is_actual_declaration_keyword(token: BaseToken) -> bool
{
	match token
	{
		BaseToken::Import => true,
		BaseToken::Const => true,
		BaseToken::Fn => true,
		BaseToken::Struct => true,
		BaseToken::Word8 => true,
		BaseToken::Word16 => true,
		BaseToken::Word32 => true,
		BaseToken::Word64 => true,
		BaseToken::Word128 => true,
		_ => false,
	}
}

fn starts_declaration(token: BaseToken) -> bool
{
	match token
	{
		BaseToken::Pub => true,
		BaseToken::Extern => true,
		_ => is_actual_declaration_keyword(token),
	}
}

fn parse_declaration(
	tokens: &Tokens,
	buffer: &mut ParseBuffer<'_>,
	span: &mut Span,
) -> Result<NodeId, ParsingError>
{
	let start_of_declaration = span.start.into();
	let mut flags = EnumSet::new();
	dbg!(peek(tokens, span));
	if consume_optional(BaseToken::Pub, tokens, span)
	{
		flags.insert(DeclarationFlag::Public);
		buffer.set_public();
	}
	else
	{
		buffer.set_private();
	}
	if consume_optional(BaseToken::Extern, tokens, span)
	{
		flags.insert(DeclarationFlag::External);
	}
	let declaring_token_id = span.start;
	let declaring_token = take(tokens, span);
	match declaring_token
	{
		BaseToken::Import => parse_import_declaration(
			tokens,
			buffer,
			span,
			start_of_declaration,
			flags,
		),
		BaseToken::Const => parse_constant_declaration(
			tokens,
			buffer,
			span,
			start_of_declaration,
			flags,
		),
		BaseToken::Fn => parse_function_declaration(
			tokens,
			buffer,
			span,
			flags,
			start_of_declaration,
		),
		BaseToken::Struct => parse_struct_declaration(
			tokens,
			buffer,
			span,
			start_of_declaration,
			flags,
		),
		BaseToken::Word8
		| BaseToken::Word16
		| BaseToken::Word32
		| BaseToken::Word64
		| BaseToken::Word128 => parse_word_declaration(
			tokens,
			buffer,
			span,
			start_of_declaration,
			flags,
			declaring_token,
		),
		_ => Err(ParsingError::UnexpectedToken {
			token: declaring_token_id,
			expectation: "Expected top-level declaration.",
		}),
	}
}

fn parse_import_declaration(
	tokens: &Tokens,
	buffer: &mut ParseBuffer<'_>,
	span: &mut Span,
	start_of_declaration: parse_node::TokenId,
	flags: EnumSet<DeclarationFlag>,
) -> Result<NodeId, ParsingError>
{
	let literal = span.start.into();
	consume(BaseToken::StringLiteral, tokens, span)?;
	consume(BaseToken::Semicolon, tokens, span)?;
	buffer.push_undeclared(ParseNode::SimpleStringLiteral { literal });
	buffer.push_undeclared(ParseNode::DeclarationFlags(flags));
	let node = buffer.push(ParseNode::ImportDeclaration {
		start_of_declaration,
	});
	Ok(node)
}

fn parse_constant_declaration(
	tokens: &Tokens,
	buffer: &mut ParseBuffer<'_>,
	span: &mut Span,
	start_of_declaration: parse_node::TokenId,
	flags: EnumSet<DeclarationFlag>,
) -> Result<NodeId, ParsingError>
{
	let identifier = span.start.into();
	consume(BaseToken::Identifier, tokens, span)?;
	let location_of_colon = span.start;
	let value_type = if consume_optional(BaseToken::Colon, tokens, span)
	{
		parse_type(tokens, buffer, span)?
	}
	else
	{
		return Err(ParsingError::MissingConstantType {
			unexpected_token: location_of_colon,
		});
	};
	consume(BaseToken::Assignment, tokens, span)?;
	let expression = parse_expression(tokens, buffer, span)?;
	consume(BaseToken::Semicolon, tokens, span)?;
	buffer.expect_most_recent_node(expression);
	buffer.push_older_node(value_type);
	buffer.push_undeclared(ParseNode::Identifier { identifier });
	buffer.push_undeclared(ParseNode::DeclarationFlags(flags));
	let node = buffer.push(ParseNode::ConstantDeclaration {
		start_of_declaration,
	});
	Ok(node)
}

fn parse_word_declaration(
	tokens: &Tokens,
	buffer: &mut ParseBuffer<'_>,
	span: &mut Span,
	start_of_declaration: parse_node::TokenId,
	flags: EnumSet<DeclarationFlag>,
	declaring_token: BaseToken,
) -> Result<NodeId, ParsingError>
{
	let size_in_bytes = match declaring_token
	{
		BaseToken::Word8 => 1,
		BaseToken::Word16 => 2,
		BaseToken::Word32 => 4,
		BaseToken::Word64 => 8,
		BaseToken::Word128 => 16,
		_ => unreachable!(),
	};
	let structural_type = ParseNode::StructuralType {
		size_in_bytes_if_word: Some(size_in_bytes),
	};

	let identifier = span.start.into();
	consume(BaseToken::Identifier, tokens, span)?;
	let list = parse_struct_members(tokens, buffer, span)?;
	buffer.push_list(list);
	buffer.push_undeclared(structural_type);
	buffer.push_undeclared(ParseNode::Identifier { identifier });
	buffer.push_undeclared(ParseNode::DeclarationFlags(flags));
	let node = buffer.push(ParseNode::StructureDeclaration {
		start_of_declaration,
	});
	Ok(node)
}

fn parse_struct_declaration(
	tokens: &Tokens,
	buffer: &mut ParseBuffer<'_>,
	span: &mut Span,
	start_of_declaration: parse_node::TokenId,
	mut flags: EnumSet<DeclarationFlag>,
) -> Result<NodeId, ParsingError>
{
	let structural_type = ParseNode::StructuralType {
		size_in_bytes_if_word: None,
	};

	let identifier = span.start.into();
	consume(BaseToken::Identifier, tokens, span)?;
	let list: NodeId = if consume_optional(BaseToken::Semicolon, tokens, span)
	{
		flags.insert(DeclarationFlag::OpaqueStruct);
		buffer.push(ParseNode::NoMoreItems)
	}
	else
	{
		let list = parse_struct_members(tokens, buffer, span)?;
		list
	};
	buffer.push_list(list);
	buffer.push_undeclared(structural_type);
	buffer.push_undeclared(ParseNode::Identifier { identifier });
	buffer.push_undeclared(ParseNode::DeclarationFlags(flags));
	let node = buffer.push(ParseNode::StructureDeclaration {
		start_of_declaration,
	});
	Ok(node)
}

fn parse_struct_members(
	tokens: &Tokens,
	buffer: &mut ParseBuffer<'_>,
	span: &mut Span,
) -> Result<NodeId, ParsingError>
{
	consume(BaseToken::BraceLeft, tokens, span)?;
	buffer.start_list();
	while !consume_optional(BaseToken::BraceRight, tokens, span)
	{
		let member = parse_member(tokens, buffer, span)?;
		consume(BaseToken::Comma, tokens, span)?;
		buffer.push_list_item(member);
	}
	let start_of_list = buffer.push_end_of_list();
	Ok(start_of_list)
}

fn parse_function_declaration(
	tokens: &Tokens,
	buffer: &mut ParseBuffer<'_>,
	span: &mut Span,
	flags: EnumSet<DeclarationFlag>,
	start_of_declaration: parse_node::TokenId,
) -> Result<NodeId, ParsingError>
{
	let identifier = span.start.into();
	consume(BaseToken::Identifier, tokens, span)?;
	let (parameters, return_type) =
		parse_rest_of_function_signature(tokens, buffer, span)?;

	let node = if consume_optional(BaseToken::Semicolon, tokens, span)
	{
		// This is to allow layout compatibility with Function.
		buffer.push_undeclared(ParseNode::Padding);
		buffer.push_undeclared(ParseNode::Padding);
		buffer.push_older_node(return_type);
		buffer.push_list(parameters);
		buffer.push_undeclared(ParseNode::Identifier { identifier });
		buffer.push_undeclared(ParseNode::DeclarationFlags(flags));
		buffer.push(ParseNode::FunctionHeadDeclaration {
			start_of_declaration,
		})
	}
	else
	{
		// When the private zone gets expunged, two padding blocks
		// shift into the position of the return value and statements.
		if flags.contains(DeclarationFlag::Public)
		{
			buffer.set_private();
		}
		let (statements, return_value) =
			parse_function_body(tokens, buffer, span)?;
		// Note that we do not use buffer.expect_most_recent_node() here.
		// This is to allow layout compatibility with FunctionHead.
		buffer.push_older_node(return_value);
		buffer.push_list(statements);
		if flags.contains(DeclarationFlag::Public)
		{
			buffer.set_public();
		}
		buffer.push_older_node(return_type);
		buffer.push_list(parameters);
		buffer.push_undeclared(ParseNode::Identifier { identifier });
		buffer.push_undeclared(ParseNode::DeclarationFlags(flags));
		buffer.push(ParseNode::FunctionDeclaration {
			start_of_declaration,
		})
	};
	Ok(node)
}

fn parse_rest_of_function_signature(
	tokens: &Tokens,
	buffer: &mut ParseBuffer<'_>,
	span: &mut Span,
) -> Result<(NodeId, NodeId), ParsingError>
{
	consume(BaseToken::ParenLeft, tokens, span)?;
	buffer.start_list();
	loop
	{
		if consume_optional(BaseToken::ParenRight, tokens, span)
		{
			break;
		}
		let parameter = parse_parameter(tokens, buffer, span)?;
		buffer.push_list_item(parameter);
		if consume_optional(BaseToken::Comma, tokens, span)
		{
			continue;
		}
		else
		{
			consume(BaseToken::ParenRight, tokens, span)?;
			break;
		}
	}
	let parameters = buffer.push_end_of_list();

	let return_type = if consume_optional(BaseToken::Arrow, tokens, span)
	{
		parse_type(tokens, buffer, span)?
	}
	else
	{
		buffer.push(ParseNode::SimpleValueType(ValueTypeKeyword::Void))
	};
	Ok((parameters, return_type))
}

fn parse_member(
	tokens: &Tokens,
	buffer: &mut ParseBuffer<'_>,
	span: &mut Span,
) -> Result<NodeId, ParsingError>
{
	let identifier = span.start.into();
	consume(BaseToken::Identifier, tokens, span)?;
	let location_of_colon = span.start;
	let value_type = if consume_optional(BaseToken::Colon, tokens, span)
	{
		parse_type(tokens, buffer, span)?
	}
	else
	{
		return Err(ParsingError::MissingMemberType {
			unexpected_token: location_of_colon,
		});
	};
	buffer.expect_most_recent_node(value_type);
	let node = buffer.push(ParseNode::IdentifierAndVT { identifier });
	Ok(node)
}

fn parse_parameter(
	tokens: &Tokens,
	buffer: &mut ParseBuffer<'_>,
	span: &mut Span,
) -> Result<NodeId, ParsingError>
{
	let identifier = span.start.into();
	consume(BaseToken::Identifier, tokens, span)?;
	let location_of_colon = span.start;
	let value_type = if consume_optional(BaseToken::Colon, tokens, span)
	{
		parse_type(tokens, buffer, span)?
	}
	else
	{
		return Err(ParsingError::MissingParameterType {
			unexpected_token: location_of_colon,
		});
	};
	buffer.expect_most_recent_node(value_type);
	let node = buffer.push(ParseNode::IdentifierAndVT { identifier });
	Ok(node)
}

fn parse_type(
	tokens: &Tokens,
	buffer: &mut ParseBuffer<'_>,
	span: &mut Span,
) -> Result<NodeId, ParsingError>
{
	let vt_start = span.start;
	let value_type = parse_inner_type(tokens, buffer, span)?;
	let vt_end = span.start;
	let node = if tokens.spans_multiple_tokens(vt_start..vt_end)
	{
		buffer.expect_most_recent_node(value_type);
		buffer.push_undeclared(ParseNode::EndOfSpan { end: vt_end.into() });
		buffer.push(ParseNode::CompositeValueType {
			start: vt_start.into(),
		})
	}
	else
	{
		value_type
	};
	Ok(node)
}

fn parse_inner_type(
	tokens: &Tokens,
	buffer: &mut ParseBuffer<'_>,
	span: &mut Span,
) -> Result<NodeId, ParsingError>
{
	let token_id = span.start;
	let node_id = match take(tokens, span)
	{
		BaseToken::ValueTypeKeyword =>
		{
			let vap = tokens.get_value_type_and_payload(token_id);
			let value_type = vap.value_type();
			buffer.push(ParseNode::SimpleValueType(value_type))
		}
		BaseToken::Identifier =>
		{
			let identifier = token_id.into();
			buffer.push(ParseNode::UnresolvedStructOrWordVT { identifier })
		}
		BaseToken::Ampersand =>
		{
			let inner = parse_inner_type(tokens, buffer, span)?;
			buffer.expect_most_recent_node(inner);
			buffer.push(ParseNode::PointerVT {})
		}
		BaseToken::ParenLeft =>
		{
			let inner = parse_inner_type(tokens, buffer, span)?;
			consume(BaseToken::ParenRight, tokens, span)?;
			buffer.expect_most_recent_node(inner);
			buffer.push(ParseNode::ViewVT {})
		}
		BaseToken::BracketLeft =>
		{
			// TODO if my types are outward-in, should I store them that way in the parse tree?
			let length_token_id = span.start;
			let length_or_bracket = take(tokens, span);
			let outer_node = match length_or_bracket
			{
				BaseToken::BracketRight => ParseNode::ArraylikeVT {},
				BaseToken::Colon => ParseNode::SliceVT {},
				BaseToken::Dots => ParseNode::EndlessArrayVT {},
				BaseToken::NakedDecimal => ParseNode::ArrayVT {
					fixed_length: length_token_id.into(),
				},
				BaseToken::Identifier => ParseNode::ArrayWithNamedLengthVT {
					named_length_identifier: length_token_id.into(),
				},
				_ =>
				{
					return Err(ParsingError::UnexpectedToken {
						token: length_token_id,
						expectation: "Expected size literal or named constant.",
					});
				}
			};
			if length_or_bracket != BaseToken::BracketRight
			{
				consume(BaseToken::BracketRight, tokens, span)?;
			}
			let inner = parse_inner_type(tokens, buffer, span)?;
			buffer.expect_most_recent_node(inner);
			buffer.push(outer_node)
		}
		_ =>
		{
			return Err(ParsingError::UnexpectedToken {
				token: token_id,
				expectation: "Expected type keyword.",
			});
		}
	};
	Ok(node_id)
}

fn parse_function_body(
	tokens: &Tokens,
	buffer: &mut ParseBuffer<'_>,
	span: &mut Span,
) -> Result<(NodeId, NodeId), ParsingError>
{
	consume(BaseToken::BraceLeft, tokens, span)?;
	buffer.start_list();
	loop
	{
		if consume_optional(BaseToken::BraceRight, tokens, span)
		{
			let statements = buffer.push_end_of_list();
			let return_value = buffer.push_none();
			return Ok((statements, return_value));
		}
		let _ = take(tokens, span);

		// TODO implement statements
		// let statement = parse_statement(tokens, buffer, span)?;
		// buffer.push_list_item(statement);

		// TODO check if statement is return: and handle that
		// doesn't matter if it is an ugly hack because I want to change the syntax
	}
}

// fn parse_statement(
// 	tokens: &Tokens,
// 	buffer: &mut ParseBuffer<'_>,
// 	span: &mut Span,
// ) -> Result<NodeId, ParsingError>
// {
// 	todo!()
// }

fn parse_expression(
	tokens: &Tokens,
	buffer: &mut ParseBuffer<'_>,
	span: &mut Span,
) -> Result<NodeId, ParsingError>
{
	parse_addition(tokens, buffer, span)
}

fn parse_addition(
	tokens: &Tokens,
	buffer: &mut ParseBuffer<'_>,
	span: &mut Span,
) -> Result<NodeId, ParsingError>
{
	let mut expression = parse_multiplication(tokens, buffer, span)?;

	loop
	{
		let op_token_id = span.start;
		let op = match peek(tokens, span)
		{
			BaseToken::Ampersand | BaseToken::Pipe | BaseToken::Caret =>
			{
				return parse_rest_of_bitwise_expression(
					expression, tokens, buffer, span,
				);
			}
			BaseToken::ShiftLeft | BaseToken::ShiftRight =>
			{
				return parse_rest_of_bitshift_operation(
					expression, tokens, buffer, span,
				);
			}
			BaseToken::Plus => BinaryOp::Add,
			BaseToken::Minus => BinaryOp::Subtract,
			_ => return Ok(expression),
		};
		let _peeked = take(tokens, span);

		let right = parse_multiplication(tokens, buffer, span)?;

		buffer.expect_most_recent_node(right);
		buffer.push_older_node(expression);
		buffer.push_undeclared(ParseNode::BinaryOp(op));
		expression = buffer.push(ParseNode::Binary {
			token: op_token_id.into(),
		});
	}
}

fn parse_rest_of_bitwise_expression(
	mut expression: NodeId,
	tokens: &Tokens,
	buffer: &mut ParseBuffer<'_>,
	span: &mut Span,
) -> Result<NodeId, ParsingError>
{
	let mut op_token_id = span.start;
	let op_token = take(tokens, span);
	let op = match op_token
	{
		BaseToken::Ampersand => BinaryOp::BitwiseAnd,
		BaseToken::Pipe => BinaryOp::BitwiseOr,
		BaseToken::Caret => BinaryOp::BitwiseXor,
		_ =>
		{
			return Err(ParsingError::UnexpectedToken {
				token: op_token_id,
				expectation: "Expected bitwise operator.",
			});
		}
	};

	// Do not a bitwise expression after an unparenthesized binary expression.
	// TODO check if most recent node is binary

	loop
	{
		let right = parse_unary_expression(tokens, buffer, span)?;

		buffer.expect_most_recent_node(right);
		buffer.push_older_node(expression);
		buffer.push_undeclared(ParseNode::BinaryOp(op));
		expression = buffer.push(ParseNode::Binary {
			token: op_token_id.into(),
		});

		op_token_id = span.start;
		if consume_optional(op_token, tokens, span)
		{
			continue;
		}
		else
		{
			return Ok(expression);
		}
	}
}

fn parse_rest_of_bitshift_operation(
	left: NodeId,
	tokens: &Tokens,
	buffer: &mut ParseBuffer<'_>,
	span: &mut Span,
) -> Result<NodeId, ParsingError>
{
	let op_token_id = span.start;
	let op_token = take(tokens, span);
	let op = match op_token
	{
		BaseToken::ShiftLeft => BinaryOp::ShiftLeft,
		BaseToken::ShiftRight => BinaryOp::ShiftRight,
		_ =>
		{
			return Err(ParsingError::UnexpectedToken {
				token: op_token_id,
				expectation: "Expected bitshift operator.",
			});
		}
	};

	// Do not a bitshift expression after an unparenthesized binary expression.
	// TODO check if most recent node is binary

	let right = parse_unary_expression(tokens, buffer, span)?;

	buffer.expect_most_recent_node(right);
	buffer.push_older_node(left);
	buffer.push_undeclared(ParseNode::BinaryOp(op));
	let expression = buffer.push(ParseNode::Binary {
		token: op_token_id.into(),
	});
	Ok(expression)
}

fn parse_multiplication(
	tokens: &Tokens,
	buffer: &mut ParseBuffer<'_>,
	span: &mut Span,
) -> Result<NodeId, ParsingError>
{
	let mut expression = parse_singular_expression(tokens, buffer, span)?;

	loop
	{
		let op_token_id = span.start;
		let op = match peek(tokens, span)
		{
			BaseToken::Times => BinaryOp::Multiply,
			BaseToken::Divide => BinaryOp::Divide,
			BaseToken::Modulo => BinaryOp::Modulo,
			_ => return Ok(expression),
		};
		let _peeked = take(tokens, span);

		let right = parse_singular_expression(tokens, buffer, span)?;

		buffer.expect_most_recent_node(right);
		buffer.push_older_node(expression);
		buffer.push_undeclared(ParseNode::BinaryOp(op));
		expression = buffer.push(ParseNode::Binary {
			token: op_token_id.into(),
		});
	}
}

fn parse_singular_expression(
	tokens: &Tokens,
	buffer: &mut ParseBuffer<'_>,
	span: &mut Span,
) -> Result<NodeId, ParsingError>
{
	let bitcast_token_id = span.start;
	let bitcast = consume_optional(BaseToken::Cast, tokens, span)
		.then_some(bitcast_token_id);

	let mut expression = parse_unary_expression(tokens, buffer, span)?;

	if let Some(bitcast_token_id) = bitcast
	{
		buffer.expect_most_recent_node(expression);
		expression = buffer.push(ParseNode::BitCast {
			cast_keyword: bitcast_token_id.into(),
		});
	}

	while consume_optional(BaseToken::As, tokens, span)
	{
		let start_of_type = span.start.into();
		let coerced_type = parse_type(tokens, buffer, span)?;
		buffer.expect_most_recent_node(coerced_type);
		buffer.push_older_node(expression);
		expression = buffer.push(ParseNode::TypeCast { start_of_type });
	}

	Ok(expression)
}

fn parse_unary_expression(
	tokens: &Tokens,
	buffer: &mut ParseBuffer<'_>,
	span: &mut Span,
) -> Result<NodeId, ParsingError>
{
	match peek(tokens, span)
	{
		BaseToken::PipeForType =>
		{
			let _peeked = take(tokens, span);
			let queried_type = parse_type(tokens, buffer, span)?;
			consume(BaseToken::Pipe, tokens, span)?;
			buffer.expect_most_recent_node(queried_type);
			let expression = buffer.push(ParseNode::SizeOf {});
			Ok(expression)
		}
		BaseToken::Pipe =>
		{
			let _peeked = take(tokens, span);
			let reference = parse_reference(tokens, buffer, span)?;
			consume(BaseToken::Pipe, tokens, span)?;
			buffer.expect_most_recent_node(reference);
			let expression = buffer.push(ParseNode::LengthOf {});
			Ok(expression)
		}
		BaseToken::Exclamation =>
		{
			let op_token_id = span.start;
			let _peeked = take(tokens, span);
			let op = UnaryOp::BitwiseComplement;

			let expression = parse_primary_expression(tokens, buffer, span)?;
			buffer.expect_most_recent_node(expression);
			buffer.push_undeclared(ParseNode::UnaryOp(op));
			let expression = buffer.push(ParseNode::Unary {
				token: op_token_id.into(),
			});
			Ok(expression)
		}
		BaseToken::Minus =>
		{
			let op_token_id = span.start;
			let _peeked = take(tokens, span);
			let op = UnaryOp::Negative;

			// TODO either handle negative integers here
			// or just before I check integer ranges
			// because -128i8 is valid, but 128i8 is not

			let expression = parse_primary_expression(tokens, buffer, span)?;
			buffer.expect_most_recent_node(expression);
			buffer.push_undeclared(ParseNode::UnaryOp(op));
			let expression = buffer.push(ParseNode::Unary {
				token: op_token_id.into(),
			});
			Ok(expression)
		}
		_ => parse_primary_expression(tokens, buffer, span),
	}
}

fn parse_primary_expression(
	tokens: &Tokens,
	buffer: &mut ParseBuffer<'_>,
	span: &mut Span,
) -> Result<NodeId, ParsingError>
{
	let first_token_id = span.start;
	match take(tokens, span)
	{
		BaseToken::NakedDecimal =>
		{
			let expression = buffer.push(ParseNode::UntypedIntegerLiteral {
				literal: first_token_id.into(),
			});
			Ok(expression)
		}
		BaseToken::BitInteger =>
		{
			let expression = buffer.push(ParseNode::UntypedIntegerLiteral {
				literal: first_token_id.into(),
			});
			Ok(expression)
		}
		BaseToken::SuffixedInteger =>
		{
			let vap = tokens.get_value_type_and_payload(first_token_id);
			let value_type = vap.value_type();
			buffer.push_undeclared(ParseNode::SimpleValueType(value_type));
			let expression = buffer.push(ParseNode::TypedIntegerLiteral {
				literal: first_token_id.into(),
			});
			Ok(expression)
		}
		BaseToken::CharLiteral =>
		{
			let expression = buffer.push(ParseNode::CharLiteral {
				literal: first_token_id.into(),
			});
			Ok(expression)
		}
		BaseToken::BoolLiteral =>
		{
			let expression = buffer.push(ParseNode::BooleanLiteral {
				literal: first_token_id.into(),
			});
			Ok(expression)
		}
		BaseToken::StringLiteral =>
		{
			let token = BaseToken::StringLiteral;
			let expression = if consume_optional(token, tokens, span)
			{
				let mut end = span.start.into();
				while consume_optional(token, tokens, span)
				{
					end = span.start.into();
				}
				buffer.push_undeclared(ParseNode::EndOfSpan { end });
				buffer.push(ParseNode::CompositeStringLiteral {
					start: first_token_id.into(),
				})
			}
			else
			{
				buffer.push(ParseNode::SimpleStringLiteral {
					literal: first_token_id.into(),
				})
			};
			Ok(expression)
		}
		BaseToken::Ampersand =>
		{
			let mut depth = 1;
			while consume_optional(BaseToken::Ampersand, tokens, span)
			{
				depth += 1;
				if depth > MAX_ADDRESS_DEPTH
				{
					let end = span.start.into();
					return Err(ParsingError::MaximumParseDepthExceeded {
						start: first_token_id.into(),
						end,
					});
				}
			}
			let identifier = span.start.into();
			let steps = parse_deref_steps_list(tokens, buffer, span)?;
			buffer.push_list(steps);
			buffer.push_undeclared(ParseNode::Identifier { identifier });
			buffer.push_undeclared(ParseNode::DerefAddressDepth { depth });
			let expression = buffer.push(ParseNode::Deref {
				start_of_reference: first_token_id.into(),
			});
			Ok(expression)
		}
		BaseToken::Identifier =>
		{
			// TODO function call
			// TODO structure initialization

			let identifier = first_token_id.into();
			let steps = parse_deref_steps_list(tokens, buffer, span)?;
			buffer.push_list(steps);
			buffer.push_undeclared(ParseNode::Identifier { identifier });
			buffer.push_undeclared(ParseNode::DerefAddressDepth { depth: 0 });
			let expression = buffer.push(ParseNode::Deref {
				start_of_reference: first_token_id.into(),
			});
			Ok(expression)
		}
		BaseToken::Builtin =>
		{
			// TODO
			todo!()
		}
		BaseToken::BracketLeft =>
		{
			let mut num_elements: usize = 0;
			buffer.start_list();
			if !consume_optional(BaseToken::BracketRight, tokens, span)
			{
				loop
				{
					let element = parse_expression(tokens, buffer, span)?;
					buffer.push_list_item(element);
					num_elements += 1;
					if !consume_optional(BaseToken::Comma, tokens, span)
					{
						break;
					}
				}
				consume(BaseToken::BracketRight, tokens, span)?;
			}
			let elements = buffer.push_end_of_list();
			buffer.push_list(elements);
			let num_elements = parse_node::U24::new(num_elements);
			let expression =
				buffer.push(ParseNode::ArrayLiteral { num_elements });
			Ok(expression)
		}
		BaseToken::ParenLeft =>
		{
			let inner = parse_expression(tokens, buffer, span)?;
			consume(BaseToken::ParenRight, tokens, span)?;
			buffer.expect_most_recent_node(inner);
			let expression = buffer.push(ParseNode::Parenthesized {});
			Ok(expression)
		}
		_ => Err(ParsingError::UnexpectedToken {
			token: first_token_id,
			expectation: "Expected literal or identifier.",
		}),
	}
}

fn parse_reference(
	tokens: &Tokens,
	buffer: &mut ParseBuffer<'_>,
	span: &mut Span,
) -> Result<NodeId, ParsingError>
{
	let first_token_id = span.start;
	let mut address_depth = 1;
	while consume_optional(BaseToken::Ampersand, tokens, span)
	{
		address_depth += 1;
		if address_depth > MAX_ADDRESS_DEPTH
		{
			let start = first_token_id;
			let end = span.start;
			return Err(ParsingError::MaximumParseDepthExceeded { start, end });
		}
	}
	let identifier = span.start.into();
	consume(BaseToken::Identifier, tokens, span)?;
	let steps = parse_deref_steps_list(tokens, buffer, span)?;
	buffer.push_list(steps);
	buffer.push_undeclared(ParseNode::Identifier { identifier });
	buffer.push_undeclared(ParseNode::DerefAddressDepth { depth: 0 });
	let expression = buffer.push(ParseNode::Deref {
		start_of_reference: first_token_id.into(),
	});
	Ok(expression)
}

fn parse_deref_steps_list(
	tokens: &Tokens,
	buffer: &mut ParseBuffer<'_>,
	span: &mut Span,
) -> Result<NodeId, ParsingError>
{
	let start = span.start.into();
	buffer.start_list();
	for _ in 0..MAX_REFERENCE_DEPTH
	{
		let step = if consume_optional(BaseToken::BracketLeft, tokens, span)
		{
			let argument = parse_expression(tokens, buffer, span)?;
			consume(BaseToken::BracketRight, tokens, span)?;
			buffer.expect_most_recent_node(argument);
			buffer.push(ParseNode::DerefStepElement {})
		}
		else if consume_optional(BaseToken::Dot, tokens, span)
		{
			let field_identifier = span.start.into();
			consume(BaseToken::Identifier, tokens, span)?;
			buffer.push(ParseNode::DerefStepMember { field_identifier })
		}
		else
		{
			let start_of_list = buffer.push_end_of_list();
			return Ok(start_of_list);
		};
		buffer.push_list_item(step);
	}
	let end = span.start.into();
	Err(ParsingError::MaximumParseDepthExceeded { start, end })
}

#[inline(always)]
fn peek(tokens: &Tokens, span: &mut Span) -> BaseToken
{
	tokens.get(span.start)
}

#[inline(always)]
fn take(tokens: &Tokens, span: &mut Span) -> BaseToken
{
	let token = tokens.get(span.start);
	tokens.advance(&mut span.start);
	token
}

#[inline(always)]
fn consume(
	expected: BaseToken,
	tokens: &Tokens,
	span: &mut Span,
) -> Result<(), ParsingError>
{
	let token_id = span.start;
	let token = tokens.get(span.start);
	tokens.advance(&mut span.start);
	if token == expected
	{
		Ok(())
	}
	else
	{
		let expectation = match expected
		{
			BaseToken::Assignment => "Expected assignment.",
			BaseToken::BraceLeft => "Expected opening brace.",
			BaseToken::BraceRight => "Expected closing brace.",
			BaseToken::BracketLeft => "Expected opening bracket.",
			BaseToken::BracketRight => "Expected closing bracket.",
			BaseToken::Dot => "Expected dot.",
			BaseToken::ParenLeft => "Expected opening parenthesis.",
			BaseToken::ParenRight => "Expected closing parenthesis.",
			BaseToken::Pipe => "Expected pipe.",
			BaseToken::Semicolon => "Expected semicolon.",
			BaseToken::StringLiteral => "Expected string literal.",
			BaseToken::Identifier => "Expected identifier.",
			_ => unreachable!(),
		};
		Err(ParsingError::UnexpectedToken {
			token: token_id,
			expectation,
		})
	}
}

#[inline(always)]
fn consume_optional(
	expected: BaseToken,
	tokens: &Tokens,
	span: &mut Span,
) -> bool
{
	let found = tokens.get(span.start) == expected;
	if found
	{
		tokens.advance(&mut span.start);
	}
	found
}
