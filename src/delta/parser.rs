pub mod parse_node;
pub mod parse_tree;
mod tokens;

use enumset::EnumSet;

use crate::alpha::common::BinaryOp;
use crate::alpha::common::ComparisonOp;
use crate::alpha::common::DeclarationFlag;
use crate::alpha::common::UnaryOp;
use crate::delta::lexer;
use crate::delta::lexer::BaseToken;
use crate::delta::lexer::ValueTypeKeyword;
use crate::delta::parser::parse_node::NodeId;
use crate::delta::parser::parse_node::ParseNode;
use crate::delta::parser::parse_tree::MAX_PARSE_NODE_CONTEXT;
use crate::delta::parser::parse_tree::ParseBuffer;
use crate::delta::parser::parse_tree::ParseTree;
use crate::delta::parser::tokens::Tokens;

pub const MAX_ADDRESS_DEPTH: u8 = 127;
pub const MAX_REFERENCE_DEPTH: usize = 127;

#[derive(Debug, Clone, Copy)]
pub enum ParsingError
{
	UnexpectedToken
	{
		token: lexer::tokens::TokenId,
		expectation: &'static str,
	},
	UnexpectedSemicolonAfterIdentifier
	{
		semicolon: lexer::tokens::TokenId,
		identifier_start: lexer::tokens::TokenId,
	},
	UnexpectedSemicolonAfterReturnValue
	{
		semicolon: lexer::tokens::TokenId,
		return_value_start: lexer::tokens::TokenId,
	},
	MissingReturnValueAfterStatement
	{
		unexpected_token: lexer::tokens::TokenId,
		return_statement_start: lexer::tokens::TokenId,
	},
	MissingConstantType
	{
		unexpected_token: lexer::tokens::TokenId,
	},
	MissingParameterType
	{
		unexpected_token: lexer::tokens::TokenId,
	},
	MissingMemberType
	{
		unexpected_token: lexer::tokens::TokenId,
	},
	MaximumParseDepthExceeded
	{
		start: lexer::tokens::TokenId,
		end: lexer::tokens::TokenId,
	},
}

pub fn parse(tokens: &lexer::tokens::Tokens) -> ParseTree
{
	let num_possible_declarations = tokens
		.base_tokens()
		.iter()
		.filter(|&&token| starts_declaration(token))
		.count();
	let mut parse_tree = ParseTree::empty(tokens, num_possible_declarations);

	let tokens = Tokens::from(tokens);
	let mut buffer = parse_tree.buffer();
	for _ in 0..MAX_PARSE_NODE_CONTEXT
	{
		// Padding.
		buffer.push_undeclared(ParseNode::NoMoreItems);
	}
	let mut start_of_next_declaration = tokens.cursor();
	for _ in 0..(num_possible_declarations + 2)
	{
		let mut tokens = tokens.starting_from(start_of_next_declaration);
		match tokens.peek()
		{
			BaseToken::EndOfSource => break,
			_ => (),
		}
		match parse_declaration(&mut tokens, &mut buffer)
		{
			Ok(node) => buffer.finish_declaration(node),
			Err(error) => buffer.store_error(error),
		}
		start_of_next_declaration = tokens.find_next(starts_declaration);
	}
	assert_eq!(
		tokens.starting_from(start_of_next_declaration).peek(),
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

fn starts_declaration(token: BaseToken) -> bool
{
	match token
	{
		BaseToken::Pub => true,
		BaseToken::Extern => true,
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

fn parse_declaration(
	tokens: &mut Tokens<'_>,
	buffer: &mut ParseBuffer<'_>,
) -> Result<NodeId, ParsingError>
{
	let start_of_declaration = tokens.cursor().into();
	let mut flags = EnumSet::new();
	if tokens.consume_optional(BaseToken::Pub)
	{
		flags.insert(DeclarationFlag::Public);
		buffer.set_public();
	}
	else
	{
		buffer.set_private();
	}
	if tokens.consume_optional(BaseToken::Extern)
	{
		flags.insert(DeclarationFlag::External);
	}
	let declaring_token_id = tokens.cursor();
	let declaring_token = tokens.take();
	match declaring_token
	{
		BaseToken::Import => parse_import_declaration(
			tokens,
			buffer,
			start_of_declaration,
			flags,
		),
		BaseToken::Const => parse_constant_declaration(
			tokens,
			buffer,
			start_of_declaration,
			flags,
		),
		BaseToken::Fn => parse_function_declaration(
			tokens,
			buffer,
			flags,
			start_of_declaration,
		),
		BaseToken::Struct => parse_struct_declaration(
			tokens,
			buffer,
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
	tokens: &mut Tokens<'_>,
	buffer: &mut ParseBuffer<'_>,
	start_of_declaration: parse_node::TokenId,
	flags: EnumSet<DeclarationFlag>,
) -> Result<NodeId, ParsingError>
{
	let literal = tokens.cursor().into();
	tokens.consume(BaseToken::StringLiteral)?;
	tokens.consume(BaseToken::Semicolon)?;
	buffer.push_undeclared(ParseNode::SimpleStringLiteral { literal });
	buffer.push_undeclared(ParseNode::DeclarationFlags(flags));
	let node = buffer.push(ParseNode::ImportDeclaration {
		start_of_declaration,
	});
	Ok(node)
}

fn parse_constant_declaration(
	tokens: &mut Tokens<'_>,
	buffer: &mut ParseBuffer<'_>,
	start_of_declaration: parse_node::TokenId,
	flags: EnumSet<DeclarationFlag>,
) -> Result<NodeId, ParsingError>
{
	let identifier = tokens.cursor().into();
	tokens.consume(BaseToken::Identifier)?;
	let location_of_colon = tokens.cursor();
	let value_type = if tokens.consume_optional(BaseToken::Colon)
	{
		parse_type(tokens, buffer)?
	}
	else
	{
		return Err(ParsingError::MissingConstantType {
			unexpected_token: location_of_colon,
		});
	};
	tokens.consume(BaseToken::Assignment)?;
	let expression = parse_expression(tokens, buffer)?;
	tokens.consume(BaseToken::Semicolon)?;
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
	tokens: &mut Tokens<'_>,
	buffer: &mut ParseBuffer<'_>,
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

	let identifier = tokens.cursor().into();
	tokens.consume(BaseToken::Identifier)?;
	let list = parse_struct_members(tokens, buffer)?;
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
	tokens: &mut Tokens<'_>,
	buffer: &mut ParseBuffer<'_>,
	start_of_declaration: parse_node::TokenId,
	mut flags: EnumSet<DeclarationFlag>,
) -> Result<NodeId, ParsingError>
{
	let structural_type = ParseNode::StructuralType {
		size_in_bytes_if_word: None,
	};

	let identifier = tokens.cursor().into();
	tokens.consume(BaseToken::Identifier)?;
	let list: NodeId = if tokens.consume_optional(BaseToken::Semicolon)
	{
		flags.insert(DeclarationFlag::OpaqueStruct);
		buffer.push(ParseNode::NoMoreItems)
	}
	else
	{
		let list = parse_struct_members(tokens, buffer)?;
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
	tokens: &mut Tokens<'_>,
	buffer: &mut ParseBuffer<'_>,
) -> Result<NodeId, ParsingError>
{
	tokens.consume(BaseToken::BraceLeft)?;
	let mut list = buffer.start_list();
	while !tokens.consume_optional(BaseToken::BraceRight)
	{
		let member = parse_member(tokens, buffer)?;
		tokens.consume(BaseToken::Comma)?;
		buffer.push_list_item(member, &mut list);
	}
	let start_of_list = buffer.push_end_of_list(list);
	Ok(start_of_list)
}

fn parse_function_declaration(
	tokens: &mut Tokens<'_>,
	buffer: &mut ParseBuffer<'_>,
	flags: EnumSet<DeclarationFlag>,
	start_of_declaration: parse_node::TokenId,
) -> Result<NodeId, ParsingError>
{
	let identifier = tokens.cursor().into();
	tokens.consume(BaseToken::Identifier)?;
	let (parameters, return_type) =
		parse_rest_of_function_signature(tokens, buffer)?;

	let body_node = if tokens.consume_optional(BaseToken::Semicolon)
	{
		ParseNode::NoMoreItems
	}
	else
	{
		// When the private zone gets expunged, two padding blocks
		// shift into the position of the return value and statements.
		if flags.contains(DeclarationFlag::Public)
		{
			buffer.set_private();
		}
		let (statements, return_value) = parse_function_body(tokens, buffer)?;
		// Note that we do not use buffer.expect_most_recent_node() here.
		// This is to allow layout compatibility with FunctionHead.
		buffer.push_optional_node(return_value);
		buffer.push_list(statements);
		let body = buffer.push(ParseNode::FunctionBody {});
		if flags.contains(DeclarationFlag::Public)
		{
			buffer.set_public();
		}
		ParseNode::FunctionImpl { body }
	};
	buffer.push_undeclared(body_node);
	buffer.push_older_node(return_type);
	buffer.push_list(parameters);
	buffer.push_undeclared(ParseNode::Identifier { identifier });
	buffer.push_undeclared(ParseNode::DeclarationFlags(flags));
	let node = buffer.push(ParseNode::FunctionDeclaration {
		start_of_declaration,
	});
	Ok(node)
}

fn parse_rest_of_function_signature(
	tokens: &mut Tokens<'_>,
	buffer: &mut ParseBuffer<'_>,
) -> Result<(NodeId, NodeId), ParsingError>
{
	tokens.consume(BaseToken::ParenLeft)?;
	let mut list = buffer.start_list();
	loop
	{
		if tokens.consume_optional(BaseToken::ParenRight)
		{
			break;
		}
		let parameter = parse_parameter(tokens, buffer)?;
		buffer.push_list_item(parameter, &mut list);
		if tokens.consume_optional(BaseToken::Comma)
		{
			continue;
		}
		else
		{
			tokens.consume(BaseToken::ParenRight)?;
			break;
		}
	}
	let parameters = buffer.push_end_of_list(list);

	let return_type = if tokens.consume_optional(BaseToken::Arrow)
	{
		parse_type(tokens, buffer)?
	}
	else
	{
		buffer.push(ParseNode::SimpleValueType(ValueTypeKeyword::Void))
	};
	Ok((parameters, return_type))
}

fn parse_member(
	tokens: &mut Tokens<'_>,
	buffer: &mut ParseBuffer<'_>,
) -> Result<NodeId, ParsingError>
{
	let identifier = tokens.cursor().into();
	tokens.consume(BaseToken::Identifier)?;
	let location_of_colon = tokens.cursor();
	let value_type = if tokens.consume_optional(BaseToken::Colon)
	{
		parse_type(tokens, buffer)?
	}
	else
	{
		return Err(ParsingError::MissingMemberType {
			unexpected_token: location_of_colon,
		});
	};
	buffer.expect_most_recent_node(value_type);
	let node = buffer.push(ParseNode::IdentifierAndType { identifier });
	Ok(node)
}

fn parse_parameter(
	tokens: &mut Tokens<'_>,
	buffer: &mut ParseBuffer<'_>,
) -> Result<NodeId, ParsingError>
{
	let identifier = tokens.cursor().into();
	tokens.consume(BaseToken::Identifier)?;
	let location_of_colon = tokens.cursor();
	let value_type = if tokens.consume_optional(BaseToken::Colon)
	{
		parse_type(tokens, buffer)?
	}
	else
	{
		return Err(ParsingError::MissingParameterType {
			unexpected_token: location_of_colon,
		});
	};
	buffer.expect_most_recent_node(value_type);
	let node = buffer.push(ParseNode::IdentifierAndType { identifier });
	Ok(node)
}

fn parse_type(
	tokens: &mut Tokens<'_>,
	buffer: &mut ParseBuffer<'_>,
) -> Result<NodeId, ParsingError>
{
	let vt_start = tokens.cursor();
	let value_type = parse_inner_type(tokens, buffer)?;
	let vt_end = tokens.cursor();
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
	tokens: &mut Tokens<'_>,
	buffer: &mut ParseBuffer<'_>,
) -> Result<NodeId, ParsingError>
{
	let token_id = tokens.cursor();
	let node_id = match tokens.take()
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
			let inner = parse_inner_type(tokens, buffer)?;
			buffer.expect_most_recent_node(inner);
			buffer.push(ParseNode::PointerVT {})
		}
		BaseToken::ParenLeft =>
		{
			let inner = parse_inner_type(tokens, buffer)?;
			tokens.consume(BaseToken::ParenRight)?;
			buffer.expect_most_recent_node(inner);
			buffer.push(ParseNode::ViewVT {})
		}
		BaseToken::BracketLeft =>
		{
			// TODO if my types are outward-in, should I store them that way in the parse tree?
			let length_token_id = tokens.cursor();
			let length_or_bracket = tokens.take();
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
				tokens.consume(BaseToken::BracketRight)?;
			}
			let inner = parse_inner_type(tokens, buffer)?;
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
	tokens: &mut Tokens<'_>,
	buffer: &mut ParseBuffer<'_>,
) -> Result<(NodeId, Option<NodeId>), ParsingError>
{
	tokens.consume(BaseToken::BraceLeft)?;
	let mut list = buffer.start_list();
	loop
	{
		if tokens.consume_optional(BaseToken::BraceRight)
		{
			let statements = buffer.push_end_of_list(list);
			return Ok((statements, None));
		}

		if tokens.consume_optional(BaseToken::Return)
		{
			let statements = buffer.push_end_of_list(list);
			tokens.consume(BaseToken::Colon)?;
			let return_value = parse_expression(tokens, buffer)?;
			return Ok((statements, Some(return_value)));
		}

		let statement = parse_statement(tokens, buffer)?;
		buffer.push_list_item(statement, &mut list);
	}
}

fn parse_rest_of_block(
	tokens: &mut Tokens<'_>,
	buffer: &mut ParseBuffer<'_>,
) -> Result<NodeId, ParsingError>
{
	let mut list = buffer.start_list();
	loop
	{
		if tokens.consume_optional(BaseToken::BraceRight)
		{
			let statements = buffer.push_end_of_list(list);
			return Ok(statements);
		}

		let statement = parse_statement(tokens, buffer)?;
		buffer.push_list_item(statement, &mut list);
	}
}

fn parse_statement(
	tokens: &mut Tokens<'_>,
	buffer: &mut ParseBuffer<'_>,
) -> Result<NodeId, ParsingError>
{
	let first_token_id = tokens.cursor();
	let statement = match tokens.take()
	{
		BaseToken::BraceLeft =>
		{
			let statements = parse_rest_of_block(tokens, buffer)?;
			buffer.push(ParseNode::Block { first: statements })
		}
		BaseToken::If =>
		{
			let comparison = {
				// Do not greedily parse
				//   if x == y { foo: bar
				// as an illegal comparison against a structure initialization.
				// It is more natural to parse it as the start of a block statement.
				let mut tokens = tokens.with_reservation(|token| {
					matches!(token, BaseToken::BraceLeft | BaseToken::Semicolon)
				});
				parse_comparison(&mut tokens, buffer)?
			};
			let then = parse_then(tokens, buffer)?;
			buffer.expect_most_recent_node(then);
			buffer.push(ParseNode::If { comparison })
		}
		BaseToken::Loop =>
		{
			tokens.consume(BaseToken::Semicolon)?;
			buffer.push(ParseNode::Loop {
				token: first_token_id.into(),
			})
		}
		BaseToken::Goto =>
		{
			let label = tokens.cursor();
			if tokens.consume_optional(BaseToken::Return)
			{
				// A bit of a hack: pretend this keyword is an identifier.
				// It's source span will be a valid identifiier.
			}
			else
			{
				tokens.consume(BaseToken::Identifier)?;
			};
			tokens.consume(BaseToken::Semicolon)?;
			buffer.push_undeclared(ParseNode::Identifier {
				identifier: label.into(),
			});
			buffer.push(ParseNode::Goto {
				token: first_token_id.into(),
			})
		}
		BaseToken::Var =>
		{
			let identifier = tokens.cursor().into();
			tokens.consume(BaseToken::Identifier)?;

			let value_type = if tokens.consume_optional(BaseToken::Colon)
			{
				let value_type = parse_type(tokens, buffer)?;
				Some(value_type)
			}
			else
			{
				None
			};
			let value = if tokens.consume_optional(BaseToken::Assignment)
			{
				let expression = parse_expression(tokens, buffer)?;
				Some(expression)
			}
			else
			{
				None
			};
			tokens.consume(BaseToken::Semicolon)?;
			buffer.push_optional_node(value);
			buffer.push_optional_node(value_type);
			buffer.push(ParseNode::VariableDeclaration { identifier })
		}
		BaseToken::Identifier =>
		{
			let identifier = first_token_id.into();
			let colon = tokens.cursor().into();
			if tokens.consume_optional(BaseToken::Colon)
			{
				buffer.push(ParseNode::Label { colon })
			}
			else if tokens.consume_optional(BaseToken::ParenLeft)
			{
				let arguments = parse_rest_of_arguments(tokens, buffer)?;
				tokens.consume(BaseToken::Semicolon)?;
				let identifier = first_token_id.into();
				buffer.push_list(arguments);
				buffer.push_undeclared(ParseNode::Identifier { identifier });
				buffer.push(ParseNode::MethodCall { is_builtin: false })
			}
			else
			{
				let deref_steps = parse_deref_steps_list(tokens, buffer)?;
				buffer.push_list(deref_steps);
				buffer.push_undeclared(ParseNode::Identifier { identifier });
				buffer
					.push_undeclared(ParseNode::DerefAddressDepth { depth: 0 });
				let reference = buffer.push(ParseNode::Deref {
					start_of_reference: identifier,
				});

				let semicolon = tokens.cursor();
				if tokens.consume_optional(BaseToken::Semicolon)
				{
					return Err(
						ParsingError::UnexpectedSemicolonAfterIdentifier {
							semicolon,
							identifier_start: first_token_id,
						},
					);
				}
				tokens.consume(BaseToken::Assignment)?;
				let expression = parse_expression(tokens, buffer)?;
				tokens.consume(BaseToken::Semicolon)?;
				buffer.expect_most_recent_node(expression);
				buffer.push_older_node(reference);
				buffer.push(ParseNode::Assignment {})
			}
		}
		BaseToken::Builtin =>
		{
			tokens.consume(BaseToken::ParenLeft)?;
			let arguments = parse_rest_of_arguments(tokens, buffer)?;
			tokens.consume(BaseToken::Semicolon)?;
			let identifier = first_token_id.into();
			buffer.push_list(arguments);
			buffer.push_undeclared(ParseNode::Identifier { identifier });
			buffer.push(ParseNode::MethodCall { is_builtin: true })
		}
		BaseToken::Ampersand =>
		{
			let mut depth = 1;
			while tokens.consume_optional(BaseToken::Ampersand)
			{
				depth += 1;
				if depth > MAX_ADDRESS_DEPTH
				{
					let end = tokens.cursor().into();
					return Err(ParsingError::MaximumParseDepthExceeded {
						start: first_token_id.into(),
						end,
					});
				}
			}
			let identifier = tokens.cursor().into();
			tokens.consume(BaseToken::Identifier)?;
			let steps = parse_deref_steps_list(tokens, buffer)?;
			buffer.push_list(steps);
			buffer.push_undeclared(ParseNode::Identifier { identifier });
			buffer.push_undeclared(ParseNode::DerefAddressDepth { depth });
			let reference = buffer.push(ParseNode::Deref {
				start_of_reference: identifier,
			});

			let semicolon = tokens.cursor();
			if tokens.consume_optional(BaseToken::Semicolon)
			{
				return Err(ParsingError::UnexpectedSemicolonAfterIdentifier {
					semicolon,
					identifier_start: first_token_id,
				});
			}
			tokens.consume(BaseToken::Assignment)?;
			let expression = parse_expression(tokens, buffer)?;
			tokens.consume(BaseToken::Semicolon)?;
			buffer.expect_most_recent_node(expression);
			buffer.push_older_node(reference);
			buffer.push(ParseNode::Assignment {})
		}
		_ =>
		{
			return Err(ParsingError::UnexpectedToken {
				token: first_token_id.into(),
				expectation: "Expected statement.",
			});
		}
	};
	Ok(statement)
}

fn parse_rest_of_arguments(
	tokens: &mut Tokens<'_>,
	buffer: &mut ParseBuffer<'_>,
) -> Result<NodeId, ParsingError>
{
	let mut list = buffer.start_list();
	loop
	{
		if tokens.consume_optional(BaseToken::ParenRight)
		{
			break;
		}

		let expression = parse_expression(tokens, buffer)?;
		buffer.push_list_item(expression, &mut list);

		if tokens.consume_optional(BaseToken::Comma)
		{
			continue;
		}
		else
		{
			tokens.consume(BaseToken::ParenRight)?;
			break;
		}
	}
	let arguments = buffer.push_end_of_list(list);
	return Ok(arguments);
}

fn parse_rest_of_structural(
	tokens: &mut Tokens<'_>,
	buffer: &mut ParseBuffer<'_>,
) -> Result<NodeId, ParsingError>
{
	let mut list = buffer.start_list();
	loop
	{
		if tokens.consume_optional(BaseToken::BraceRight)
		{
			break;
		}

		let identifier = tokens.cursor().into();
		tokens.consume(BaseToken::Identifier)?;
		let expression = if tokens.consume_optional(BaseToken::Colon)
		{
			parse_expression(tokens, buffer)?
		}
		else
		{
			// Field init shorthand.
			let list = buffer.start_list();
			let deref_steps = buffer.push_end_of_list(list);
			buffer.push_list(deref_steps);
			buffer.push_undeclared(ParseNode::Identifier { identifier });
			buffer.push_undeclared(ParseNode::DerefAddressDepth { depth: 0 });
			buffer.push(ParseNode::Deref {
				start_of_reference: identifier,
			})
		};
		buffer.expect_most_recent_node(expression);
		let field =
			buffer.push(ParseNode::IdentifierAndExpression { identifier });
		buffer.push_list_item(field, &mut list);

		if tokens.consume_optional(BaseToken::Comma)
		{
			continue;
		}
		else
		{
			tokens.consume(BaseToken::BraceRight)?;
			break;
		}
	}
	let statements = buffer.push_end_of_list(list);
	return Ok(statements);
}

fn parse_then(
	tokens: &mut Tokens<'_>,
	buffer: &mut ParseBuffer<'_>,
) -> Result<NodeId, ParsingError>
{
	let then_branch = parse_statement(tokens, buffer)?;
	let branches = if tokens.consume_optional(BaseToken::Else)
	{
		let else_branch = parse_statement(tokens, buffer)?;
		buffer.expect_most_recent_node(else_branch);
		buffer.push(ParseNode::ThenElse { then: then_branch })
	}
	else
	{
		buffer.expect_most_recent_node(then_branch);
		buffer.push(ParseNode::Then {})
	};
	Ok(branches)
}

fn parse_comparison(
	tokens: &mut Tokens<'_>,
	buffer: &mut ParseBuffer<'_>,
) -> Result<NodeId, ParsingError>
{
	let left = parse_expression(tokens, buffer)?;

	let op_token_id = tokens.cursor();
	let op = match tokens.take()
	{
		BaseToken::Equals => ComparisonOp::Equals,
		BaseToken::DoesNotEqual => ComparisonOp::DoesNotEqual,
		BaseToken::AngleLeft => ComparisonOp::IsLess,
		BaseToken::AngleRight => ComparisonOp::IsGreater,
		BaseToken::IsGE => ComparisonOp::IsGE,
		BaseToken::IsLE => ComparisonOp::IsLE,
		_ =>
		{
			return Err(ParsingError::UnexpectedToken {
				token: op_token_id,
				expectation: "Expected comparison operator.",
			});
		}
	};

	let right = parse_expression(tokens, buffer)?;

	buffer.expect_most_recent_node(right);
	buffer.push_older_node(left);
	buffer.push_undeclared(ParseNode::ComparisonOp(op));
	let comparison = buffer.push(ParseNode::Comparison {
		token: op_token_id.into(),
	});
	Ok(comparison)
}

fn parse_expression(
	tokens: &mut Tokens<'_>,
	buffer: &mut ParseBuffer<'_>,
) -> Result<NodeId, ParsingError>
{
	parse_addition(tokens, buffer)
}

fn parse_addition(
	tokens: &mut Tokens<'_>,
	buffer: &mut ParseBuffer<'_>,
) -> Result<NodeId, ParsingError>
{
	let mut expression = parse_multiplication(tokens, buffer)?;

	loop
	{
		let op_token_id = tokens.cursor();
		let op = match tokens.peek()
		{
			BaseToken::Ampersand | BaseToken::Pipe | BaseToken::Caret =>
			{
				return parse_rest_of_bitwise_expression(
					expression, tokens, buffer,
				);
			}
			BaseToken::ShiftLeft | BaseToken::ShiftRight =>
			{
				return parse_rest_of_bitshift_operation(
					expression, tokens, buffer,
				);
			}
			BaseToken::Plus => BinaryOp::Add,
			BaseToken::Minus => BinaryOp::Subtract,
			_ => return Ok(expression),
		};
		let _peeked = tokens.take();

		let right = parse_multiplication(tokens, buffer)?;

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
	tokens: &mut Tokens<'_>,
	buffer: &mut ParseBuffer<'_>,
) -> Result<NodeId, ParsingError>
{
	let mut op_token_id = tokens.cursor();
	let op_token = tokens.take();
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
		let right = parse_unary_expression(tokens, buffer)?;

		buffer.expect_most_recent_node(right);
		buffer.push_older_node(expression);
		buffer.push_undeclared(ParseNode::BinaryOp(op));
		expression = buffer.push(ParseNode::Binary {
			token: op_token_id.into(),
		});

		op_token_id = tokens.cursor();
		if tokens.consume_optional(op_token)
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
	tokens: &mut Tokens<'_>,
	buffer: &mut ParseBuffer<'_>,
) -> Result<NodeId, ParsingError>
{
	let op_token_id = tokens.cursor();
	let op_token = tokens.take();
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

	let right = parse_unary_expression(tokens, buffer)?;

	buffer.expect_most_recent_node(right);
	buffer.push_older_node(left);
	buffer.push_undeclared(ParseNode::BinaryOp(op));
	let expression = buffer.push(ParseNode::Binary {
		token: op_token_id.into(),
	});
	Ok(expression)
}

fn parse_multiplication(
	tokens: &mut Tokens<'_>,
	buffer: &mut ParseBuffer<'_>,
) -> Result<NodeId, ParsingError>
{
	let mut expression = parse_singular_expression(tokens, buffer)?;

	loop
	{
		let op_token_id = tokens.cursor();
		let op = match tokens.peek()
		{
			BaseToken::Times => BinaryOp::Multiply,
			BaseToken::Divide => BinaryOp::Divide,
			BaseToken::Modulo => BinaryOp::Modulo,
			_ => return Ok(expression),
		};
		let _peeked = tokens.take();

		let right = parse_singular_expression(tokens, buffer)?;

		buffer.expect_most_recent_node(right);
		buffer.push_older_node(expression);
		buffer.push_undeclared(ParseNode::BinaryOp(op));
		expression = buffer.push(ParseNode::Binary {
			token: op_token_id.into(),
		});
	}
}

fn parse_singular_expression(
	tokens: &mut Tokens<'_>,
	buffer: &mut ParseBuffer<'_>,
) -> Result<NodeId, ParsingError>
{
	let bitcast_token_id = tokens.cursor();
	let bitcast = tokens
		.consume_optional(BaseToken::Cast)
		.then_some(bitcast_token_id);

	let mut expression = parse_unary_expression(tokens, buffer)?;

	if let Some(bitcast_token_id) = bitcast
	{
		buffer.expect_most_recent_node(expression);
		expression = buffer.push(ParseNode::BitCast {
			cast_keyword: bitcast_token_id.into(),
		});
	}

	while tokens.consume_optional(BaseToken::As)
	{
		let start_of_type = tokens.cursor().into();
		let coerced_type = parse_type(tokens, buffer)?;
		buffer.expect_most_recent_node(coerced_type);
		buffer.push_older_node(expression);
		expression = buffer.push(ParseNode::TypeCast { start_of_type });
	}

	Ok(expression)
}

fn parse_unary_expression(
	tokens: &mut Tokens<'_>,
	buffer: &mut ParseBuffer<'_>,
) -> Result<NodeId, ParsingError>
{
	match tokens.peek()
	{
		BaseToken::PipeForType =>
		{
			let _peeked = tokens.take();
			let queried_type = parse_type(tokens, buffer)?;
			tokens.consume(BaseToken::Pipe)?;
			buffer.expect_most_recent_node(queried_type);
			let expression = buffer.push(ParseNode::SizeOf {});
			Ok(expression)
		}
		BaseToken::Pipe =>
		{
			let _peeked = tokens.take();
			let reference = parse_reference(tokens, buffer)?;
			tokens.consume(BaseToken::Pipe)?;
			buffer.expect_most_recent_node(reference);
			let expression = buffer.push(ParseNode::LengthOf {});
			Ok(expression)
		}
		BaseToken::Exclamation =>
		{
			let op_token_id = tokens.cursor();
			let _peeked = tokens.take();
			let op = UnaryOp::BitwiseComplement;

			let expression = parse_primary_expression(tokens, buffer)?;
			buffer.expect_most_recent_node(expression);
			buffer.push_undeclared(ParseNode::UnaryOp(op));
			let expression = buffer.push(ParseNode::Unary {
				token: op_token_id.into(),
			});
			Ok(expression)
		}
		BaseToken::Minus =>
		{
			let op_token_id = tokens.cursor();
			let _peeked = tokens.take();
			let op = UnaryOp::Negative;

			// TODO either handle negative integers here
			// or just before I check integer ranges
			// because -128i8 is valid, but 128i8 is not

			let expression = parse_primary_expression(tokens, buffer)?;
			buffer.expect_most_recent_node(expression);
			buffer.push_undeclared(ParseNode::UnaryOp(op));
			let expression = buffer.push(ParseNode::Unary {
				token: op_token_id.into(),
			});
			Ok(expression)
		}
		_ => parse_primary_expression(tokens, buffer),
	}
}

fn parse_primary_expression(
	tokens: &mut Tokens<'_>,
	buffer: &mut ParseBuffer<'_>,
) -> Result<NodeId, ParsingError>
{
	let first_token_id = tokens.cursor();
	match tokens.take()
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
			let expression = if tokens.consume_optional(token)
			{
				let mut end = tokens.cursor().into();
				while tokens.consume_optional(token)
				{
					end = tokens.cursor().into();
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
			while tokens.consume_optional(BaseToken::Ampersand)
			{
				depth += 1;
				if depth > MAX_ADDRESS_DEPTH
				{
					let end = tokens.cursor().into();
					return Err(ParsingError::MaximumParseDepthExceeded {
						start: first_token_id.into(),
						end,
					});
				}
			}
			let identifier = tokens.cursor().into();
			tokens.consume(BaseToken::Identifier)?;
			let steps = parse_deref_steps_list(tokens, buffer)?;
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
			let identifier = first_token_id.into();
			if tokens.consume_optional(BaseToken::ParenLeft)
			{
				let arguments = parse_rest_of_arguments(tokens, buffer)?;
				buffer.push_list(arguments);
				buffer.push_undeclared(ParseNode::Identifier { identifier });
				let expression =
					buffer.push(ParseNode::FunctionCall { is_builtin: false });
				return Ok(expression);
			}
			if tokens.consume_optional(BaseToken::BraceLeft)
			{
				let arguments = parse_rest_of_structural(tokens, buffer)?;
				buffer.push_list(arguments);
				let expression = buffer.push(ParseNode::Structural {
					unresolved_struct_or_word: identifier,
				});
				return Ok(expression);
			}

			let steps = parse_deref_steps_list(tokens, buffer)?;
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
			tokens.consume(BaseToken::ParenLeft)?;
			let arguments = parse_rest_of_arguments(tokens, buffer)?;
			let identifier = first_token_id.into();
			buffer.push_list(arguments);
			buffer.push_undeclared(ParseNode::Identifier { identifier });
			let expression =
				buffer.push(ParseNode::FunctionCall { is_builtin: true });
			Ok(expression)
		}
		BaseToken::BracketLeft =>
		{
			let mut num_elements: usize = 0;
			let mut list = buffer.start_list();
			if !tokens.consume_optional(BaseToken::BracketRight)
			{
				loop
				{
					if tokens.consume_optional(BaseToken::BracketRight)
					{
						break;
					}
					let element = parse_expression(tokens, buffer)?;
					buffer.push_list_item(element, &mut list);
					num_elements += 1;
					if tokens.consume_optional(BaseToken::Comma)
					{
						continue;
					}
					else
					{
						tokens.consume(BaseToken::BracketRight)?;
						break;
					}
				}
			}
			let elements = buffer.push_end_of_list(list);
			buffer.push_list(elements);
			let num_elements = parse_node::U24::new(num_elements);
			let expression =
				buffer.push(ParseNode::ArrayLiteral { num_elements });
			Ok(expression)
		}
		BaseToken::ParenLeft =>
		{
			let inner = parse_expression(tokens, buffer)?;
			tokens.consume(BaseToken::ParenRight)?;
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
	tokens: &mut Tokens<'_>,
	buffer: &mut ParseBuffer<'_>,
) -> Result<NodeId, ParsingError>
{
	let first_token_id = tokens.cursor();
	let mut address_depth = 1;
	while tokens.consume_optional(BaseToken::Ampersand)
	{
		address_depth += 1;
		if address_depth > MAX_ADDRESS_DEPTH
		{
			let start = first_token_id;
			let end = tokens.cursor();
			return Err(ParsingError::MaximumParseDepthExceeded { start, end });
		}
	}
	let identifier = tokens.cursor().into();
	tokens.consume(BaseToken::Identifier)?;
	let steps = parse_deref_steps_list(tokens, buffer)?;
	buffer.push_list(steps);
	buffer.push_undeclared(ParseNode::Identifier { identifier });
	buffer.push_undeclared(ParseNode::DerefAddressDepth { depth: 0 });
	let expression = buffer.push(ParseNode::Deref {
		start_of_reference: first_token_id.into(),
	});
	Ok(expression)
}

fn parse_deref_steps_list(
	tokens: &mut Tokens<'_>,
	buffer: &mut ParseBuffer<'_>,
) -> Result<NodeId, ParsingError>
{
	let start = tokens.cursor().into();
	let mut list = buffer.start_list();
	for _ in 0..MAX_REFERENCE_DEPTH
	{
		let step = if tokens.consume_optional(BaseToken::BracketLeft)
		{
			let argument = parse_expression(tokens, buffer)?;
			tokens.consume(BaseToken::BracketRight)?;
			buffer.expect_most_recent_node(argument);
			buffer.push(ParseNode::DerefStepElement {})
		}
		else if tokens.consume_optional(BaseToken::Dot)
		{
			let field_identifier = tokens.cursor().into();
			tokens.consume(BaseToken::Identifier)?;
			buffer.push(ParseNode::DerefStepMember { field_identifier })
		}
		else
		{
			let start_of_list = buffer.push_end_of_list(list);
			return Ok(start_of_list);
		};
		buffer.push_list_item(step, &mut list);
	}
	let end = tokens.cursor().into();
	Err(ParsingError::MaximumParseDepthExceeded { start, end })
}
