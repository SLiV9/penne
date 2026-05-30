use enumset::EnumSet;

use crate::alpha::common::BinaryOp;
use crate::alpha::common::ComparisonOp;
use crate::alpha::common::DeclarationFlag;
use crate::alpha::common::UnaryOp;
use crate::delta::lexer;
use crate::delta::lexer::BaseToken;
use crate::delta::lexer::ValueTypeKeyword;
use crate::delta::lexer::tokens::Tokens;

pub const MAX_ADDRESS_DEPTH: u8 = 127;
pub const MAX_REFERENCE_DEPTH: usize = 127;
pub const MAX_NUM_PARSING_ERRORS: usize = 100;

const MAX_NUM_NODES: usize = 1 << 24;

#[derive(Debug, Clone, Copy)]
struct NodeId(U24);

#[derive(Debug, Clone, Copy)]
struct TokenId(U24);

#[derive(Clone, Copy)]
struct U24([u8; 3]);

impl U24
{
	fn new(value: u32) -> Self
	{
		debug_assert!((value as usize) < MAX_NUM_NODES);
		let [_x3, x2, x1, x0] = value.to_le_bytes();
		Self([x2, x1, x0])
	}
}

impl From<U24> for u32
{
	fn from(value: U24) -> Self
	{
		let [x2, x1, x0] = value.0;
		let bytes = [0, x2, x1, x0];
		u32::from_le_bytes(bytes)
	}
}

impl std::fmt::Debug for U24
{
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result
	{
		let value = u32::from(*self);
		f.debug_tuple("U24").field(&value).finish()
	}
}

#[derive(Debug, Clone, Copy)]
pub enum ParseNode
{
	ConstantDeclaration
	{
		start_of_declaration: TokenId,
		// flags: DeclarationFlags = nodes[-1]
		// identifier: Identifier = nodes[-2]
		// value_type: Item<ValueType> = nodes[-3]
		// expression: Expression = nodes[..-4]
	},
	FunctionDeclaration
	{
		start_of_declaration: TokenId,
		// flags: DeclarationFlags = nodes[-1]
		// identifier: Identifier = nodes[-2]
		// parameters: List<IdentifierAndVT> = nodes[-3]
		// statements: List<Statement> = nodes[-4],
		// return_value: Item<Expression> = nodes[-5]
		// return_type: ValueType = nodes[..-6]
	},
	FunctionHeadDeclaration
	{
		start_of_declaration: TokenId,
		// flags: DeclarationFlags = nodes[-1]
		// identifier: Identifier = nodes[-2]
		// parameters: List<IdentifierAndVT> = nodes[-3]
		// statements: Padding = nodes[-4]
		// return_value: Padding = nodes[-5]
		// return_type: ValueType = nodes[..-6]
	},
	StructureDeclaration
	{
		start_of_declaration: TokenId,
		// flags: DeclarationFlags = nodes[-1]
		// identifier: Identifier = nodes[-2]
		// members: List<IdentifierAndVT> = nodes[-3]
		// structural_type: ValueType = nodes[..-4]
	},
	ImportDeclaration
	{
		start_of_declaration: TokenId,
		// path: StringLiteral = nodes[-1]
	},
	DeclarationFlags(EnumSet<DeclarationFlag>),
	Identifier
	{
		identifier_token: TokenId,
	},
	IdentifierAndVT
	{
		identifier_token: TokenId,
		// value_type: ValueType = nodes[..-1]
	},
	IdentifierAndExpression
	{
		identifier_token: TokenId,
		// expression: Expression = nodes[..-1]
	},
	VariableDeclaration
	{
		identifier_token: TokenId,
		// value_type: Item<ValueType>/NoMoreItems = nodes[-1]
		// expression: Item<Expression>/NoMoreItems = nodes[-2]
	},
	Assignment {
		// deref: Item<Deref> = nodes[-1]
		// expression: Expression = nodes[..-2]
	},
	Loop
	{
		token: TokenId,
	},
	Goto
	{
		token: TokenId,
		// label: Identifier = nodes[-1]
	},
	Label
	{
		// label: Identifier = nodes[-1]
		semicolon: TokenId,
	},
	If
	{
		comparison: NodeId,
		// then: Then/ThenElse = nodes[-1]
	},
	Then {
		// then: Statement = nodes[..-1]
	},
	ThenElse
	{
		then: NodeId,
		// else: Statement = nodes[..-1]
	},
	Block
	{
		first: NodeId, // ListItem<Statement>
	},
	MethodCall
	{
		// identifier: Identifier = nodes[-1]
		is_builtin: bool,
		// arguments: List<Expression> = nodes[-2]
	},
	Parenthesized {
		// inner: Expression = nodes[..-1]
	},
	Comparison
	{
		token: TokenId,
		// op: ComparisonOp = nodes[-1],
		// left: Item<Expression> = nodes[-2],
		// right: Expression = nodes[..-3]
	},
	ComparisonOp(ComparisonOp),
	Binary
	{
		token: TokenId,
		// op: BinaryOp = nodes[-1],
		// left: Item<Expression> = nodes[-2],
		// right: Expression = nodes[..-3]
	},
	BinaryOp(BinaryOp),
	Unary
	{
		token: TokenId,
		// op: UnaryOp = nodes[-1],
		// operand: Expression = nodes[..-2],
	},
	UnaryOp(UnaryOp),
	BooleanLiteral
	{
		literal: TokenId,
	},
	CharLiteral
	{
		literal: TokenId,
	},
	UntypedIntegerLiteral
	{
		literal: TokenId,
	},
	TypedIntegerLiteral
	{
		literal: TokenId,
		// value_type: SimpleValueType = nodes[-1]
	},
	StringLiteral
	{
		literal: TokenId,
	},
	ArrayLiteral
	{
		num_elements: U24,
		// items: List<Expression> = nodes[-1]
	},
	Structural
	{
		unresolved_struct_or_word: TokenId,
		// field_initializers: List<IdentifierAndExpression> = nodes[-1]
	},
	Deref
	{
		start_of_reference: TokenId,
		// address_depth: DerefAddressDepth = nodes[-1]
		// base_identifier: Identifier = nodes[-2]
		// steps: List<DerefStep> = nodes[-3]
	},
	DerefAddressDepth
	{
		depth: u8,
	},
	DerefStepElement {
		// argument: Expression = nodes[..-1]
	},
	DerefStepMember
	{
		field_identifier: TokenId,
	},
	BitCast
	{
		cast_keyword: TokenId,
		// expression: Expression = nodes[..-1]
	},
	TypeCast
	{
		start_of_type: TokenId,
		// expression: Item<Expression> = nodes[-1]
		// coerced_type: ValueType = nodes[..-2]
	},
	LengthOf {
		// deref: Deref = nodes[..-1]
	},
	SizeOf {
		// queried_type: ValueType = nodes[..-1]
	},
	FunctionCall
	{
		// identifier: Identifier = nodes[-1]
		is_builtin: bool,
		// arguments: List<Expression> = nodes[-2]
	},
	SimpleValueType(ValueTypeKeyword),
	ArrayVT
	{
		// element_type = nodes[-1]
		fixed_length: U24,
	},
	ArrayWithNamedLengthVT
	{
		// element_type = nodes[-1]
		named_length_identifier: TokenId,
	},
	SliceVT {
		// element_type = nodes[-1]
	},
	SlicePointerVT {
		// element_type = nodes[-1]
	},
	EndlessArrayVT {
		// element_type = nodes[-1]
	},
	ArraylikeVT {
		// element_type = nodes[-1]
	},
	StructVT
	{
		identifier: TokenId,
	},
	WordVT
	{
		identifier: TokenId,
	},
	PointerVT {
		// deref_type = nodes[-1]
	},
	ViewVT {
		// deref_type = nodes[-1]
	},
	Item
	{
		at: NodeId, // ParseNode
	},
	List
	{
		first: NodeId, // ListItem
	},
	ListItem
	{
		// value: ParseNode = nodes[-1]
		next: NodeId,
	},
	NoMoreItems,
	Padding,
	Poison,
}

#[derive(Debug, Clone, Copy)]
pub enum Declaration {}

pub enum ParsingError
{
	UnexpectedToken,
	UnexpectedSemicolonAfterIdentifier,
	UnexpectedSemicolonAfterReturnValue,
	MissingReturnType,
	MissingAmbiguousReturnType,
	AmbiguousReturnValue,
	ConflictingReturnValue,
	MissingReturnValue,
	MissingReturnValueAfterStatement,
	MissingConstantType,
	MissingParameterType,
	MissingMemberType,
	IllegalType,
	IllegalReturnType,
	IllegalVariableType,
	IllegalConstantType,
	IllegalParameterType,
	IllegalMemberType,
	TypeNotAllowedInExtern,
	TypeLacksKnownSize,
	UnsupportedInConstContext,
	FunctionInConstContext,
	WordSizeMismatch,
	MaximumParseDepthExceeded,
}

// TODO move into file
pub struct ParseTree
{
	nodes: Vec<ParseNode>,

	declarations: Vec<NodeId>,

	errors: Vec<ParsingError>,
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
	let num_known_declarations = tokens
		.base_tokens()
		.iter()
		.filter(|&&token| starts_declaration(token))
		.count();
	let num_possible_declarations = 2 * num_known_declarations + 2;
	let mut declarations = Vec::with_capacity(num_possible_declarations);
	let mut start_of_next_declaration = 0;
	for _ in 0..(num_known_declarations + 1)
	{
		match tokens.base_tokens()[start_of_next_declaration]
		{
			BaseToken::EndOfSource => break,
			_ => (),
		}
		assert!(declarations.len() + 2 <= declarations.capacity());
		let mut span = tokens.find_span(
			start_of_next_declaration,
			starts_declaration,
			starts_declaration,
		);
		let declaration = parse_declaration(tokens, &mut span);
		declarations.push(declaration);
		start_of_next_declaration = span.start;
		if !span.is_empty()
		{
			let token = tokens.base_tokens()[start_of_next_declaration];
			// TODO expected start of declaration
			// declarations.push(error);
		}
		start_of_next_declaration = span.end;
	}
	assert_eq!(
		tokens.base_tokens()[start_of_next_declaration],
		BaseToken::EndOfSource
	);
	parse_tree
}

fn starts_declaration(token: BaseToken) -> bool
{
	match token
	{
		BaseToken::Import => true,
		BaseToken::Pub => true,
		BaseToken::Extern => true,
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
	tokens: &Tokens,
	span: &mut std::ops::Range<usize>,
) -> Result<Declaration, &'static str>
{
	// TODO
	// let start_of_declaration = tokens.get_id(span.start).expect();
	let start_of_declaration = span.start;
	let mut flags = EnumSet::new();
	if consume_optional(BaseToken::Pub, tokens, span)
	{
		flags.insert(DeclarationFlag::Public);
	}
	if consume_optional(BaseToken::Extern, tokens, span)
	{
		flags.insert(DeclarationFlag::External);
	}
	let declaring_token = take(tokens, span);
	match declaring_token
	{
		BaseToken::Import =>
		{
			parse_import_declaration(tokens, span, start_of_declaration, flags)
		}
		BaseToken::Const => parse_constant_declaration(
			tokens,
			span,
			start_of_declaration,
			flags,
		),
		BaseToken::Fn => parse_function_declaration(
			tokens,
			span,
			flags,
			start_of_declaration,
		),
		BaseToken::Struct =>
		{
			parse_struct_declaration(tokens, span, start_of_declaration, flags)
		}
		BaseToken::Word8
		| BaseToken::Word16
		| BaseToken::Word32
		| BaseToken::Word64
		| BaseToken::Word128 => parse_word_declaration(
			tokens,
			span,
			start_of_declaration,
			flags,
			declaring_token,
		),
		_ => Err("unexpected token"),
	}
}

fn parse_import_declaration(
	tokens: &Tokens,
	span: &mut std::ops::Range<usize>,
	start_of_declaration: usize,
	flags: EnumSet<DeclarationFlag>,
) -> Result<Declaration, &'static str>
{
	let filename = consume(BaseToken::StringLiteral, tokens, span)?;
	consume(BaseToken::Semicolon, tokens, span)?;
	// TODO build declaration using filename: TokenId
	Ok(Declaration {})
}

fn parse_constant_declaration(
	tokens: &Tokens,
	span: &mut std::ops::Range<usize>,
	start_of_declaration: usize,
	flags: EnumSet<DeclarationFlag>,
) -> Result<Declaration, &'static str>
{
	let name = consume(BaseToken::Identifier, tokens, span)?;
	// TODO location of declaration
	let start_of_type = span.start;
	let value_type = if consume_optional(BaseToken::Colon, tokens, span)
	{
		let value_type = parse_wellformed_type(tokens, span)?;
		value_type
	}
	else
	{
		return Err("misisng constant type");
	};
	let end_of_type = span.start;
	consume(BaseToken::Assignment, tokens, span)?;
	let expression = parse_expression(tokens, span)?;
	consume(BaseToken::Semicolon, tokens, span)?;
	Ok(Declaration {})
}

fn parse_word_declaration(
	tokens: &Tokens,
	span: &mut std::ops::Range<usize>,
	start_of_declaration: usize,
	mut flags: EnumSet<DeclarationFlag>,
	declaring_token: BaseToken,
) -> Result<Declaration, &'static str>
{
	let name = consume(BaseToken::Identifier, tokens, span)?;
	let members = parse_struct_members(tokens, span)?;
	Ok(Declaration {})
}

fn parse_struct_declaration(
	tokens: &Tokens,
	span: &mut std::ops::Range<usize>,
	start_of_declaration: usize,
	mut flags: EnumSet<DeclarationFlag>,
) -> Result<Declaration, &'static str>
{
	let name = consume(BaseToken::Identifier, tokens, span)?;
	if consume_optional(BaseToken::Semicolon, tokens, span)
	{
		flags.insert(DeclarationFlag::OpaqueStruct);
	}
	else
	{
		let members = parse_struct_members(tokens, span)?;
	}
	Ok(Declaration {})
}

fn parse_struct_members(
	tokens: &Tokens,
	span: &mut std::ops::Range<usize>,
) -> Result<(), &'static str>
{
	let opening_brace = consume(BaseToken::BraceLeft, tokens, span)?;
	while !consume_optional(BaseToken::BraceRight, tokens, span)
	{
		let member = parse_member(tokens, span)?;
		consume(BaseToken::Comma, tokens, span)?;
	}
	Ok(())
}

fn parse_function_declaration(
	tokens: &Tokens,
	span: &mut std::ops::Range<usize>,
	mut flags: EnumSet<DeclarationFlag>,
	start_of_declaration: usize,
) -> Result<Declaration, &'static str>
{
	let name = consume(BaseToken::Identifier, tokens, span)?;
	let signature = parse_rest_of_function_signature(tokens, span)?;
	if consume_optional(BaseToken::Semicolon, tokens, span)
	{
		// FunctionHead
		Ok(Declaration {})
	}
	else
	{
		let body = parse_function_body(tokens, span)?;
		// Function
		Ok(Declaration {})
	}
}

fn parse_rest_of_function_signature(
	tokens: &Tokens,
	span: &mut std::ops::Range<usize>,
) -> Result<(), &'static str>
{
	consume(BaseToken::ParenLeft, tokens, span)?;
	loop
	{
		if consume_optional(BaseToken::ParenRight, tokens, span)
		{
			break;
		}
		let parameter = parse_parameter(tokens, span)?;
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
	let return_type = if consume_optional(BaseToken::Arrow, tokens, span)
	{
		parse_wellformed_type(tokens, span)?;
	}
	else
	{
		// void
	};
	Ok(())
}

fn parse_member(
	tokens: &Tokens,
	span: &mut std::ops::Range<usize>,
) -> Result<(), &'static str>
{
	let name = consume(BaseToken::Identifier, tokens, span)?;
	if consume_optional(BaseToken::Colon, tokens, span)
	{
		let value_type = parse_wellformed_type(tokens, span)?;
		value_type
	}
	else
	{
		return Err("missing member type");
	};
	Ok(())
}

fn parse_parameter(
	tokens: &Tokens,
	span: &mut std::ops::Range<usize>,
) -> Result<(), &'static str>
{
	let name = consume(BaseToken::Identifier, tokens, span)?;
	if consume_optional(BaseToken::Colon, tokens, span)
	{
		let value_type = parse_wellformed_type(tokens, span)?;
		value_type
	}
	else
	{
		return Err("missing parameter type");
	};
	Ok(())
}

fn parse_wellformed_type(
	tokens: &Tokens,
	span: &mut std::ops::Range<usize>,
) -> Result<(), &'static str>
{
	let value_type = parse_inner_type(tokens, span)?;
	// TODO is wellformed
	if true { Ok(()) } else { Err("illegal type") }
}

fn parse_inner_type(
	tokens: &Tokens,
	span: &mut std::ops::Range<usize>,
) -> Result<(), &'static str>
{
	match take(tokens, span)
	{
		BaseToken::ValueTypeKeyword =>
		{
			// TODO where do I store this type?
			// too much indirection during type checking is also bad
			Ok(())
		}
		BaseToken::Identifier =>
		{
			// TODO unresolved struct or word
			Ok(())
		}
		BaseToken::Ampersand =>
		{
			// TODO how do I store this nested type?
			// in a way, the tokens are already a way to store it
			let deref_type = parse_inner_type(tokens, span)?;
			Ok(())
		}
		_ => Err("Expected type keyword."),
	}
}

fn parse_function_body(
	tokens: &Tokens,
	span: &mut std::ops::Range<usize>,
) -> Result<(), &'static str>
{
	consume(BaseToken::BraceLeft, tokens, span)?;
	loop
	{
		if consume_optional(BaseToken::BraceRight, tokens, span)
		{
			break;
		}
		let statement = parse_statement(tokens, span)?;
		// TODO check if statement is return: and handle that
		// unless I switch to a new assignment-based / named returns
	}
	Ok(())
}

fn parse_statement(
	tokens: &Tokens,
	span: &mut std::ops::Range<usize>,
) -> Result<(), &'static str>
{
	Ok(())
}

fn parse_expression(
	tokens: &Tokens,
	span: &mut std::ops::Range<usize>,
) -> Result<(), &'static str>
{
	Ok(())
}

#[inline(always)]
fn take(tokens: &Tokens, span: &mut std::ops::Range<usize>) -> BaseToken
{
	let token = tokens.base_tokens()[span.start];
	span.start += 1;
	token
}

#[inline(always)]
fn consume(
	expected: BaseToken,
	tokens: &Tokens,
	span: &mut std::ops::Range<usize>,
) -> Result<(), &'static str>
{
	let token = tokens.base_tokens()[span.start];
	span.start += 1;
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
		Err(expectation)
	}
}

#[inline(always)]
fn consume_optional(
	expected: BaseToken,
	tokens: &Tokens,
	span: &mut std::ops::Range<usize>,
) -> bool
{
	let found = tokens.base_tokens()[span.start] == expected;
	span.start += usize::from(found);
	found
}
