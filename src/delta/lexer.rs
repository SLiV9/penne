pub use crate::alpha::lexer::Error;
pub use crate::alpha::lexer::Location;

#[derive(Clone, Copy, Debug, PartialEq, strum::FromRepr)]
#[repr(u8)]
pub enum BaseToken
{
	EndOfSource = 0,

	// Single-character tokens.
	ParenLeft,
	ParenRight,
	BraceLeft,
	BraceRight,
	BracketLeft,
	BracketRight,
	AngleLeft,
	AngleRight,
	Pipe,
	Ampersand,
	Caret,
	Exclamation,
	Placeholder,
	Plus,
	Minus,
	Times,
	Divide,
	Modulo,
	Colon,
	Semicolon,
	Dot,
	Comma,
	Assignment, // =

	// Double-character tokens.
	Equals,       // ==
	DoesNotEqual, // !=
	IsGE,         // >=
	IsLE,         // <=
	ShiftLeft,    // <<
	ShiftRight,   // >>
	Arrow,        // ->
	PipeForType,  // |:
	Dots,         // ..

	// Keywords.
	Fn,
	Var,
	Const,
	If,
	Goto,
	Loop,
	Else,
	Cast,
	As,
	Import,
	Pub,
	Extern,
	Struct,
	Word8,
	Word16,
	Word32,
	Word64,
	Word128,

	// Type keywords.
	ValueTypeKeyword,

	// Dynamically sized tokens.
	Identifier,
	Builtin,
	NakedDecimal,
	BitInteger,
	SuffixedInteger,
	CharLiteral,
	BoolLiteral,
	StringLiteral,
	Whitespace,
	Comment,
}

#[derive(Clone, Copy, Debug, strum::FromRepr)]
#[repr(u8)]
pub enum ValueTypeKeyword
{
	Void = 1,
	Int8,
	Int16,
	Int32,
	Int64,
	Int128,
	Uint8,
	Uint16,
	Uint32,
	Uint64,
	Uint128,
	Usize,
	Char8,
	Bool,
}

#[derive(Debug)]
pub enum TokenPayload
{
	UnreachablePayload,
	Identifier(String),
	Integer(u128),
	Bytes(Vec<u8>),
}

#[derive(Clone, Copy, Debug)]
pub struct Token
{
	base_token_and_token_id: u32,
	value_type_and_payload_id: u32,
}

impl Token
{
	fn new(
		base_token: BaseToken,
		TokenId(token_id): TokenId,
		value_type: ValueTypeKeyword,
		PayloadId(payload_id): PayloadId,
	) -> Token
	{
		let base_token = u32::from(base_token as u8);
		let value_type = u32::from(value_type as u8);
		Token {
			base_token_and_token_id: base_token | (token_id << 8),
			value_type_and_payload_id: value_type | (payload_id << 8),
		}
	}

	pub fn base_token(self) -> BaseToken
	{
		let base_token = (self.base_token_and_token_id & 0xFF) as u8;
		// TODO the codepath for this panic should be unexpected token
		BaseToken::from_repr(base_token).unwrap()
	}

	pub fn token_id(self) -> TokenId
	{
		let token_id = self.base_token_and_token_id >> 8;
		TokenId(token_id)
	}

	pub fn value_type(self) -> Option<ValueTypeKeyword>
	{
		let value_type = (self.value_type_and_payload_id & 0xFF) as u8;
		// TODO this codepath should be merged with unexpected token
		if value_type == 0
		{
			return None;
		}
		// TODO the codepath for this panic should be unexpected token
		let keyword = ValueTypeKeyword::from_repr(value_type).unwrap();
		Some(keyword)
	}

	pub fn payload_id(self) -> PayloadId
	{
		let payload_id = self.value_type_and_payload_id >> 8;
		// PayloadId validity is checked on use.
		PayloadId(payload_id)
	}
}

#[derive(Clone, Copy, Debug)]
pub struct TokenId(u32);

#[derive(Clone, Copy, Debug)]
pub struct PayloadId(u32);

struct TokenLocation
{
	index_in_source: u32,
	line_number: u32,
}

pub struct TokenizedBuffer<'source>
{
	source: &'source str,
	source_filename: &'source str,

	// Tokens (SOA)
	tokens: Vec<Token>,
	token_locations: Vec<TokenLocation>,

	payloads: Vec<TokenPayload>,
}

impl<'source> TokenizedBuffer<'source>
{
	pub fn get_payload(
		&self,
		PayloadId(payload_id): PayloadId,
	) -> Option<&TokenPayload>
	{
		if payload_id == 0
		{
			return None;
		}
		self.payloads.get(payload_id as usize)
	}
}

const MAX_SOURCE_LEN: usize = 1 << 31;
const MAX_NUM_TOKENS: usize = 1 << 23;

pub fn lex<'source>(
	source: &'source str,
	source_filename: &'source str,
) -> TokenizedBuffer<'source>
{
	// TODO turn into lexing error
	assert!(source.len() < MAX_SOURCE_LEN);

	let mut tokens = Vec::new();
	let mut token_locations = Vec::new();
	let mut payloads = Vec::new();

	// This payload is unreachable because 0 is an invalid PayloadId.
	payloads.push(TokenPayload::UnreachablePayload);

	// TODO

	// TODO turn into lexing error
	assert!(tokens.len() < MAX_NUM_TOKENS);

	assert_eq!(token_locations.len(), tokens.len());
	assert!(payloads.len() < tokens.len());

	TokenizedBuffer {
		source,
		source_filename,
		tokens,
		token_locations,
		payloads,
	}
}
