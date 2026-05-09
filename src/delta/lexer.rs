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

	// Tokens whose source span is a valid identifier.
	Identifier,
	Builtin,

	// Tokens with integer payload.
	NakedDecimal,
	BitInteger,
	SuffixedInteger,
	CharLiteral,
	BoolLiteral,

	// Tokens with Bytes payload.
	StringLiteral,
}

#[derive(Clone, Copy, Debug, strum::FromRepr)]
#[repr(u8)]
pub enum ValueTypeKeyword
{
	NoKeyword = 0,
	Void,
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

	pub fn value_type(self) -> ValueTypeKeyword
	{
		let value_type = (self.value_type_and_payload_id & 0xFF) as u8;
		// TODO the codepath for this panic should be unexpected token
		ValueTypeKeyword::from_repr(value_type).unwrap()
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

impl PayloadId
{
	const NONE: PayloadId = PayloadId(0);
}

struct TokenLocation
{
	start: u32,
	end: u32,
	start_of_line: u32,
	line_number: u32,
}

impl TokenLocation
{
	fn span(&self) -> std::ops::Range<usize>
	{
		self.span_from(self.start)
	}

	fn span_from(&self, from: u32) -> std::ops::Range<usize>
	{
		let start = from as usize;
		let end = self.end as usize;
		start..end
	}
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
	fn empty(
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

		TokenizedBuffer {
			source,
			source_filename,
			tokens,
			token_locations,
			payloads,
		}
	}

	fn push(
		&mut self,
		base_token: BaseToken,
		value_type: Option<ValueTypeKeyword>,
		payload: Option<TokenPayload>,
		location: TokenLocation,
	) -> TokenId
	{
		// TokenId validity is checked upon use.
		let token_id = TokenId(self.tokens.len() as u32);
		let value_type = value_type.unwrap_or(ValueTypeKeyword::NoKeyword);
		let payload_id = if let Some(payload) = payload
		{
			self.push_payload(payload)
		}
		else
		{
			PayloadId::NONE
		};
		let token = Token::new(base_token, token_id, value_type, payload_id);
		self.tokens.push(token);
		self.token_locations.push(location);
		token_id
	}

	fn push_payload(&mut self, payload: TokenPayload) -> PayloadId
	{
		// PayloadId validity is checked upon use.
		let payload_id = PayloadId(self.payloads.len() as u32);
		self.payloads.push(payload);
		payload_id
	}

	fn finalize(self) -> Self
	{
		// TODO turn into lexing error
		assert!(self.tokens.len() < MAX_NUM_TOKENS);

		assert_eq!(self.token_locations.len(), self.tokens.len());
		assert!(self.payloads.len() < 1 + self.tokens.len());
		self
	}

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
	let mut buffer = TokenizedBuffer::empty(source, source_filename);

	let mut iter = source.bytes().enumerate().peekable();
	let mut line_number = 1;
	let mut start_of_line = 0;
	while let Some((i, x)) = iter.next()
	{
		let mut location = TokenLocation {
			start: i as u32,
			end: (i as u32) + 1,
			start_of_line,
			line_number,
		};
		let mut value_type = None;
		let mut payload = None;
		let result = match x
		{
			b'(' => Ok(BaseToken::ParenLeft),
			b')' => Ok(BaseToken::ParenRight),
			b'{' => Ok(BaseToken::BraceLeft),
			b'}' => Ok(BaseToken::BraceRight),
			b'[' => Ok(BaseToken::BracketLeft),
			b']' => Ok(BaseToken::BracketRight),
			b'<' => match iter.peek()
			{
				Some((_, b'<')) =>
				{
					iter.next();
					location.end += 1;
					Ok(BaseToken::ShiftLeft)
				}
				Some((_, b'=')) =>
				{
					iter.next();
					location.end += 1;
					Ok(BaseToken::IsLE)
				}
				_ => Ok(BaseToken::AngleLeft),
			},
			b'>' => match iter.peek()
			{
				Some((_, b'>')) =>
				{
					iter.next();
					location.end += 1;
					Ok(BaseToken::ShiftRight)
				}
				Some((_, b'=')) =>
				{
					iter.next();
					location.end += 1;
					Ok(BaseToken::IsGE)
				}
				_ => Ok(BaseToken::AngleRight),
			},
			b'|' => match iter.peek()
			{
				Some((_, b':')) =>
				{
					iter.next();
					location.end += 1;
					Ok(BaseToken::PipeForType)
				}
				_ => Ok(BaseToken::Pipe),
			},
			b'&' => Ok(BaseToken::Ampersand),
			b'^' => Ok(BaseToken::Caret),
			b'!' => match iter.peek()
			{
				Some((_, b'=')) =>
				{
					iter.next();
					location.end += 1;
					Ok(BaseToken::DoesNotEqual)
				}
				_ => Ok(BaseToken::Exclamation),
			},
			b'+' => Ok(BaseToken::Plus),
			b'*' => Ok(BaseToken::Times),
			b'%' => Ok(BaseToken::Modulo),
			b':' => Ok(BaseToken::Colon),
			b';' => Ok(BaseToken::Semicolon),
			b'.' => match iter.peek()
			{
				Some((_, b'.')) =>
				{
					iter.next();
					location.end += 1;
					Ok(BaseToken::Dots)
				}
				_ => Ok(BaseToken::Dot),
			},
			b',' => Ok(BaseToken::Comma),
			b'=' => match iter.peek()
			{
				Some((_, b'=')) =>
				{
					iter.next();
					location.end += 1;
					Ok(BaseToken::Equals)
				}
				_ => Ok(BaseToken::Assignment),
			},
			b'-' => match iter.peek()
			{
				Some((_, b'>')) =>
				{
					iter.next();
					location.end += 1;
					Ok(BaseToken::Arrow)
				}
				_ => Ok(BaseToken::Minus),
			},
			b'/' => match iter.peek()
			{
				Some((_, b'/')) =>
				{
					iter.next();
					while let Some(_) = iter.next_if(|&(_, y)| y != b'\n')
					{
						// Skipping the rest of the comment.
					}
					continue;
				}
				_ => Ok(BaseToken::Divide),
			},
			b'a'..=b'z' | b'A'..=b'Z' | b'_' =>
			{
				while let Some(&(_, y)) = iter.peek()
				{
					if is_identifier_continuation(y)
					{
						iter.next();
						location.end += 1;
					}
					else
					{
						break;
					}
				}
				let identifier = &source[location.span()];
				let token = match identifier
				{
					"fn" => BaseToken::Fn,
					"var" => BaseToken::Var,
					"const" => BaseToken::Const,
					"if" => BaseToken::If,
					"goto" => BaseToken::Goto,
					"loop" => BaseToken::Loop,
					"else" => BaseToken::Else,
					"cast" => BaseToken::Cast,
					"as" => BaseToken::As,
					"true" =>
					{
						payload = Some(TokenPayload::Integer(1));
						BaseToken::BoolLiteral
					}
					"false" =>
					{
						payload = Some(TokenPayload::Integer(0));
						BaseToken::BoolLiteral
					}
					"bool" =>
					{
						value_type = Some(ValueTypeKeyword::Bool);
						BaseToken::ValueTypeKeyword
					}
					"void" =>
					{
						value_type = Some(ValueTypeKeyword::Void);
						BaseToken::ValueTypeKeyword
					}
					"char8" =>
					{
						value_type = Some(ValueTypeKeyword::Char8);
						BaseToken::ValueTypeKeyword
					}
					"i8" | "i16" | "i32" | "i64" | "i128" | "u8" | "u16"
					| "u32" | "u64" | "u128" | "usize" =>
					{
						value_type =
							Some(parse_integer_suffix(identifier).unwrap());
						BaseToken::ValueTypeKeyword
					}
					"import" => BaseToken::Import,
					"pub" => BaseToken::Pub,
					"extern" => BaseToken::Extern,
					"struct" => BaseToken::Struct,
					"word8" => BaseToken::Word8,
					"word16" => BaseToken::Word16,
					"word32" => BaseToken::Word32,
					"word64" => BaseToken::Word64,
					"word128" => BaseToken::Word128,
					"_" => BaseToken::Placeholder,
					_identifier =>
					{
						if let Some(&(_, b'!')) = iter.peek()
						{
							iter.next();
							location.end += 1;
							BaseToken::Builtin
						}
						else
						{
							BaseToken::Identifier
						}
					}
				};
				Ok(token)
			}
			b'0' =>
			{
				// We have to consume the entire token (0[a-zA-Z0-9_]*)
				// before we can error out and move on to the next token.
				let mut end_of_literal = location.end;
				let value = match iter.peek()
				{
					Some((_, b'x')) =>
					{
						iter.next();
						location.end += 1;
						while let Some(&(_, y)) = iter.peek()
						{
							if y.is_ascii_hexdigit()
							{
								iter.next();
								location.end += 1;
							}
							else if y == b'_'
							{
								iter.next();
								location.end += 1;
							}
							else
							{
								break;
							}
						}
						let literal = &source[location.span()];
						if !literal.is_empty()
						{
							end_of_literal = location.end;
							parse_hex_digits(&literal)
						}
						else
						{
							// 'x' was part of the suffix.
							Ok(0)
						}
					}
					Some((_i, b'b')) =>
					{
						iter.next();
						location.end += 1;
						while let Some(&(_, y)) = iter.peek()
						{
							if y == b'0' || y == b'1'
							{
								iter.next();
								location.end += 1;
							}
							else if y == b'_'
							{
								iter.next();
								location.end += 1;
							}
							else
							{
								break;
							}
						}
						let literal = &source[location.span()];
						if !literal.is_empty()
						{
							end_of_literal = location.end;
							parse_binary_digits(&literal)
						}
						else
						{
							// 'x' was part of the suffix.
							Ok(0)
						}
					}
					_ => Ok(0),
				};
				while let Some(&(_, y)) = iter.peek()
				{
					if is_identifier_continuation(y)
					{
						iter.next();
						location.end += 1;
					}
					else
					{
						break;
					}
				}
				let suffix = &source[location.span_from(end_of_literal)];
				match value
				{
					Ok(0) if location.span().len() == 1 =>
					{
						payload = Some(TokenPayload::Integer(0));
						Ok(BaseToken::NakedDecimal)
					}
					Ok(value) if suffix.is_empty() =>
					{
						payload = Some(TokenPayload::Integer(value));
						Ok(BaseToken::BitInteger)
					}
					Ok(value) => match parse_integer_suffix(&suffix)
					{
						Ok(suffix_type) =>
						{
							payload = Some(TokenPayload::Integer(value));
							value_type = Some(suffix_type);
							Ok(BaseToken::SuffixedInteger)
						}
						Err(err) => Err(err),
					},
					Err(err) => Err(err),
				}
			}
			b'1'..=b'9' =>
			{
				// We have to consume the entire token ([1-9][a-zA-Z0-9_]*)
				// before we can error out and move on to the next token.
				while let Some(&(_, y)) = iter.peek()
				{
					if y.is_ascii_digit()
					{
						iter.next();
						location.end += 1;
					}
					else if y == b'_'
					{
						iter.next();
						location.end += 1;
					}
					else
					{
						break;
					}
				}
				let literal = &source[location.span()];
				let end_of_literal = location.end;
				while let Some(&(_, y)) = iter.peek()
				{
					if is_identifier_continuation(y)
					{
						iter.next();
						location.end += 1;
					}
					else
					{
						break;
					}
				}
				let suffix = &source[location.span_from(end_of_literal)];
				match parse_decimal_digits(literal)
				{
					Ok(value) if suffix.is_empty() =>
					{
						payload = Some(TokenPayload::Integer(value));
						Ok(BaseToken::NakedDecimal)
					}
					Ok(value) => match parse_integer_suffix(&suffix)
					{
						Ok(suffix_type) =>
						{
							payload = Some(TokenPayload::Integer(value));
							value_type = Some(suffix_type);
							Ok(BaseToken::SuffixedInteger)
						}
						Err(err) => Err(err),
					},
					Err(err) => Err(err),
				}
			}
			b'"' | b'\'' =>
			{
				let opening_quote = x;
				let mut bytes = Vec::new();
				let mut closed = false;

				while let Some((_, x)) = iter.next_if(|&(_, y)| y != b'\n')
				{
					location.end += 1;
					if x == b'\\'
					{
						location.end += 1;
						match iter.next()
						{
							Some((_, b'n')) => bytes.push(b'\n'),
							Some((_, b'r')) => bytes.push(b'\r'),
							Some((_, b't')) => bytes.push(b'\t'),
							Some((_, b'\\')) => bytes.push(b'\\'),
							Some((_, b'\'')) => bytes.push(b'\''),
							Some((_, b'\"')) => bytes.push(b'\"'),
							Some((_, b'0')) => bytes.push(b'\0'),
							Some((_, b'x')) =>
							{
								let start_of_digits = location.end;
								let end_of_digits = start_of_digits + 2;
								while let Some(&(_, y)) = iter.peek()
								{
									if y.is_ascii_hexdigit()
									{
										iter.next();
										location.end += 1;

										if location.end == end_of_digits
										{
											break;
										}
									}
									else
									{
										break;
									}
								}
								let digits = &source
									[location.span_from(start_of_digits)];
								if digits.len() == 2
								{
									let byte =
										parse_hex_digits(&digits).unwrap();
									bytes.push(byte as u8);
								}
								else
								{
									// TODO error
								}
							}
							Some((_, b'u')) =>
							{
								let start_of_digits = location.end + 1;
								let end_of_digits = start_of_digits + 4;
								let mut is_closed = false;
								if let Some((_, b'{')) = iter.peek()
								{
									iter.next();
									location.end += 1;

									while let Some(&(_, y)) = iter.peek()
									{
										if y.is_ascii_hexdigit()
										{
											iter.next();
											location.end += 1;
										}
										else if y == b'}'
										{
											is_closed = true;
											iter.next();
											location.end += 1;
											break;
										}
										else
										{
											break;
										}
									}
								};
								let digits = is_closed.then(|| {
									&source[location.span_from(start_of_digits)]
								});
								let c = digits
									.and_then(|digits| {
										parse_hex_digits(digits).ok()
									})
									.and_then(|x| char::from_u32(x as u32));
								if let Some(c) = c
								{
									let mut buffer = [0; 4];
									let slice = c.encode_utf8(&mut buffer);
									bytes.extend_from_slice(slice.as_bytes());
								}
								else
								{
									// TODO error
								}
							}
							Some((_, _y)) =>
							{
								// TODO error
							}
							None =>
							{
								// TODO error
							}
						}
					}
					else if x == opening_quote
					{
						closed = true;
						break;
					}
					else if x == b' '
					{
						bytes.push(b' ');
					}
					else if x.is_ascii_graphic()
					{
						for byte in x.to_string().as_bytes()
						{
							bytes.push(*byte);
						}
					}
					else if x.is_ascii()
					{
						// TODO error
					}
					else
					{
						debug_assert!(x >= 128);
						for byte in x.to_string().as_bytes()
						{
							bytes.push(*byte);
						}
					}
				}
				if !closed
				{
					// TODO error
				}
				if opening_quote == b'"'
				{
					payload = Some(TokenPayload::Bytes(bytes));
					Ok(BaseToken::StringLiteral)
				}
				else if bytes.len() == 1
				{
					let value = u128::from(bytes[0]);
					payload = Some(TokenPayload::Integer(value));
					Ok(BaseToken::CharLiteral)
				}
				else
				{
					// Err(Error::InvalidCharLiteral)
					Err(())
				}
			}
			b' ' | b'\t' =>
			{
				continue;
			}
			b'\r' =>
			{
				continue;
			}
			b'\n' =>
			{
				todo!()
			}
			// _ => Err(Error::UnexpectedCharacter),
			_ => Err(()),
		};
		match result
		{
			Ok(base_token) =>
			{
				buffer.push(base_token, value_type, payload, location);
			}
			Err(_) => unimplemented!(),
		}
	}

	buffer.finalize()
}

fn parse_integer_suffix(suffix: &str) -> Result<ValueTypeKeyword, ()>
{
	match suffix
	{
		"i8" => Ok(ValueTypeKeyword::Int8),
		"i16" => Ok(ValueTypeKeyword::Int16),
		"i32" => Ok(ValueTypeKeyword::Int32),
		"i64" => Ok(ValueTypeKeyword::Int64),
		"i128" => Ok(ValueTypeKeyword::Int128),
		"u8" => Ok(ValueTypeKeyword::Uint8),
		"u16" => Ok(ValueTypeKeyword::Uint16),
		"u32" => Ok(ValueTypeKeyword::Uint32),
		"u64" => Ok(ValueTypeKeyword::Uint64),
		"u128" => Ok(ValueTypeKeyword::Uint128),
		"usize" => Ok(ValueTypeKeyword::Usize),
		_ => Err(()),
	}
}

fn is_identifier_continuation(x: u8) -> bool
{
	matches!(x, b'a'..=b'z' | b'A'..=b'Z' | b'0'..=b'9' | b'_')
}

// It is a bit weird that the digits are validated but the length is not.
fn parse_binary_digits(validated_digits: &str) -> Result<u128, ()>
{
	// TODO
	Err(())
}

fn parse_decimal_digits(validated_digits: &str) -> Result<u128, ()>
{
	// TODO
	Err(())
}

fn parse_hex_digits(validated_digits: &str) -> Result<u128, ()>
{
	// TODO
	Err(())
}
