mod digits;
pub mod tokens;

pub use crate::alpha::lexer::Error as LexingError;

use digits::{parse_binary_digits, parse_decimal_digits, parse_hex_digits};
use tokens::*;

#[derive(Clone, Copy, Debug, PartialEq)]
#[derive(strum::FromRepr, strum::Display, strum::EnumIter)]
#[repr(u8)]
#[strum(serialize_all = "lowercase")]
pub enum BaseToken
{
	EndOfSource = 0,

	// Single-character tokens.
	#[strum(serialize = "(")]
	ParenLeft,
	#[strum(serialize = ")")]
	ParenRight,
	// "{"
	BraceLeft,
	// "}"
	BraceRight,
	#[strum(serialize = "[")]
	BracketLeft,
	#[strum(serialize = "]")]
	BracketRight,
	#[strum(serialize = "<")]
	AngleLeft,
	#[strum(serialize = ">")]
	AngleRight,
	#[strum(serialize = "|")]
	Pipe,
	#[strum(serialize = "&")]
	Ampersand,
	#[strum(serialize = "^")]
	Caret,
	#[strum(serialize = "!")]
	Exclamation,
	#[strum(serialize = "_")]
	Placeholder,
	#[strum(serialize = "+")]
	Plus,
	#[strum(serialize = "-")]
	Minus,
	#[strum(serialize = "*")]
	Times,
	#[strum(serialize = "/")]
	Divide,
	#[strum(serialize = "%")]
	Modulo,
	#[strum(serialize = ":")]
	Colon,
	#[strum(serialize = ";")]
	Semicolon,
	#[strum(serialize = ".")]
	Dot,
	#[strum(serialize = ",")]
	Comma,
	#[strum(serialize = "=")]
	Assignment,

	// Double-character tokens.
	#[strum(serialize = "==")]
	Equals,
	#[strum(serialize = "!=")]
	DoesNotEqual,
	#[strum(serialize = ">=")]
	IsGE,
	#[strum(serialize = "<=")]
	IsLE,
	#[strum(serialize = "<<")]
	ShiftLeft,
	#[strum(serialize = ">>")]
	ShiftRight,
	#[strum(serialize = "->")]
	Arrow,
	#[strum(serialize = "|:")]
	PipeForType,
	#[strum(serialize = "..")]
	Dots,

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

	// Tokens whose source span can be decoded as a string literal.
	StringLiteral,

	// Placeholder.
	Error,
}

#[derive(Clone, Copy, Debug, PartialEq)]
#[derive(strum::FromRepr, strum::Display, strum::EnumIter)]
#[repr(u8)]
#[strum(serialize_all = "lowercase")]
pub enum ValueTypeKeyword
{
	NoKeyword = 0,
	Void,
	#[strum(serialize = "i8")]
	Int8,
	#[strum(serialize = "i16")]
	Int16,
	#[strum(serialize = "i32")]
	Int32,
	#[strum(serialize = "i64")]
	Int64,
	#[strum(serialize = "i128")]
	Int128,
	#[strum(serialize = "u8")]
	Uint8,
	#[strum(serialize = "u16")]
	Uint16,
	#[strum(serialize = "u32")]
	Uint32,
	#[strum(serialize = "u64")]
	Uint64,
	#[strum(serialize = "u128")]
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
}

pub fn lex(source: &str, source_filename: &str) -> Tokens
{
	if source.len() > MAX_SOURCE_LEN
	{
		let mut buffer = Tokens::empty(source_filename.to_string(), 0);
		let dummy_location = TokenLocation {
			start: 0,
			end: 0,
			start_of_line: 0,
			line_number: 0,
		};
		buffer.push_error(LexingError::TooManySourceBytes, dummy_location);
		return buffer;
	}

	let mut buffer = Tokens::empty(source_filename.to_string(), source.len());

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
				line_number += 1;
				start_of_line = location.end;
				continue;
			}
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
						let start_of_literal = location.end;
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
						let literal =
							&source[location.span_from(start_of_literal)];
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
						let start_of_literal = location.end;
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
						let literal =
							&source[location.span_from(start_of_literal)];
						if !literal.is_empty()
						{
							end_of_literal = location.end;
							parse_binary_digits(&literal)
						}
						else
						{
							// 'b' was part of the suffix.
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
			b'\'' =>
			{
				let opening_quote = x;
				let mut num_bytes = 0;
				let mut byte = 0u8;
				let mut push_byte = |b: u8| {
					byte = b;
					num_bytes += 1;
				};
				let mut closed = false;
				let mut first_error = None;

				while let Some((_, x)) = iter.next_if(|&(_, y)| y != b'\n')
				{
					location.end += 1;
					if x == b'\\'
					{
						let start_of_escape = location.end - 1;
						location.end += 1;
						match iter.next()
						{
							Some((_, b'n')) => push_byte(b'\n'),
							Some((_, b'r')) => push_byte(b'\r'),
							Some((_, b't')) => push_byte(b'\t'),
							Some((_, b'\\')) => push_byte(b'\\'),
							Some((_, b'\'')) => push_byte(b'\''),
							Some((_, b'\"')) => push_byte(b'\"'),
							Some((_, b'0')) => push_byte(b'\0'),
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
									push_byte(byte as u8);
								}
								else if first_error.is_none()
								{
									first_error = Some((
										LexingError::InvalidEscapeSequence,
										TokenLocation {
											start: start_of_escape,
											..location
										},
									));
								}
							}
							Some((_, _y)) =>
							{
								if first_error.is_none()
								{
									first_error = Some((
										LexingError::InvalidEscapeSequence,
										TokenLocation {
											start: start_of_escape,
											..location
										},
									));
								}
							}
							None =>
							{
								location.end -= 1;
								if first_error.is_none()
								{
									first_error = Some((
										LexingError::UnexpectedTrailingBackslash,
										TokenLocation {
											start: start_of_escape,
											..location
										})
									);
								}
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
						push_byte(b' ');
					}
					else if x.is_ascii_graphic()
					{
						push_byte(x);
					}
					else if x.is_ascii()
					{
						if first_error.is_none()
						{
							first_error = Some((
								LexingError::UnexpectedCharacter,
								TokenLocation {
									start: location.end - 1,
									..location
								},
							));
						}
					}
					else
					{
						debug_assert!(x >= 128);
						push_byte(x)
					}
				}
				if !closed
				{
					if first_error.is_none()
					{
						first_error = Some((
							LexingError::MissingClosingQuote,
							TokenLocation {
								start: location.end,
								..location
							},
						));
					}
				}
				if let Some((error, error_location)) = first_error
				{
					location = error_location;
					Err(error)
				}
				else if num_bytes == 1
				{
					let value = u128::from(byte);
					payload = Some(TokenPayload::Integer(value));
					Ok(BaseToken::CharLiteral)
				}
				else
				{
					Err(LexingError::InvalidCharLiteral)
				}
			}
			b'"' =>
			{
				let opening_quote = x;
				let push_byte = |b: u8| {};
				let mut closed = false;
				let mut first_error = None;

				while let Some((_, x)) = iter.next_if(|&(_, y)| y != b'\n')
				{
					location.end += 1;
					if x == b'\\'
					{
						let start_of_escape = location.end - 1;
						location.end += 1;
						match iter.next()
						{
							Some((_, b'n')) => push_byte(b'\n'),
							Some((_, b'r')) => push_byte(b'\r'),
							Some((_, b't')) => push_byte(b'\t'),
							Some((_, b'\\')) => push_byte(b'\\'),
							Some((_, b'\'')) => push_byte(b'\''),
							Some((_, b'\"')) => push_byte(b'\"'),
							Some((_, b'0')) => push_byte(b'\0'),
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
									push_byte(byte as u8);
								}
								else if first_error.is_none()
								{
									first_error = Some((
										LexingError::InvalidEscapeSequence,
										TokenLocation {
											start: start_of_escape,
											..location
										},
									));
								}
							}
							Some((_, b'u')) =>
							{
								let mut digits = None;
								if let Some((_, b'{')) = iter.peek()
								{
									iter.next();
									location.end += 1;
									let start_of_digits = location.end;

									while let Some(&(_, y)) = iter.peek()
									{
										if y.is_ascii_hexdigit()
										{
											iter.next();
											location.end += 1;
										}
										else if y == b'}'
										{
											if location.end > start_of_digits
											{
												digits = Some(
													&source[location
														.span_from(
															start_of_digits,
														)],
												);
											}
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
								let c = digits
									.and_then(|digits| {
										parse_hex_digits(digits).ok()
									})
									.and_then(|x| char::from_u32(x as u32));
								if let Some(c) = c
								{
									let mut buffer = [0; 4];
									let slice = c.encode_utf8(&mut buffer);
									for &byte in slice.as_bytes()
									{
										push_byte(byte)
									}
								}
								else if first_error.is_none()
								{
									first_error = Some((
										LexingError::InvalidEscapeSequence,
										TokenLocation {
											start: start_of_escape,
											..location
										},
									));
								}
							}
							Some((_, _y)) =>
							{
								if first_error.is_none()
								{
									first_error = Some((
										LexingError::InvalidEscapeSequence,
										TokenLocation {
											start: start_of_escape,
											..location
										},
									));
								}
							}
							None =>
							{
								location.end -= 1;
								if first_error.is_none()
								{
									first_error = Some((
										LexingError::UnexpectedTrailingBackslash,
										TokenLocation {
											start: start_of_escape,
											..location
										})
									);
								}
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
						push_byte(b' ');
					}
					else if x.is_ascii_graphic()
					{
						push_byte(x);
					}
					else if x.is_ascii()
					{
						if first_error.is_none()
						{
							first_error = Some((
								LexingError::UnexpectedCharacter,
								TokenLocation {
									start: location.end - 1,
									..location
								},
							));
						}
					}
					else
					{
						debug_assert!(x >= 128);
						push_byte(x);
					}
				}
				if !closed
				{
					if first_error.is_none()
					{
						first_error = Some((
							LexingError::MissingClosingQuote,
							TokenLocation {
								start: location.end,
								..location
							},
						));
					}
				}
				if let Some((error, error_location)) = first_error
				{
					location = error_location;
					Err(error)
				}
				else
				{
					Ok(BaseToken::StringLiteral)
				}
			}
			_ => Err(LexingError::UnexpectedCharacter),
		};
		match result
		{
			Ok(base_token) =>
			{
				buffer.push(base_token, value_type, payload, location);
			}
			Err(error) =>
			{
				buffer.push_error(error, location);
			}
		}
	}

	let end_of_source_location = TokenLocation {
		start: source.len() as u32,
		end: source.len() as u32,
		start_of_line,
		line_number,
	};

	buffer.finalize(end_of_source_location)
}

fn parse_integer_suffix(suffix: &str) -> Result<ValueTypeKeyword, LexingError>
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
		_ => Err(LexingError::InvalidIntegerTypeSuffix),
	}
}

pub(crate) fn is_identifier_continuation(x: u8) -> bool
{
	matches!(x, b'a'..=b'z' | b'A'..=b'Z' | b'0'..=b'9' | b'_')
}
