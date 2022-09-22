/**/

use thiserror::Error;

use crate::common::*;

#[derive(Debug, PartialEq, Eq)]
pub enum Token
{
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
	DebugDollar,
	Plus,
	Minus,
	Times,
	Divide,
	Modulo,
	Colon,
	Semicolon,
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

	// Keywords.
	Fn,
	Var,
	Const,
	If,
	Goto,
	Loop,
	Else,
	Pub,
	Extern,

	// Literals.
	Identifier(String),
	NakedInteger(i128),
	BitInteger(u64),
	Int8(i8),
	Int16(i16),
	Int32(i32),
	Int64(i64),
	Int128(i128),
	Uint8(u8),
	Uint16(u16),
	Uint32(u32),
	Uint64(u64),
	Uint128(u128),
	Usize(usize),
	Bool(bool),
	StringLiteral
	{
		bytes: Vec<u8>,
		value_type: Option<ValueType>,
	},

	// Types.
	Type(ValueType),
}

#[derive(Debug, Error)]
pub enum Error
{
	#[error("unexpected character {character:?}")]
	UnexpectedCharacter
	{
		character: char
	},
	#[error("invalid integer literal")]
	InvalidIntegerLiteral(#[from] std::num::ParseIntError),
	#[error("large integer literal without type suffix")]
	InvalidNakedIntegerLiteral,
	#[error("invalid integer type suffix '\\{sequence:?}'")]
	InvalidIntegerTypeSuffix
	{
		sequence: String
	},
	#[error("invalid escape sequence '\\{sequence:?}'")]
	InvalidEscapeSequence
	{
		sequence: String
	},
	#[error("unexpected trailing backslash")]
	UnexpectedTrailingBackslash,
	#[error("missing closing quote")]
	MissingClosingQuote,
	#[error("invalid mixed string")]
	InvalidMixedString,
}

#[derive(Debug, Clone)]
pub struct Location
{
	source_filename: String,
	line: String,
	line_number: usize,
	line_offset: usize,
}

impl Location
{
	pub fn format(&self) -> String
	{
		format!(
			"at {}:{}:{} ('{}')",
			self.source_filename, self.line_number, self.line_offset, self.line
		)
	}
}

#[derive(Debug)]
pub struct LexedToken
{
	pub result: Result<Token, Error>,
	pub location: Location,
}

#[must_use]
pub fn lex(source: &str, source_filename: &str) -> Vec<LexedToken>
{
	let mut tokens = Vec::new();
	for (line_number, line) in source.lines().enumerate()
	{
		// Syntax should remain such that each line can be lexed independently.
		lex_line(line, source_filename, line_number, &mut tokens);
	}
	tokens
}

fn lex_line(
	line: &str,
	source_filename: &str,
	line_number: usize,
	tokens: &mut Vec<LexedToken>,
)
{
	let mut iter = line.chars().enumerate().peekable();
	while let Some((line_offset, x)) = iter.next()
	{
		let location = Location {
			source_filename: source_filename.to_string(),
			line: line.to_string(),
			line_number,
			line_offset,
		};
		let result = match x
		{
			'(' => Ok(Token::ParenLeft),
			')' => Ok(Token::ParenRight),
			'{' => Ok(Token::BraceLeft),
			'}' => Ok(Token::BraceRight),
			'[' => Ok(Token::BracketLeft),
			']' => Ok(Token::BracketRight),
			'<' => match iter.peek()
			{
				Some((_, '<')) =>
				{
					iter.next();
					Ok(Token::ShiftLeft)
				}
				Some((_, '=')) =>
				{
					iter.next();
					Ok(Token::IsLE)
				}
				_ => Ok(Token::AngleLeft),
			},
			'>' => match iter.peek()
			{
				Some((_, '>')) =>
				{
					iter.next();
					Ok(Token::ShiftRight)
				}
				Some((_, '=')) =>
				{
					iter.next();
					Ok(Token::IsGE)
				}
				_ => Ok(Token::AngleRight),
			},
			'|' => Ok(Token::Pipe),
			'&' => Ok(Token::Ampersand),

			'^' => Ok(Token::Caret),
			'!' => match iter.peek()
			{
				Some((_, '=')) =>
				{
					iter.next();
					Ok(Token::DoesNotEqual)
				}
				_ => Ok(Token::Exclamation),
			},
			'+' => Ok(Token::Plus),
			'*' => Ok(Token::Times),
			'%' => Ok(Token::Modulo),
			':' => Ok(Token::Colon),
			';' => Ok(Token::Semicolon),
			',' => Ok(Token::Comma),
			'=' => match iter.peek()
			{
				Some((_, '=')) =>
				{
					iter.next();
					Ok(Token::Equals)
				}
				_ => Ok(Token::Assignment),
			},
			'-' => match iter.peek()
			{
				Some((_, '>')) =>
				{
					iter.next();
					Ok(Token::Arrow)
				}
				_ => Ok(Token::Minus),
			},
			'/' => match iter.peek()
			{
				Some((_, '/')) =>
				{
					break;
				}
				_ => Ok(Token::Divide),
			},
			'a'..='z' | 'A'..='Z' | '_' =>
			{
				let mut identifier = x.to_string();
				while let Some(&(_, y)) = iter.peek()
				{
					if is_identifier_continuation(y)
					{
						identifier.push(y);
						iter.next();
					}
					else
					{
						break;
					}
				}
				let token = match identifier.as_str()
				{
					"fn" => Token::Fn,
					"var" => Token::Var,
					"const" => Token::Const,
					"if" => Token::If,
					"goto" => Token::Goto,
					"loop" => Token::Loop,
					"else" => Token::Else,
					"true" => Token::Bool(true),
					"false" => Token::Bool(false),
					"i8" => Token::Type(ValueType::Int8),
					"i16" => Token::Type(ValueType::Int16),
					"i32" => Token::Type(ValueType::Int32),
					"i64" => Token::Type(ValueType::Int64),
					"i128" => Token::Type(ValueType::Int128),
					"u8" => Token::Type(ValueType::Uint8),
					"u16" => Token::Type(ValueType::Uint16),
					"u32" => Token::Type(ValueType::Uint32),
					"u64" => Token::Type(ValueType::Uint64),
					"u128" => Token::Type(ValueType::Uint128),
					"usize" => Token::Type(ValueType::Usize),
					"bool" => Token::Type(ValueType::Bool),
					"char" => Token::Type(ValueType::Char),
					"pub" => Token::Pub,
					"extern" => Token::Extern,
					_ => Token::Identifier(identifier),
				};
				Ok(token)
			}
			'0' => match iter.peek()
			{
				Some((_i, 'x')) =>
				{
					iter.next();
					let mut literal = x.to_string();
					while let Some(&(_, y)) = iter.peek()
					{
						if y.is_digit(16)
						{
							literal.push(y);
							iter.next();
						}
						else
						{
							break;
						}
					}
					u64::from_str_radix(&literal, 16)
						.map(|value| Token::BitInteger(value))
						.map_err(|e| e.into())
				}
				Some((_i, 'b')) =>
				{
					iter.next();
					let mut literal = x.to_string();
					while let Some(&(_, y)) = iter.peek()
					{
						if y.is_digit(2)
						{
							literal.push(y);
							iter.next();
						}
						else
						{
							break;
						}
					}
					u64::from_str_radix(&literal, 2)
						.map(|value| Token::BitInteger(value))
						.map_err(|e| e.into())
				}
				_ =>
				{
					let mut suffix = String::new();
					while let Some(&(_, y)) = iter.peek()
					{
						if is_identifier_continuation(y)
						{
							suffix.push(y);
							iter.next();
						}
						else
						{
							break;
						}
					}
					match suffix.as_str()
					{
						"" => Ok(Token::NakedInteger(0)),
						"i8" => Ok(Token::Int8(0)),
						"i16" => Ok(Token::Int16(0)),
						"i32" => Ok(Token::Int32(0)),
						"i64" => Ok(Token::Int64(0)),
						"i128" => Ok(Token::Int128(0)),
						"u8" => Ok(Token::Uint8(0)),
						"u16" => Ok(Token::Uint16(0)),
						"u32" => Ok(Token::Uint32(0)),
						"u64" => Ok(Token::Uint64(0)),
						"u128" => Ok(Token::Uint128(0)),
						"usize" => Ok(Token::Usize(0)),
						_ => Err(Error::InvalidIntegerTypeSuffix {
							sequence: suffix,
						}),
					}
				}
			},
			'1'..='9' =>
			{
				let mut literal = x.to_string();
				while let Some(&(_, y)) = iter.peek()
				{
					if y.is_digit(10)
					{
						literal.push(y);
						iter.next();
					}
					else
					{
						break;
					}
				}
				let mut suffix = String::new();
				while let Some(&(_, y)) = iter.peek()
				{
					if is_identifier_continuation(y)
					{
						suffix.push(y);
						iter.next();
					}
					else
					{
						break;
					}
				}
				if suffix.is_empty()
				{
					match literal.parse()
					{
						Ok(value)
							if value >= i64::MIN as i128
								&& value <= u64::MAX as i128 =>
						{
							Ok(Token::NakedInteger(value))
						}
						Ok(_) => Err(Error::InvalidNakedIntegerLiteral),
						Err(error) => Err(error.into()),
					}
				}
				else
				{
					match suffix.as_str()
					{
						"i8" => literal
							.parse()
							.map(|value| Token::Int8(value))
							.map_err(|e| e.into()),
						"i16" => literal
							.parse()
							.map(|value| Token::Int16(value))
							.map_err(|e| e.into()),
						"i32" => literal
							.parse()
							.map(|value| Token::Int32(value))
							.map_err(|e| e.into()),
						"i64" => literal
							.parse()
							.map(|value| Token::Int64(value))
							.map_err(|e| e.into()),
						"i128" => literal
							.parse()
							.map(|value| Token::Int128(value))
							.map_err(|e| e.into()),
						"u8" => literal
							.parse()
							.map(|value| Token::Uint8(value))
							.map_err(|e| e.into()),
						"u16" => literal
							.parse()
							.map(|value| Token::Uint16(value))
							.map_err(|e| e.into()),
						"u32" => literal
							.parse()
							.map(|value| Token::Uint32(value))
							.map_err(|e| e.into()),
						"u64" => literal
							.parse()
							.map(|value| Token::Uint64(value))
							.map_err(|e| e.into()),
						"u128" => literal
							.parse()
							.map(|value| Token::Uint128(value))
							.map_err(|e| e.into()),
						"usize" => literal
							.parse()
							.map(|value| Token::Usize(value))
							.map_err(|e| e.into()),
						_ => Err(Error::InvalidIntegerTypeSuffix {
							sequence: suffix,
						}),
					}
				}
			}
			'"' =>
			{
				let mut bytes = Vec::new();
				let mut closed = false;
				let mut end_of_line_offset = line_offset + 1;
				let mut must_be_utf8 = false;
				let mut must_be_bytestring = false;

				while let Some((inner_line_offset, x)) = iter.next()
				{
					end_of_line_offset = inner_line_offset + 1;
					if x == '\\'
					{
						match iter.next()
						{
							Some((_, 'n')) => bytes.push(b'\n'),
							Some((_, 'r')) => bytes.push(b'\r'),
							Some((_, '\\')) => bytes.push(b'\\'),
							Some((_, '\'')) => bytes.push(b'\''),
							Some((_, '\"')) => bytes.push(b'\"'),
							Some((_, 'x')) =>
							{
								must_be_bytestring = true;

								let mut byte = None;
								let mut digits = String::new();
								while let Some(&(_, y)) = iter.peek()
								{
									if y.is_digit(16)
									{
										digits.push(y);
										iter.next();
									}
									else
									{
										break;
									}

									if digits.len() < 2
									{
										continue;
									}

									byte = match u8::from_str_radix(&digits, 16)
									{
										Ok(v) => Some(v),
										Err(_error) => None,
									};
								}
								if let Some(byte) = byte
								{
									bytes.push(byte);
								}
								else
								{
									let error = Error::InvalidEscapeSequence {
										sequence: format!("x{}", digits),
									};
									let warning = LexedToken {
										result: Err(error),
										location: Location {
											line_offset: end_of_line_offset,
											..location.clone()
										},
									};
									tokens.push(warning);
								}
							}
							Some((_, y)) =>
							{
								let warning = LexedToken {
									result: Err(Error::InvalidEscapeSequence {
										sequence: y.to_string(),
									}),
									location: Location {
										line_offset: end_of_line_offset,
										..location.clone()
									},
								};
								tokens.push(warning);
							}
							None =>
							{
								let warning = LexedToken {
									result: Err(
										Error::UnexpectedTrailingBackslash,
									),
									location: Location {
										line_offset: end_of_line_offset,
										..location.clone()
									},
								};
								tokens.push(warning);
							}
						}
					}
					else if x == '\"'
					{
						closed = true;
						break;
					}
					else if x == ' '
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
						let warning = LexedToken {
							result: Err(Error::UnexpectedCharacter {
								character: x,
							}),
							location: Location {
								line_offset: end_of_line_offset,
								..location.clone()
							},
						};
						tokens.push(warning);
					}
					else
					{
						for byte in x.to_string().as_bytes()
						{
							bytes.push(*byte);
						}
						must_be_utf8 = true;
					}
				}
				if !closed
				{
					let warning = LexedToken {
						result: Err(Error::MissingClosingQuote),
						location: Location {
							line_offset: end_of_line_offset,
							..location.clone()
						},
					};
					tokens.push(warning);
				}
				if must_be_bytestring && must_be_utf8
				{
					Err(Error::InvalidMixedString)
				}
				else
				{
					let value_type = if must_be_utf8
					{
						Some(ValueType::String)
					}
					else if must_be_bytestring
					{
						Some(ValueType::for_byte_string())
					}
					else
					{
						None
					};
					Ok(Token::StringLiteral { bytes, value_type })
				}
			}
			' ' | '\t' => continue,
			_ => Err(Error::UnexpectedCharacter { character: x }),
		};
		tokens.push(LexedToken { result, location });
	}
}

fn is_identifier_continuation(x: char) -> bool
{
	match x
	{
		'a'..='z' | 'A'..='Z' | '0'..='9' | '_' => true,
		_ => false,
	}
}
