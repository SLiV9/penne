//
// Part of penne
// Copyright (c) 2020 Sander in 't Veld
// License: MIT
//

//! The lexer cuts each line of source code into a sequence of tokens.

use ariadne::Label;

use crate::common::*;

#[derive(Debug, PartialEq)]
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

	// Keywords.
	Fn,
	Var,
	Const,
	If,
	Goto,
	Loop,
	Else,
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
	},

	// Types.
	Type(ValueType),
}

#[derive(Debug, Clone)]
pub enum Error
{
	UnexpectedCharacter,
	InvalidIntegerLiteral(std::num::ParseIntError),
	InvalidNakedIntegerLiteral,
	InvalidBitIntegerLiteral,
	InvalidIntegerTypeSuffix,
	InvalidEscapeSequence,
	UnexpectedTrailingBackslash,
	MissingClosingQuote,
	InvalidMixedString,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Location
{
	pub source_filename: String,
	pub span: std::ops::Range<usize>,
	pub line_number: usize,
	pub line_offset: usize,
}

impl Location
{
	pub fn format(&self) -> String
	{
		format!(
			"at {}:{}:{}",
			self.source_filename, self.line_number, self.line_offset
		)
	}

	pub fn label(&self) -> Label<(String, std::ops::Range<usize>)>
	{
		Label::new((self.source_filename.to_string(), self.span.clone()))
	}

	pub fn label_before_start(&self)
		-> Label<(String, std::ops::Range<usize>)>
	{
		let location = Location {
			span: self.span.start..self.span.start,
			..self.clone()
		};
		location.label()
	}

	pub fn label_after_end(&self) -> Label<(String, std::ops::Range<usize>)>
	{
		let location = Location {
			span: self.span.end..self.span.end,
			..self.clone()
		};
		location.label()
	}

	pub fn combined_with(self, other: &Location) -> Location
	{
		Location {
			span: self.span.start..other.span.end,
			..self
		}
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
	let mut offset = 0;
	for (i, line) in source.lines().enumerate()
	{
		// Syntax should remain such that each line can be lexed independently.
		lex_line(line, source_filename, offset, 1 + i, &mut tokens);
		offset += line.chars().count() + 1;
	}
	tokens
}

fn lex_line(
	line: &str,
	source_filename: &str,
	source_offset_of_line: usize,
	line_number: usize,
	tokens: &mut Vec<LexedToken>,
)
{
	let mut iter = line.chars().enumerate().peekable();
	let mut source_offset_start = source_offset_of_line;
	while let Some((line_offset, x)) = iter.next()
	{
		let mut source_offset_end = source_offset_start + 1;
		let location = Location {
			source_filename: source_filename.to_string(),
			span: source_offset_start..source_offset_end,
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
					source_offset_end += 1;
					Ok(Token::ShiftLeft)
				}
				Some((_, '=')) =>
				{
					iter.next();
					source_offset_end += 1;
					Ok(Token::IsLE)
				}
				_ => Ok(Token::AngleLeft),
			},
			'>' => match iter.peek()
			{
				Some((_, '>')) =>
				{
					iter.next();
					source_offset_end += 1;
					Ok(Token::ShiftRight)
				}
				Some((_, '=')) =>
				{
					iter.next();
					source_offset_end += 1;
					Ok(Token::IsGE)
				}
				_ => Ok(Token::AngleRight),
			},
			'|' => match iter.peek()
			{
				Some((_, ':')) =>
				{
					iter.next();
					source_offset_end += 1;
					Ok(Token::PipeForType)
				}
				_ => Ok(Token::Pipe),
			},
			'&' => Ok(Token::Ampersand),

			'^' => Ok(Token::Caret),
			'!' => match iter.peek()
			{
				Some((_, '=')) =>
				{
					iter.next();
					source_offset_end += 1;
					Ok(Token::DoesNotEqual)
				}
				_ => Ok(Token::Exclamation),
			},
			'+' => Ok(Token::Plus),
			'*' => Ok(Token::Times),
			'%' => Ok(Token::Modulo),
			':' => Ok(Token::Colon),
			';' => Ok(Token::Semicolon),
			'.' => Ok(Token::Dot),
			',' => Ok(Token::Comma),
			'=' => match iter.peek()
			{
				Some((_, '=')) =>
				{
					iter.next();
					source_offset_end += 1;
					Ok(Token::Equals)
				}
				_ => Ok(Token::Assignment),
			},
			'-' => match iter.peek()
			{
				Some((_, '>')) =>
				{
					iter.next();
					source_offset_end += 1;
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
						source_offset_end += 1;
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
					"as" => Token::As,
					"true" => Token::Bool(true),
					"false" => Token::Bool(false),
					"void" => Token::Type(ValueType::Void),
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
					"import" => Token::Import,
					"pub" => Token::Pub,
					"extern" => Token::Extern,
					"struct" => Token::Struct,
					"word8" => Token::Word8,
					"word16" => Token::Word16,
					"word32" => Token::Word32,
					"word64" => Token::Word64,
					"word128" => Token::Word128,
					_ => Token::Identifier(identifier),
				};
				Ok(token)
			}
			'0' => match iter.peek()
			{
				Some((_i, 'x')) =>
				{
					iter.next();
					source_offset_end += 1;
					let mut literal = x.to_string();
					while let Some(&(_, y)) = iter.peek()
					{
						if y.is_ascii_hexdigit()
						{
							literal.push(y);
							iter.next();
							source_offset_end += 1;
						}
						else
						{
							break;
						}
					}
					u64::from_str_radix(&literal, 16)
						.map(Token::BitInteger)
						.map_err(|_error| Error::InvalidBitIntegerLiteral)
				}
				Some((_i, 'b')) =>
				{
					iter.next();
					source_offset_end += 1;
					let mut literal = x.to_string();
					while let Some(&(_, y)) = iter.peek()
					{
						if y.is_digit(2)
						{
							literal.push(y);
							iter.next();
							source_offset_end += 1;
						}
						else
						{
							break;
						}
					}
					u64::from_str_radix(&literal, 2)
						.map(Token::BitInteger)
						.map_err(|_error| Error::InvalidBitIntegerLiteral)
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
							source_offset_end += 1;
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
						_ => Err(Error::InvalidIntegerTypeSuffix),
					}
				}
			},
			'1'..='9' =>
			{
				let mut literal = x.to_string();
				while let Some(&(_, y)) = iter.peek()
				{
					if y.is_ascii_digit()
					{
						literal.push(y);
						iter.next();
						source_offset_end += 1;
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
						source_offset_end += 1;
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
						Err(_error) => Err(Error::InvalidNakedIntegerLiteral),
					}
				}
				else
				{
					match suffix.as_str()
					{
						"i8" => literal
							.parse()
							.map(Token::Int8)
							.map_err(Error::InvalidIntegerLiteral),
						"i16" => literal
							.parse()
							.map(Token::Int16)
							.map_err(Error::InvalidIntegerLiteral),
						"i32" => literal
							.parse()
							.map(Token::Int32)
							.map_err(Error::InvalidIntegerLiteral),
						"i64" => literal
							.parse()
							.map(Token::Int64)
							.map_err(Error::InvalidIntegerLiteral),
						"i128" => literal
							.parse()
							.map(Token::Int128)
							.map_err(Error::InvalidIntegerLiteral),
						"u8" => literal
							.parse()
							.map(Token::Uint8)
							.map_err(Error::InvalidIntegerLiteral),
						"u16" => literal
							.parse()
							.map(Token::Uint16)
							.map_err(Error::InvalidIntegerLiteral),
						"u32" => literal
							.parse()
							.map(Token::Uint32)
							.map_err(Error::InvalidIntegerLiteral),
						"u64" => literal
							.parse()
							.map(Token::Uint64)
							.map_err(Error::InvalidIntegerLiteral),
						"u128" => literal
							.parse()
							.map(Token::Uint128)
							.map_err(Error::InvalidIntegerLiteral),
						"usize" => literal
							.parse()
							.map(Token::Usize)
							.map_err(Error::InvalidIntegerLiteral),
						_ => Err(Error::InvalidIntegerTypeSuffix),
					}
				}
			}
			'"' =>
			{
				let mut bytes = Vec::new();
				let mut closed = false;
				let mut first_error_token = None;
				let mut end_of_line_offset = line_offset + 1;

				while let Some((inner_line_offset, x)) = iter.next()
				{
					let source_offset_start_of_char = source_offset_end;
					source_offset_end += 1;
					end_of_line_offset = inner_line_offset + 1;
					if x == '\\'
					{
						source_offset_end += 1;
						match iter.next()
						{
							Some((_, 'n')) => bytes.push(b'\n'),
							Some((_, 'r')) => bytes.push(b'\r'),
							Some((_, 't')) => bytes.push(b'\t'),
							Some((_, '\\')) => bytes.push(b'\\'),
							Some((_, '\'')) => bytes.push(b'\''),
							Some((_, '\"')) => bytes.push(b'\"'),
							Some((_, '0')) => bytes.push(b'\0'),
							Some((_, 'x')) =>
							{
								let mut byte = None;
								let mut digits = String::new();
								while let Some(&(_, y)) = iter.peek()
								{
									if y.is_ascii_hexdigit()
									{
										digits.push(y);
										iter.next();
										source_offset_end += 1;
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
									break;
								}
								if let Some(byte) = byte
								{
									bytes.push(byte);
								}
								else
								{
									let error = Error::InvalidEscapeSequence;
									let warning = LexedToken {
										result: Err(error),
										location: Location {
											span: source_offset_start_of_char
												..source_offset_end,
											line_offset: end_of_line_offset,
											..location.clone()
										},
									};
									first_error_token.get_or_insert(warning);
								}
							}
							Some((_, 'u')) =>
							{
								let literal = if let Some((_, '{')) =
									iter.peek()
								{
									iter.next();
									source_offset_end += 1;

									let mut literal = String::new();
									while let Some(&(_, y)) = iter.peek()
									{
										if y.is_ascii_hexdigit()
										{
											literal.push(y);
											iter.next();
											source_offset_end += 1;
										}
										else
										{
											break;
										}
									}
									literal
								}
								else
								{
									String::new()
								};
								let c = u32::from_str_radix(&literal, 16)
									.ok()
									.and_then(|x| char::from_u32(x));
								if let Some(c) = c
								{
									let mut buffer = [0; 4];
									let slice = c.encode_utf8(&mut buffer);
									bytes.extend_from_slice(slice.as_bytes());
								}
								else
								{
									let error = Error::InvalidEscapeSequence;
									let warning = LexedToken {
										result: Err(error),
										location: Location {
											span: source_offset_start_of_char
												..source_offset_end,
											line_offset: end_of_line_offset,
											..location.clone()
										},
									};
									first_error_token.get_or_insert(warning);
								}
							}
							Some((_, _y)) =>
							{
								let warning = LexedToken {
									result: Err(Error::InvalidEscapeSequence),
									location: Location {
										span: source_offset_start_of_char
											..source_offset_end,
										line_offset: end_of_line_offset,
										..location.clone()
									},
								};
								first_error_token.get_or_insert(warning);
							}
							None =>
							{
								let warning = LexedToken {
									result: Err(
										Error::UnexpectedTrailingBackslash,
									),
									location: Location {
										span: source_offset_start_of_char
											..source_offset_end,
										line_offset: end_of_line_offset,
										..location.clone()
									},
								};
								first_error_token.get_or_insert(warning);
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
							result: Err(Error::UnexpectedCharacter),
							location: Location {
								span: source_offset_start_of_char
									..source_offset_end,
								line_offset: end_of_line_offset,
								..location.clone()
							},
						};
						first_error_token.get_or_insert(warning);
					}
					else
					{
						for byte in x.to_string().as_bytes()
						{
							bytes.push(*byte);
						}
					}
				}
				if !closed
				{
					let warning = LexedToken {
						result: Err(Error::MissingClosingQuote),
						location: Location {
							span: source_offset_start..source_offset_end,
							line_offset: end_of_line_offset,
							..location.clone()
						},
					};
					first_error_token.get_or_insert(warning);
				}
				if let Some(error_token) = first_error_token
				{
					tokens.push(error_token);
					source_offset_start = source_offset_end;
					continue;
				}
				Ok(Token::StringLiteral { bytes })
			}
			'$' => Ok(Token::DebugDollar),
			' ' | '\t' =>
			{
				source_offset_start = source_offset_end;
				continue;
			}
			_ => Err(Error::UnexpectedCharacter),
		};
		let location = Location {
			span: source_offset_start..source_offset_end,
			..location
		};
		tokens.push(LexedToken { result, location });
		source_offset_start = source_offset_end;
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
