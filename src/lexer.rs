//
// Part of penne
// Copyright (c) 2020 Sander in 't Veld
// License: MIT
//

//! The lexer cuts each line of source code into a sequence of tokens.

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

	// Literals.
	Identifier(String),
	Builtin(String),
	NakedDecimal(u128),
	BitInteger(u128),
	SuffixedInteger
	{
		value: u128,
		suffix_type: ValueType,
	},
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
	UnexpectedZeroByteFile,
	UnexpectedCharacter,
	InvalidIntegerLength,
	InvalidIntegerTypeSuffix,
	InvalidEscapeSequence,
	UnexpectedTrailingBackslash,
	MissingClosingQuote,
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
	pub fn combined_with(self, other: &Location) -> Location
	{
		let start = std::cmp::min(self.span.start, other.span.start);
		let end = std::cmp::max(self.span.end, other.span.end);
		Location {
			span: start..end,
			..self
		}
	}

	pub fn comparison_key(&self) -> (&str, usize, usize)
	{
		(&self.source_filename, self.line_number, self.line_offset)
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
	if source.len() == 0
	{
		let placeholder = LexedToken {
			result: Err(Error::UnexpectedZeroByteFile),
			location: Location {
				source_filename: source_filename.to_string(),
				span: 0..0,
				line_number: 1,
				line_offset: 1,
			},
		};
		tokens.push(placeholder);
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
					return;
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
					"cast" => Token::Cast,
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
					"char8" => Token::Type(ValueType::Char8),
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
					"_" => Token::Placeholder,
					_identifier =>
					{
						if let Some(&(_, '!')) = iter.peek()
						{
							iter.next();
							source_offset_end += 1;
							Token::Builtin(identifier)
						}
						else
						{
							Token::Identifier(identifier)
						}
					}
				};
				Ok(token)
			}
			'0' =>
			{
				let mut literal = String::new();
				let mut suffix = String::new();
				let value = match iter.peek()
				{
					Some((_i, 'x')) =>
					{
						iter.next();
						source_offset_end += 1;
						while let Some(&(_, y)) = iter.peek()
						{
							if y.is_ascii_hexdigit()
							{
								literal.push(y);
								iter.next();
								source_offset_end += 1;
							}
							else if y == '_'
							{
								iter.next();
								source_offset_end += 1;
							}
							else
							{
								break;
							}
						}
						match u128::from_str_radix(&literal, 16)
						{
							Ok(value) => Ok(value),
							Err(_) if literal.is_empty() =>
							{
								suffix.push('x');
								Ok(0)
							}
							Err(_) => Err(Error::InvalidIntegerLength),
						}
					}
					Some((_i, 'b')) =>
					{
						iter.next();
						source_offset_end += 1;
						while let Some(&(_, y)) = iter.peek()
						{
							if y.is_digit(2)
							{
								literal.push(y);
								iter.next();
								source_offset_end += 1;
							}
							else if y == '_'
							{
								iter.next();
								source_offset_end += 1;
							}
							else
							{
								break;
							}
						}
						match u128::from_str_radix(&literal, 2)
						{
							Ok(value) => Ok(value),
							Err(_) if literal.is_empty() =>
							{
								suffix.push('b');
								Ok(0)
							}
							Err(_) => Err(Error::InvalidIntegerLength),
						}
					}
					_ => Ok(0),
				};
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
				// We have to consume the entire token (0[a-zA-Z0-9_]*)
				// before we can error out and move on to the next token.
				match value
				{
					Ok(0) if literal.is_empty() && suffix.is_empty() =>
					{
						Ok(Token::NakedDecimal(0))
					}
					Ok(value) if suffix.is_empty() =>
					{
						Ok(Token::BitInteger(value))
					}
					Ok(value) => match parse_integer_suffix(&suffix)
					{
						Ok(suffix_type) =>
						{
							Ok(Token::SuffixedInteger { value, suffix_type })
						}
						Err(err) => Err(err),
					},
					Err(err) => Err(err),
				}
			}
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
					else if y == '_'
					{
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
				// We have to consume the entire token ([1-9][a-zA-Z0-9_]*)
				// before we can error out and move on to the next token.
				match literal.parse()
				{
					Ok(value) if suffix.is_empty() =>
					{
						Ok(Token::NakedDecimal(value))
					}
					Ok(value) => match parse_integer_suffix(&suffix)
					{
						Ok(suffix_type) =>
						{
							Ok(Token::SuffixedInteger { value, suffix_type })
						}
						Err(err) => Err(err),
					},
					Err(_) => Err(Error::InvalidIntegerLength),
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
								let mut digits = String::new();
								while let Some(&(_, y)) = iter.peek()
								{
									if y.is_ascii_hexdigit()
									{
										digits.push(y);
										iter.next();
										source_offset_end += 1;

										if digits.len() == 2
										{
											let byte =
												u8::from_str_radix(&digits, 16)
													.unwrap();
											bytes.push(byte);
											break;
										}
									}
									else
									{
										break;
									}
								}
								if digits.len() < 2
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

									let mut is_closed = false;
									let mut literal = String::new();
									while let Some(&(_, y)) = iter.peek()
									{
										if y.is_ascii_hexdigit()
										{
											literal.push(y);
											iter.next();
											source_offset_end += 1;
										}
										else if y == '}'
										{
											is_closed = true;
											iter.next();
											source_offset_end += 1;
											break;
										}
										else
										{
											break;
										}
									}
									if !is_closed
									{
										literal.clear();
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

fn parse_integer_suffix(suffix: &str) -> Result<ValueType, Error>
{
	match suffix
	{
		"i8" => Ok(ValueType::Int8),
		"i16" => Ok(ValueType::Int16),
		"i32" => Ok(ValueType::Int32),
		"i64" => Ok(ValueType::Int64),
		"i128" => Ok(ValueType::Int128),
		"u8" => Ok(ValueType::Uint8),
		"u16" => Ok(ValueType::Uint16),
		"u32" => Ok(ValueType::Uint32),
		"u64" => Ok(ValueType::Uint64),
		"u128" => Ok(ValueType::Uint128),
		"usize" => Ok(ValueType::Usize),
		_ => Err(Error::InvalidIntegerTypeSuffix),
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
