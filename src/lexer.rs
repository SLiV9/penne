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
	Plus,
	Minus,
	Times,
	Divide,
	Colon,
	Semicolon,
	Comma,
	Assignment, // =

	// Double-character tokens.
	Equals, // ==
	Arrow,  // ->

	// Keywords.
	Fn,
	Var,
	If,
	Goto,
	Loop,
	Else,

	// Literals.
	Identifier(String),
	Int32(i32),
	Bool(bool),

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

pub fn lex(source: &str, source_filename: &str) -> Vec<LexedToken>
{
	let mut tokens = Vec::new();
	for (line_number, line) in source.lines().enumerate()
	{
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
			'+' => Ok(Token::Plus),
			'*' => Ok(Token::Times),
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
					"if" => Token::If,
					"goto" => Token::Goto,
					"loop" => Token::Loop,
					"else" => Token::Else,
					"true" => Token::Bool(true),
					"false" => Token::Bool(false),
					"i32" => Token::Type(ValueType::Int32),
					"bool" => Token::Type(ValueType::Bool),
					_ => Token::Identifier(identifier),
				};
				Ok(token)
			}
			'0'..='9' =>
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
				match literal.parse()
				{
					Ok(value) => Ok(Token::Int32(value)),
					Err(error) => Err(error.into()),
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
