/**/

use thiserror::Error;

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
	Assignment, // =

	// Double-character tokens.
	Equals, // ==

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
}

#[derive(Debug, Error)]
pub enum Error
{
	#[error("unexpected character {character:?}")]
	UnexpectedCharacter
	{
		character: char
	},
	#[error("invalid integer")]
	InvalidInteger(#[from] std::num::ParseIntError),
}

impl Error
{
	fn at(self, line: usize, line_offset: usize) -> AnnotatedError
	{
		AnnotatedError {
			error: self,
			line,
			line_offset,
		}
	}
}

#[derive(Debug)]
pub struct AnnotatedError
{
	error: Error,
	line: usize,
	line_offset: usize,
}

impl std::error::Error for AnnotatedError {}
impl std::fmt::Display for AnnotatedError
{
	fn fmt(
		&self,
		f: &mut std::fmt::Formatter,
	) -> std::result::Result<(), std::fmt::Error>
	{
		write!(f, "at {}:{}: {}", self.line, self.line_offset, self.error)
	}
}

pub fn lex(source: &str) -> Result<Vec<Token>, AnnotatedError>
{
	let mut tokens = Vec::new();
	let mut line = 0;
	let mut last_line_start_offset = 0;
	let mut iter = source.chars().enumerate().peekable();
	while let Some((offset_of_x, x)) = iter.next()
	{
		let line_offset = offset_of_x - last_line_start_offset;
		let token = match x
		{
			'(' => Token::ParenLeft,
			')' => Token::ParenRight,
			'{' => Token::BraceLeft,
			'}' => Token::BraceRight,
			'+' => Token::Plus,
			'-' => Token::Minus,
			'*' => Token::Times,
			':' => Token::Colon,
			';' => Token::Semicolon,
			'=' => match iter.peek()
			{
				Some((_, '=')) =>
				{
					iter.next();
					Token::Equals
				}
				_ => Token::Assignment,
			},
			'/' => match iter.peek()
			{
				Some((_, '/')) =>
				{
					while let Some((offset_of_y, y)) = iter.next()
					{
						if y == '\n'
						{
							line += 1;
							last_line_start_offset = offset_of_y + 1;
							break;
						}
					}
					continue;
				}
				_ => Token::Divide,
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
				match identifier.as_str()
				{
					"fn" => Token::Fn,
					"var" => Token::Var,
					"if" => Token::If,
					"goto" => Token::Goto,
					"loop" => Token::Loop,
					"else" => Token::Else,
					"true" => Token::Bool(true),
					"false" => Token::Bool(false),
					_ => Token::Identifier(identifier),
				}
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
				let value: i32 = match literal.parse()
				{
					Ok(value) => value,
					Err(error) =>
					{
						return Err(AnnotatedError {
							error: error.into(),
							line,
							line_offset,
						})
					}
				};
				Token::Int32(value)
			}
			' ' | '\t' | '\r' => continue,
			'\n' =>
			{
				line += 1;
				last_line_start_offset = offset_of_x + 1;
				continue;
			}
			_ =>
			{
				return Err(Error::UnexpectedCharacter { character: x }
					.at(line, line_offset))
			}
		};
		tokens.push(token);
	}
	Ok(tokens)
}

fn is_identifier_continuation(x: char) -> bool
{
	match x
	{
		'a'..='z' | 'A'..='Z' | '0'..='9' | '_' => true,
		_ => false,
	}
}
