/**/

use anyhow::anyhow;

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

pub fn lex(source: &str) -> Result<Vec<Token>, anyhow::Error>
{
	let mut tokens = Vec::new();
	let mut iter = source.chars().peekable();
	while let Some(x) = iter.next()
	{
		let token = match x
		{
			'(' => Token::ParenLeft,
			')' => Token::ParenRight,
			'{' => Token::BraceLeft,
			'}' => Token::BraceRight,
			'+' => Token::Plus,
			'-' => Token::Minus,
			':' => Token::Colon,
			';' => Token::Semicolon,
			'=' => match iter.peek()
			{
				Some('=') =>
				{
					iter.next();
					Token::Equals
				}
				_ => Token::Assignment,
			},
			'/' => match iter.peek()
			{
				Some('/') =>
				{
					while let Some(y) = iter.next()
					{
						if y == '\n'
						{
							break;
						}
					}
					continue;
				}
				Some(y) =>
				{
					return Err(anyhow!("unexpected sequence '{}' '{}'", x, y));
				}
				None =>
				{
					return Err(anyhow!("unexpected final character '{}'", x));
				}
			},
			'a'..='z' | 'A'..='Z' | '_' =>
			{
				let mut identifier = x.to_string();
				while let Some(&y) = iter.peek()
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
				while let Some(&y) = iter.peek()
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
				let value = literal.parse()?;
				Token::Int32(value)
			}
			' ' | '\t' | '\r' | '\n' => continue,
			_ => return Err(anyhow!("unexpected character '{}'", x)),
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
