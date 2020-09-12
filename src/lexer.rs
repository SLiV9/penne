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

	// Keywords.
	Fn,

	// Literals.
	Identifier(String),
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
					_ => Token::Identifier(identifier),
				}
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
