use rand::distr::weighted::WeightedIndex;
use rand::prelude::*;
use strum::IntoEnumIterator as _;

use super::lexer::is_identifier_continuation;
use super::lexer::BaseToken;
use super::lexer::ValueTypeKeyword;

#[derive(Debug, Clone, Copy, Default)]
#[derive(clap::ValueEnum, serde::Deserialize, serde::Serialize, strum::Display)]
#[serde(rename_all = "snake_case")]
#[strum(serialize_all = "snake_case")]
pub enum FuzzingStrategy
{
	#[default]
	Tokens,
}

/// Fill up a buffer to a given percentage of its capacity
/// using weighted-random tokens.
///
/// Passing in a buffer with small capacity may result in an infinite loop.
pub fn fill_to_capacity_with_tokens(
	percentage: usize,
	buffer: &mut String,
	num_errors: usize,
) -> Result<(), anyhow::Error>
{
	let mut rng = rand::rng();

	let mut base_token_weights = [0; 256];
	for base_token in BaseToken::iter()
	{
		use BaseToken::*;
		let i = base_token as u8 as usize;
		let weight = match base_token
		{
			EndOfSource => 0,

			ParenLeft | ParenRight | BraceLeft | BraceRight | BracketLeft
			| BracketRight | AngleLeft | AngleRight => 10,

			Pipe | Ampersand | Caret | Exclamation | Placeholder | Plus
			| Minus | Times | Divide | Modulo | Colon | Semicolon | Dot
			| Comma | Assignment | Equals | DoesNotEqual | IsGE | IsLE
			| ShiftLeft | ShiftRight | Arrow | PipeForType | Dots => 5,

			Fn | Var | Const | If | Goto | Loop | Else | Cast | As | Import
			| Pub | Extern | Struct => 5,

			Word8 | Word16 | Word32 | Word64 | Word128 => 1,

			ValueTypeKeyword => 10,

			Identifier => 2,
			Builtin => 1,

			NakedDecimal => 2,
			BitInteger => 1,
			SuffixedInteger => 1,
			CharLiteral => 1,
			BoolLiteral => 1,

			StringLiteral => 1,

			Error => 0,
		};
		base_token_weights[i] = weight;
	}
	let base_token_dist = WeightedIndex::new(base_token_weights)?;

	let mut value_type_weights = [0; 256];
	for value_type in ValueTypeKeyword::iter()
	{
		use ValueTypeKeyword::*;
		let i = value_type as u8 as usize;
		let weight = match value_type
		{
			ValueTypeKeyword::NoKeyword => 0,
			_ => 1,
		};
		value_type_weights[i] = weight;
	}
	let value_type_dist = WeightedIndex::new(value_type_weights)?;

	let mut us_ascii_weights: [i32; 128] = std::array::from_fn(|i| {
		let x = i as u8;
		if x.is_ascii_graphic()
		{
			10
		}
		else
		{
			1
		}
	});
	let us_ascii_dist = WeightedIndex::new(us_ascii_weights)?;

	let random_uint = |rng: &mut dyn Rng| -> u128 {
		if rng.random_bool(0.2)
		{
			0
		}
		else if rng.random_bool(0.05)
		{
			rng.random_range(1..=u128::MAX)
		}
		else if rng.random_bool(0.25)
		{
			let digits: u32 = rng.random_range(3..20);
			let limit: u128 = 10u128.pow(digits);
			rng.random_range(1..limit)
		}
		else
		{
			rng.random_range(1..1000)
		}
	};

	let random_char = |rng: &mut dyn Rng| -> char {
		if rng.random_bool(0.05)
		{
			let unicode_char: char = rng.random();
			unicode_char
		}
		else
		{
			let ascii = us_ascii_dist.sample(rng) as u8;
			char::from(ascii)
		}
	};

	let random_identifier = |rng: &mut dyn Rng| -> String {
		let mut bytes = Vec::new();
		for i in 0..rng.random_range(1..20)
		{
			if i > 0 && rng.random_bool(0.1)
			{
				bytes.push(rng.random_range(b'0'..=b'9'));
			}
			else if rng.random_bool(0.7)
			{
				bytes.push(rng.random_range(b'a'..=b'z'));
			}
			if rng.random_bool(0.9)
			{
				bytes.push(rng.random_range(b'A'..=b'Z'));
			}
			else
			{
				bytes.push(b'_');
			}
		}
		std::str::from_utf8(&bytes).unwrap().to_string()
	};

	let add_space_if_necessary = |buffer: &mut String| {
		if let Some(last_byte) = buffer.bytes().last()
		{
			if is_identifier_continuation(last_byte)
			{
				buffer.push(' ');
			}
		}
	};

	let mut next_newline_at = rng.random_range(10..160);
	let mut next_comment_at = rng.random_range(200..500);

	assert!(percentage < 100);
	while 100 * buffer.len() < percentage * buffer.capacity()
	{
		while buffer.len() > next_newline_at
		{
			if rng.random_bool(0.05)
			{
				buffer.push('\r');
			}
			buffer.push('\n');

			if rng.random_bool(0.9)
			{
				next_newline_at = buffer.len() + rng.random_range(10..160);
			}
		}

		while buffer.len() > next_comment_at
		{
			if rng.random_bool(0.8)
			{
				buffer.push('\n');
			}
			buffer.push_str("//");
			for _ in 0..rng.random_range(0..160)
			{
				let c: char = random_char(&mut rng);
				if c != '\n'
				{
					buffer.push(c);
				}
			}
			buffer.push('\n');

			if rng.random_bool(0.9)
			{
				next_comment_at = buffer.len() + rng.random_range(200..500);
			}
		}

		let base_token = base_token_dist.sample(&mut rng) as u8;
		let base_token = BaseToken::from_repr(base_token).unwrap();
		match base_token
		{
			BaseToken::Identifier =>
			{
				add_space_if_necessary(buffer);
				let identifier = random_identifier(&mut rng);
				buffer.push_str(&identifier);
			}
			BaseToken::Builtin =>
			{
				add_space_if_necessary(buffer);
				let identifier = random_identifier(&mut rng);
				buffer.push_str(&identifier);
				buffer.push('!');
			}
			BaseToken::ValueTypeKeyword =>
			{
				add_space_if_necessary(buffer);
				let value_type = value_type_dist.sample(&mut rng) as u8;
				let value_type =
					ValueTypeKeyword::from_repr(value_type).unwrap();
				buffer.push_str(&value_type.to_string());
			}
			BaseToken::NakedDecimal =>
			{
				add_space_if_necessary(buffer);
				let value = random_uint(&mut rng);
				buffer.push_str(&value.to_string());
			}
			BaseToken::BitInteger =>
			{
				add_space_if_necessary(buffer);
				let value = random_uint(&mut rng);
				let text = if rng.random_bool(0.5)
				{
					if rng.random_bool(0.5)
					{
						format!("0x{value:X}")
					}
					else
					{
						format!("0x{value:x}")
					}
				}
				else
				{
					format!("0b{value:b}")
				};
				buffer.push_str(&text);
			}
			BaseToken::SuffixedInteger =>
			{
				add_space_if_necessary(buffer);
				let value = random_uint(&mut rng);
				let text = if rng.random_bool(0.75)
				{
					value.to_string()
				}
				else if rng.random_bool(0.5)
				{
					if rng.random_bool(0.5)
					{
						format!("0x{value:X}")
					}
					else
					{
						format!("0x{value:x}")
					}
				}
				else
				{
					format!("0b{value:b}")
				};
				buffer.push_str(&text);
				let value_type = value_type_dist.sample(&mut rng) as u8;
				let value_type =
					ValueTypeKeyword::from_repr(value_type).unwrap();
				buffer.push_str(&value_type.to_string());
			}
			BaseToken::CharLiteral =>
			{
				buffer.push('\'');
				if rng.random_bool(0.01)
				{
					let value: u32 = rng.random_range(..256);
					buffer.push_str(&format!("\\x{value:X}"));
				}
				else
				{
					let c: char = random_char(&mut rng);
					for x in c.escape_default()
					{
						buffer.push(x);
					}
				}
				buffer.push('\'');
			}
			BaseToken::BoolLiteral =>
			{
				add_space_if_necessary(buffer);
				if rng.random_bool(0.5)
				{
					buffer.push_str("true");
				}
				else
				{
					buffer.push_str("false");
				}
			}
			BaseToken::StringLiteral =>
			{
				buffer.push('"');
				for _ in 0..rng.random_range(0..100)
				{
					if rng.random_bool(0.01)
					{
						let value: u32 = rng.random_range(..256);
						buffer.push_str(&format!("\\x{value:X}"));
					}
					else
					{
						let c: char = random_char(&mut rng);
						for x in c.escape_default()
						{
							buffer.push(x);
						}
					}
				}
				buffer.push('"');
			}
			BaseToken::BraceLeft => buffer.push('{'),
			BaseToken::BraceRight => buffer.push('}'),
			_ =>
			{
				let token = base_token.to_string();
				if is_identifier_continuation(token.bytes().next().unwrap())
				{
					add_space_if_necessary(buffer);
				}
				buffer.push_str(&token);
			}
		}
	}

	for _ in 0..num_errors
	{
		let mut i = rng.random_range(..buffer.len());
		while !buffer.is_char_boundary(i)
		{
			i -= 1;
		}
		let mut j = i + 1;
		while !buffer.is_char_boundary(j)
		{
			j += 1;
		}
		let nonsense: String = random_char(&mut rng).to_string();
		buffer.replace_range(i..j, &nonsense);
	}

	Ok(())
}
