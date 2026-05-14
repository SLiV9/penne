use crate::delta::lexer::tokens::MAX_SOURCE_LEN;

#[derive(Clone, Copy, Debug, PartialEq)]
#[derive(strum::FromRepr, strum::IntoStaticStr, strum::EnumIter)]
#[repr(u8)]
#[strum(serialize_all = "lowercase")]
pub enum Marker
{
	Unrecognized = 0,

	Comment = 0x0F,
	Newline = b'\n',
	Whitespace = b' ',

	Exclamation = b'!',
	DoubleQuote = b'"',
	Modulo = b'%',
	Ampersand = b'&',
	SingleQuote = b'\'',
	ParenLeft = b'(',
	ParenRight = b')',
	Times = b'*',
	Plus = b'+',
	Comma = b',',
	Minus = b'-',
	Dot = b'.',
	Divide = b'/',
	NumericLiteralStartWithZero = b'0',
	NumericLiteralPositive = b'1',
	Dots = b'6',         // ..   ('.' + 8)
	DoesNotEqual = b'9', // !=   ('!' + 24)
	Colon = b':',
	Semicolon = b';',
	AngleLeft = b'<',
	Assignment = b'=',
	AngleRight = b'>',
	IdentifierStart = b'A',
	Arrow = b'E',      // ->   ('-' + 24)
	ShiftLeft = b'L',  // <<   ('<' = 16)
	ShiftRight = b'N', // >>   ('<' = 16)
	IsGE = b'T',       // >=   ('>' + 24)
	Equals = b'U',     // ==   ('=' + 24)
	IsLE = b'V',       // <=   ('<' + 24)
	BracketLeft = b'[',
	Caret = b'^',
	BracketRight = b']',
	Placeholder = b'_',
	IdentifierContinuation = b'a',
	PipeForType = b'd', // |:   ('|' - 24)
	BraceLeft = b'{',
	Pipe = b'|',
	BraceRight = b'}',

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

	// Keyword literals.
	True,
	False,

	// Scanner errors.
	#[strum(serialize = "ERR")]
	UnexpectedZeroByteFile,
	#[strum(serialize = "ERR")]
	TooManySourceBytes,
}

pub fn scan(source: &[u8]) -> Vec<Marker>
{
	if source.is_empty()
	{
		return vec![Marker::UnexpectedZeroByteFile];
	}
	if source.len() > MAX_SOURCE_LEN
	{
		return vec![Marker::TooManySourceBytes];
	}

	let mut buffer = vec![Marker::Unrecognized; source.len()];
	scan_markers(source, &mut buffer);
	buffer
}

#[inline(never)]
fn scan_markers(source: &[u8], markers: &mut [Marker])
{
	debug_assert_eq!(source.len(), markers.len());
	if source.len() != markers.len()
	{
		return;
	}

	let slow_context = |prev: u8, i: usize| -> [u8; 16] {
		std::array::from_fn(|j| {
			if j == 0
			{
				prev
			}
			else
			{
				source.get(i + j - 1).copied().unwrap_or(0x03)
			}
		})
	};

	for i in 0..source.len()
	{
		let context: [u8; 16] = if i == 0
		{
			slow_context(0x02, i)
		}
		else if i + 15 <= source.len()
		{
			source[(i - 1)..(i + 15)]
				.as_array()
				.copied()
				.expect("16 byte slice")
		}
		else
		{
			slow_context(source[i - 1], i)
		};
		markers[i] = scan_marker(context);
	}
}

#[inline(always)]
fn scan_marker(context: [u8; 16]) -> Marker
{
	let prev = context[0];
	let curr = context[1];
	let next = context[2];
	match curr
	{
		b' ' => Marker::Whitespace,
		b'\t' => Marker::Whitespace,
		b'\r' => Marker::Whitespace,
		b'\n' => Marker::Newline,
		b'(' => Marker::ParenLeft,
		b')' => Marker::ParenRight,
		b'{' => Marker::BraceLeft,
		b'}' => Marker::BraceRight,
		b'[' => Marker::BracketLeft,
		b']' => Marker::BracketRight,
		b'&' => Marker::Ampersand,
		b'^' => Marker::Caret,
		b'+' => Marker::Plus,
		b'*' => Marker::Times,
		b'%' => Marker::Modulo,
		b':' => Marker::Colon,
		b';' => Marker::Semicolon,
		b',' => Marker::Comma,
		b'<' => match next
		{
			b'<' => Marker::ShiftLeft,
			b'=' => Marker::IsLE,
			_ => Marker::AngleLeft,
		},
		b'>' => match next
		{
			b'>' => Marker::ShiftRight,
			b'=' => Marker::IsGE,
			_ => Marker::AngleRight,
		},
		b'|' => match next
		{
			b':' => Marker::PipeForType,
			_ => Marker::Pipe,
		},
		b'!' => match next
		{
			b'=' => Marker::DoesNotEqual,
			_ => Marker::Exclamation,
		},
		b'.' => match next
		{
			b'.' => Marker::Dots,
			_ => Marker::Dot,
		},
		b'=' => match next
		{
			b'=' => Marker::Equals,
			_ => Marker::Assignment,
		},
		b'-' => match next
		{
			b'>' => Marker::Arrow,
			_ => Marker::Minus,
		},
		b'/' => match next
		{
			b'/' => Marker::Comment,
			_ => Marker::Divide,
		},
		b'a'..=b'z' if is_identifier_continuation(prev) =>
		{
			Marker::IdentifierContinuation
		}
		b'a'..=b'z' =>
		{
			// scan_keyword_marker(context).unwrap_or(Marker::IdentifierStart)
			Marker::IdentifierStart
		}
		b'A'..=b'Z' if is_identifier_continuation(prev) =>
		{
			Marker::IdentifierContinuation
		}
		b'A'..=b'Z' => Marker::IdentifierStart,
		b'0'..=b'9' if is_identifier_continuation(prev) =>
		{
			Marker::IdentifierContinuation
		}
		b'0' => Marker::NumericLiteralStartWithZero,
		b'1'..=b'9' => Marker::NumericLiteralPositive,
		b'_' if is_identifier_continuation(prev) =>
		{
			Marker::IdentifierContinuation
		}
		b'_' if is_identifier_continuation(next) => Marker::IdentifierStart,
		b'_' => Marker::Placeholder,
		b'\'' | b'"' if prev == b'\\' => Marker::Unrecognized,
		b'\'' => Marker::SingleQuote,
		b'"' => Marker::DoubleQuote,
		_ => Marker::Unrecognized,
	}
}

#[inline(always)]
fn scan_keyword_marker(context: [u8; 16]) -> Option<Marker>
{
	use Marker::*;

	let is_keyword = |marker: Marker| -> bool {
		let keyword: &'static str = marker.into();
		let keyword: &[u8] = keyword.as_bytes();
		for i in 0..keyword.len()
		{
			if context[1 + i] != keyword[i]
			{
				return false;
			}
		}
		!is_identifier_continuation(context[1 + keyword.len()])
	};

	for marker in [
		Fn, Var, Const, If, Goto, Loop, Else, Cast, As, Import, Pub, Extern,
		Struct, Word8, Word16, Word32, Word64, Word128, Void, Int8, Int16,
		Int32, Int64, Int128, Uint8, Uint16, Uint32, Uint64, Uint128, Usize,
		Char8, Bool, True, False,
	]
	{
		if is_keyword(marker)
		{
			return Some(marker);
		}
	}
	None
}

#[inline(always)]
pub(crate) fn is_identifier_continuation(x: u8) -> bool
{
	matches!(x, b'a'..=b'z' | b'A'..=b'Z' | b'0'..=b'9' | b'_')
}
