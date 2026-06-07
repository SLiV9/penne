#[inline(always)]
pub fn parse_decimal_digit(a: u8) -> Option<u8>
{
	match a
	{
		b'0'..=b'9' => Some(a & 0b0000_1111),
		_ => None,
	}
}

#[inline(always)]
pub fn parse_hex_digit(a: u8) -> Option<u8>
{
	match a
	{
		b'A'..=b'F' => Some((a & 0b0000_1111) + 9),
		b'a'..=b'f' => Some((a & 0b0000_1111) + 9),
		b'0'..=b'9' => Some(a & 0b0000_1111),
		_ => None,
	}
}
