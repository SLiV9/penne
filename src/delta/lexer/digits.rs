use super::LexingError;

#[inline(never)]
pub fn parse_binary_digits(validated_digits: &[u8])
-> Result<u128, LexingError>
{
	debug_assert!(
		validated_digits
			.iter()
			.copied()
			.map(char::from)
			.all(|x| x.is_digit(2) || x == '_')
	);
	debug_assert!(
		validated_digits
			.iter()
			.copied()
			.map(char::from)
			.any(|x| x.is_digit(2))
	);
	// TODO faster and with _ support
	let validated_digits =
		unsafe { std::str::from_utf8_unchecked(validated_digits) };
	u128::from_str_radix(validated_digits, 2)
		.map_err(|_| LexingError::InvalidIntegerLength)
}

#[inline(never)]
pub fn parse_decimal_digits(
	validated_digits: &[u8],
) -> Result<u128, LexingError>
{
	debug_assert!(
		validated_digits
			.iter()
			.copied()
			.map(char::from)
			.all(|x| x.is_digit(10) || x == '_')
	);
	debug_assert!(
		validated_digits
			.iter()
			.copied()
			.map(char::from)
			.any(|x| x.is_digit(10))
	);
	// TODO faster and with _ support
	let validated_digits =
		unsafe { std::str::from_utf8_unchecked(validated_digits) };
	u128::from_str_radix(validated_digits, 10)
		.map_err(|_| LexingError::InvalidIntegerLength)
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
