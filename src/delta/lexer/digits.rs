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
			.all(|x| x.is_digit(2))
	);
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
			.all(|x| x.is_digit(10))
	);
	let validated_digits =
		unsafe { std::str::from_utf8_unchecked(validated_digits) };
	u128::from_str_radix(validated_digits, 10)
		.map_err(|_| LexingError::InvalidIntegerLength)
}

#[inline(never)]
pub fn parse_hex_digits(validated_digits: &[u8]) -> Result<u128, LexingError>
{
	debug_assert!(
		validated_digits
			.iter()
			.copied()
			.map(char::from)
			.all(|x| x.is_digit(16))
	);
	let validated_digits =
		unsafe { std::str::from_utf8_unchecked(validated_digits) };
	u128::from_str_radix(validated_digits, 16)
		.map_err(|_| LexingError::InvalidIntegerLength)
}

#[inline(always)]
pub fn parse_double_hex(validated_digits: [u8; 2]) -> u8
{
	debug_assert!(
		validated_digits
			.iter()
			.copied()
			.map(char::from)
			.all(|x| x.is_digit(16))
	);
	let validated_digits =
		unsafe { std::str::from_utf8_unchecked(&validated_digits) };
	let result = u8::from_str_radix(validated_digits, 16);
	debug_assert!(result.is_ok(), "already checked");
	result.unwrap_or(0)
}
