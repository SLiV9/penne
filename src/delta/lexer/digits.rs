use super::LexingError;

pub fn parse_binary_digits(validated_digits: &[u8])
	-> Result<u128, LexingError>
{
	debug_assert!(validated_digits
		.iter()
		.copied()
		.map(char::from)
		.all(|x| x.is_digit(2)));
	let validated_digits =
		unsafe { std::str::from_utf8_unchecked(validated_digits) };
	u128::from_str_radix(validated_digits, 2)
		.map_err(|_| LexingError::InvalidIntegerLength)
}

pub fn parse_decimal_digits(
	validated_digits: &[u8],
) -> Result<u128, LexingError>
{
	debug_assert!(validated_digits
		.iter()
		.copied()
		.map(char::from)
		.all(|x| x.is_digit(10)));
	let validated_digits =
		unsafe { std::str::from_utf8_unchecked(validated_digits) };
	u128::from_str_radix(validated_digits, 10)
		.map_err(|_| LexingError::InvalidIntegerLength)
}

pub fn parse_hex_digits(validated_digits: &[u8]) -> Result<u128, LexingError>
{
	debug_assert!(validated_digits
		.iter()
		.copied()
		.map(char::from)
		.all(|x| x.is_digit(16)));
	let validated_digits =
		unsafe { std::str::from_utf8_unchecked(validated_digits) };
	u128::from_str_radix(validated_digits, 16)
		.map_err(|_| LexingError::InvalidIntegerLength)
}
