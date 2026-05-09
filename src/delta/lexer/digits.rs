use super::LexingError;

pub fn parse_binary_digits(validated_digits: &str)
	-> Result<u128, LexingError>
{
	debug_assert!(validated_digits.chars().all(|x| x.is_digit(2)));
	u128::from_str_radix(validated_digits, 2)
		.map_err(|_| LexingError::InvalidIntegerLength)
}

pub fn parse_decimal_digits(validated_digits: &str)
	-> Result<u128, LexingError>
{
	debug_assert!(validated_digits.chars().all(|x| x.is_digit(10)));
	u128::from_str_radix(validated_digits, 10)
		.map_err(|_| LexingError::InvalidIntegerLength)
}

pub fn parse_hex_digits(validated_digits: &str) -> Result<u128, LexingError>
{
	debug_assert!(validated_digits.chars().all(|x| x.is_digit(16)));
	u128::from_str_radix(validated_digits, 16)
		.map_err(|_| LexingError::InvalidIntegerLength)
}
