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

#[inline(never)]
pub fn parse_hex_digits(validated_digits: &[u8]) -> Result<u128, LexingError>
{
	debug_assert!(
		validated_digits
			.iter()
			.copied()
			.map(char::from)
			.all(|x| x.is_digit(16) || x == '_')
	);
	debug_assert!(
		validated_digits
			.iter()
			.copied()
			.map(char::from)
			.any(|x| x.is_digit(16))
	);
	// TODO faster and with _ support
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

	let parse = |a: u8| {
		let is_letter_bit = (a & 0b0100_0000) >> 6;
		let nine_if_is_letter = 9 * is_letter_bit;
		let lo = a & 0b0000_1111;
		debug_assert_eq!(
			lo,
			match a
			{
				b'A'..=b'F' => a - b'A' + 1,
				b'a'..=b'f' => a - b'a' + 1,
				_ => a - b'0',
			}
		);
		lo + nine_if_is_letter
	};

	let [a, b] = validated_digits;
	let x = parse(a);
	let y = parse(b);
	x * 16 + y
}
