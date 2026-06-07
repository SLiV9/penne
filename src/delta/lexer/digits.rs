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

pub const HEX_LUT: [u8; 256] = const {
	let mut lut = [0xFF; 256];
	lut[b'0' as usize] = 0;
	lut[b'1' as usize] = 1;
	lut[b'2' as usize] = 2;
	lut[b'3' as usize] = 3;
	lut[b'4' as usize] = 4;
	lut[b'5' as usize] = 5;
	lut[b'6' as usize] = 6;
	lut[b'7' as usize] = 7;
	lut[b'8' as usize] = 8;
	lut[b'9' as usize] = 9;
	lut[b'a' as usize] = 10;
	lut[b'b' as usize] = 11;
	lut[b'c' as usize] = 12;
	lut[b'd' as usize] = 13;
	lut[b'e' as usize] = 14;
	lut[b'f' as usize] = 15;
	lut[b'A' as usize] = 10;
	lut[b'B' as usize] = 11;
	lut[b'C' as usize] = 12;
	lut[b'D' as usize] = 13;
	lut[b'E' as usize] = 14;
	lut[b'F' as usize] = 15;
	lut
};
