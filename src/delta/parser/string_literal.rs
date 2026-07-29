use crate::delta::lexer::tokens::TokenLocation;

/// Source must be a span of source bytes that has already been
/// lexed without errors at an earlier compiler stage.
pub fn relex_string_literal(source_with_quotes: &str) -> Vec<u8>
{
	let source = source_with_quotes;
	let mut bytes = Vec::with_capacity(source.len());

	let mut iter = source.bytes().enumerate().peekable();

	while let Some(x) = iter.next()
	{
		// The span of a (composite) string literal starts with the opening quote.
		let (_, opening_quote) = x;
		let result = crate::delta::lexer::lex_string_literal(
			opening_quote,
			&mut TokenLocation::dummy(),
			&mut iter,
			|byte| bytes.push(byte),
		);
		result.expect("already lexed this during lexing stage");

		// There may be whitespace in between the parts of a composite literal.
		while let Some(_) = iter.next_if(|(_, x)| b" \t\r\n".contains(x))
		{
			// Ignore.
		}
	}

	// It would be very nice if this is true,
	// and I think it can be proven.
	debug_assert!(bytes.len() <= source.len());
	bytes
}
