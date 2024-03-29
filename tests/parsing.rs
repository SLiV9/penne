//
// Part of penne
// Copyright (c) 2020 Sander in 't Veld
// License: MIT
//

use penne::lexer::Token;
use penne::resolved::{Expression, Statement};
use penne::*;

use pretty_assertions::assert_eq;

fn compile(filename: &str) -> Result<Vec<Declaration>, Errors>
{
	let source = std::fs::read_to_string(filename).unwrap();
	penne::compile_source(&source, &filename)
}

#[test]
fn lex_euro_in_string()
{
	let filename = "tests/samples/valid/euro_in_string.pn";
	let source = std::fs::read_to_string(filename).unwrap();
	let tokens = lexer::lex(&source, &filename);
	let gathered: Vec<(Token, std::ops::Range<usize>)> = tokens
		.into_iter()
		.map(|t| (t.result.unwrap(), t.location.span))
		.collect();
	let expected = [
		(Token::Fn, 0..2),
		(Token::Identifier(String::from("euro")), 3..7),
		(Token::ParenLeft, 7..8),
		(Token::ParenRight, 8..9),
		(Token::BraceLeft, 10..11),
		(Token::Var, 13..16),
		(Token::Identifier(String::from("x")), 17..18),
		(Token::Assignment, 19..20),
		(
			Token::StringLiteral {
				bytes: "€".as_bytes().to_vec(),
			},
			21..24,
		),
		(Token::Semicolon, 24..25),
		(Token::Identifier(String::from("end")), 27..30),
		(Token::Colon, 30..31),
		(Token::BraceRight, 32..33),
	];
	assert_eq!(&gathered[..], &expected[..]);
}

fn parse_string_in_file(filename: &str, expected: &str)
{
	let declarations = match compile(filename)
	{
		Ok(declarations) => declarations,
		#[allow(unreachable_code)]
		Err(errors) => match errors.panic() {},
	};
	let body = match declarations.into_iter().next()
	{
		Some(Declaration::Function { body, .. }) => body,
		_ => panic!("broken test"),
	};
	let statement = body.statements.into_iter().next();
	match statement
	{
		Some(Statement::Declaration {
			value: Some(Expression::StringLiteral { bytes, .. }),
			..
		}) =>
		{
			assert_eq!(&bytes[..], expected.as_bytes());
		}
		_ => panic!("broken test"),
	}
}

#[test]
fn parse_unicode_string_in_utf8_file()
{
	parse_string_in_file(
		"tests/samples/valid/unicode_string_in_utf8_file.pn",
		"$10/£10/€10. Aÿ!!! 하와!!!",
	)
}

#[test]
fn parse_string_escapes()
{
	parse_string_in_file(
		"tests/samples/valid/string_escapes.pn",
		"\n\r\t\'\"\0\\",
	)
}

#[test]
fn parse_euro_in_string()
{
	parse_string_in_file("tests/samples/valid/euro_in_string.pn", "€")
}

#[test]
fn parse_euro_in_string_escaped()
{
	parse_string_in_file("tests/samples/valid/euro_in_string_escaped.pn", "€")
}

fn compile_to_fail(codes: &[u16], filename: &str)
{
	match compile(filename)
	{
		Ok(_) => panic!("broken test"),
		Err(errors) =>
		{
			assert_eq!(errors.codes(), codes, "unexpected {:?}", errors)
		}
	}
}

#[test]
fn fail_to_parse_unexpected_end_of_file()
{
	compile_to_fail(&[100], "tests/samples/invalid/unexpected_end_of_file.pn")
}

#[test]
fn fail_to_parse_unexpected_end_of_import()
{
	compile_to_fail(&[100], "tests/samples/invalid/unexpected_end_of_import.pn")
}

#[test]
fn fail_to_parse_unexpected_end_of_declaration()
{
	compile_to_fail(
		&[100],
		"tests/samples/invalid/unexpected_end_of_declaration.pn",
	)
}

#[test]
fn fail_to_parse_empty_file()
{
	compile_to_fail(&[101], "tests/samples/invalid/empty_file.pn")
}

#[test]
fn fail_to_parse_invalid_character()
{
	compile_to_fail(&[110], "tests/samples/invalid/invalid_character.pn")
}

#[test]
fn fail_to_parse_invalid_character_in_identifier()
{
	compile_to_fail(
		&[110],
		"tests/samples/invalid/invalid_character_in_identifier.pn",
	)
}

#[test]
fn fail_to_parse_invalid_character_at_top_level()
{
	compile_to_fail(
		&[110],
		"tests/samples/invalid/invalid_character_at_top_level.pn",
	)
}

#[test]
fn fail_to_parse_invalid_character_in_string()
{
	compile_to_fail(
		&[110],
		"tests/samples/invalid/invalid_character_in_string.pn",
	)
}

#[test]
fn fail_to_parse_invalid_escape()
{
	compile_to_fail(&[162], "tests/samples/invalid/invalid_escape.pn")
}

#[test]
fn fail_to_parse_invalid_escape_hex()
{
	compile_to_fail(&[162], "tests/samples/invalid/invalid_escape_hex.pn")
}

#[test]
fn fail_to_parse_invalid_escape_unicode()
{
	compile_to_fail(&[162], "tests/samples/invalid/invalid_escape_unicode.pn")
}

#[test]
fn fail_to_parse_invalid_escape_empty()
{
	compile_to_fail(&[162], "tests/samples/invalid/invalid_escape_empty.pn")
}

#[test]
fn fail_to_parse_invalid_escape_unclosed()
{
	compile_to_fail(&[162], "tests/samples/invalid/invalid_escape_unclosed.pn")
}

#[test]
fn fail_to_parse_multiple_invalid_escapes()
{
	compile_to_fail(&[162], "tests/samples/invalid/multiple_invalid_escapes.pn")
}

#[test]
fn fail_to_parse_invalid_trailing_slash_in_string()
{
	compile_to_fail(
		&[161],
		"tests/samples/invalid/invalid_trailing_slash_in_string.pn",
	)
}

#[test]
fn fail_to_parse_missing_closing_quote()
{
	compile_to_fail(&[160], "tests/samples/invalid/missing_closing_quote.pn")
}

#[test]
fn fail_to_parse_empty_char()
{
	compile_to_fail(&[163], "tests/samples/invalid/empty_char.pn")
}

#[test]
fn fail_to_parse_multibyte_char()
{
	compile_to_fail(&[163], "tests/samples/invalid/multibyte_char.pn")
}

#[test]
fn fail_to_parse_missing_quotes_after_import()
{
	compile_to_fail(
		&[300],
		"tests/samples/invalid/missing_quotes_after_import.pn",
	)
}

#[test]
fn fail_to_parse_invalid_unicode_in_import()
{
	compile_to_fail(
		&[300],
		"tests/samples/invalid/invalid_unicode_in_import.pn",
	)
}

#[test]
fn fail_to_parse_invalid_space_before_suffix()
{
	compile_to_fail(
		&[300],
		"tests/samples/invalid/invalid_space_before_suffix.pn",
	)
}

#[test]
fn fail_to_parse_invalid_integer_suffix()
{
	compile_to_fail(&[141], "tests/samples/invalid/invalid_integer_suffix.pn")
}

#[test]
fn fail_to_parse_octal_literal()
{
	compile_to_fail(&[141], "tests/samples/invalid/octal_literal.pn")
}

#[test]
fn fail_to_parse_octal_suffix()
{
	compile_to_fail(&[141], "tests/samples/invalid/octal_suffix.pn")
}

#[test]
fn fail_to_parse_integer_too_big()
{
	compile_to_fail(&[140], "tests/samples/invalid/integer_too_big.pn")
}

#[test]
fn fail_to_parse_i32_too_big()
{
	compile_to_fail(&[140], "tests/samples/invalid/i32_too_big.pn")
}

#[test]
fn fail_to_parse_i128_too_big()
{
	compile_to_fail(&[140], "tests/samples/invalid/i128_too_big.pn")
}

#[test]
fn fail_to_parse_untyped_integer_too_big()
{
	compile_to_fail(&[581], "tests/samples/invalid/untyped_integer_too_big.pn")
}

#[test]
fn fail_to_parse_bit_integer_too_big()
{
	compile_to_fail(&[140], "tests/samples/invalid/bit_integer_too_big.pn")
}

#[test]
fn fail_to_parse_binary_integer_too_big()
{
	compile_to_fail(&[140], "tests/samples/invalid/binary_integer_too_big.pn")
}

#[test]
fn fail_to_parse_unfinished_function_signature()
{
	compile_to_fail(
		&[300],
		"tests/samples/invalid/unfinished_function_signature.pn",
	)
}

#[test]
fn fail_to_parse_import_without_semicolon()
{
	compile_to_fail(&[300], "tests/samples/invalid/import_without_semicolon.pn")
}

#[test]
fn fail_to_parse_non_existing_statement()
{
	compile_to_fail(&[301], "tests/samples/invalid/non_existing_statement.pn")
}

#[test]
fn fail_to_parse_duplicate_return()
{
	compile_to_fail(&[420], "tests/samples/invalid/duplicate_return.pn")
}

#[test]
fn fail_to_parse_early_return()
{
	compile_to_fail(&[300], "tests/samples/invalid/early_return.pn")
}

#[test]
fn fail_to_parse_early_return_in_void()
{
	compile_to_fail(&[301], "tests/samples/invalid/early_return_in_void.pn")
}

#[test]
fn fail_to_parse_return_as_end()
{
	compile_to_fail(&[302], "tests/samples/invalid/return_as_end.pn")
}

#[test]
fn fail_to_parse_empty_return()
{
	compile_to_fail(&[335], "tests/samples/invalid/empty_return.pn")
}

#[test]
fn fail_to_parse_empty_return_in_void()
{
	compile_to_fail(&[335], "tests/samples/invalid/empty_return_in_void.pn")
}

#[test]
fn fail_to_parse_missing_closing_parenthesis_after_parameters()
{
	compile_to_fail(
		&[300],
		"tests/samples/invalid/missing_closing_parenthesis_after_parameters.pn",
	)
}

#[test]
fn fail_to_parse_reserved_keyword_as_identifier()
{
	compile_to_fail(
		&[300, 300, 300],
		"tests/samples/invalid/reserved_keyword_as_identifier.pn",
	)
}

#[test]
fn fail_to_parse_reserved_keyword_as_declaration_identifier()
{
	compile_to_fail(
		&[300, 300, 300],
		"tests/samples/invalid/reserved_keyword_as_declaration_identifier.pn",
	)
}

#[test]
fn fail_to_parse_negative_array_length()
{
	compile_to_fail(&[300], "tests/samples/invalid/negative_array_length.pn")
}

#[test]
fn fail_to_parse_assign_to_array_slice_length()
{
	compile_to_fail(
		&[300, 300, 300],
		"tests/samples/invalid/assign_to_array_slice_length.pn",
	)
}

#[test]
fn fail_to_parse_address_of_literal()
{
	compile_to_fail(
		&[300, 300, 300],
		"tests/samples/invalid/address_of_literal.pn",
	)
}

#[test]
fn fail_to_parse_type_before_member()
{
	compile_to_fail(&[300, 300], "tests/samples/invalid/type_before_member.pn")
}

#[test]
fn fail_to_parse_expression_as_type()
{
	compile_to_fail(&[300], "tests/samples/invalid/expression_as_type.pn")
}

#[test]
fn fail_to_parse_bool_directly_as_condition()
{
	compile_to_fail(
		&[300],
		"tests/samples/invalid/bool_directly_as_condition.pn",
	)
}

#[test]
fn fail_to_parse_bitwise_binary_expression()
{
	compile_to_fail(
		&[300],
		"tests/samples/invalid/bitwise_binary_expression.pn",
	)
}

#[test]
fn fail_to_parse_bitshift_binary_expression()
{
	compile_to_fail(
		&[300],
		"tests/samples/invalid/bitshift_binary_expression.pn",
	)
}

#[test]
fn fail_to_parse_expression_in_array_length()
{
	compile_to_fail(
		&[300, 300, 300],
		"tests/samples/invalid/expression_in_array_length.pn",
	)
}

#[test]
fn fail_to_parse_var_underscore()
{
	compile_to_fail(&[300], "tests/samples/invalid/var_underscore.pn")
}

#[test]
fn fail_to_parse_declare_builtin()
{
	compile_to_fail(&[300], "tests/samples/invalid/declare_builtin.pn")
}
