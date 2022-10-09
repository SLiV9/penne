//
// Part of penne
// Copyright (c) 2020 Sander in 't Veld
// License: MIT
//

use penne::*;

use anyhow::anyhow;

fn parse(filename: &str) -> Result<Vec<Declaration>, Errors>
{
	let source = std::fs::read_to_string(filename).unwrap();
	penne::compile_source(&source, &filename)
}

#[test]
fn fail_to_parse_invalid_character() -> Result<(), anyhow::Error>
{
	let parse_result = parse("tests/samples/invalid/invalid_character.pn");
	match parse_result
	{
		Ok(_) => Err(anyhow!("broken test")),
		Err(_) => Ok(()),
	}
}

#[test]
fn fail_to_parse_invalid_escape() -> Result<(), anyhow::Error>
{
	let parse_result = parse("tests/samples/invalid/invalid_escape.pn");
	match parse_result
	{
		Ok(_) => Err(anyhow!("broken test")),
		Err(_) => Ok(()),
	}
}

#[test]
fn fail_to_parse_invalid_trailing_slash_in_string() -> Result<(), anyhow::Error>
{
	let parse_result =
		parse("tests/samples/invalid/invalid_trailing_slash_in_string.pn");
	match parse_result
	{
		Ok(_) => Err(anyhow!("broken test")),
		Err(_) => Ok(()),
	}
}

#[test]
fn fail_to_parse_missing_closing_quote() -> Result<(), anyhow::Error>
{
	let parse_result = parse("tests/samples/invalid/missing_closing_quote.pn");
	match parse_result
	{
		Ok(_) => Err(anyhow!("broken test")),
		Err(_) => Ok(()),
	}
}

#[test]
fn fail_to_parse_integer_too_big() -> Result<(), anyhow::Error>
{
	let parse_result = parse("tests/samples/invalid/integer_too_big.pn");
	match parse_result
	{
		Ok(_) => Err(anyhow!("broken test")),
		Err(_) => Ok(()),
	}
}

#[test]
fn fail_to_parse_duplicate_return() -> Result<(), anyhow::Error>
{
	let parse_result = parse("tests/samples/invalid/duplicate_return.pn");
	match parse_result
	{
		Ok(_) => Err(anyhow!("broken test")),
		Err(_) => Ok(()),
	}
}

#[test]
fn fail_to_parse_empty_return() -> Result<(), anyhow::Error>
{
	let parse_result = parse("tests/samples/invalid/empty_return.pn");
	match parse_result
	{
		Ok(_) => Err(anyhow!("broken test")),
		Err(_) => Ok(()),
	}
}
