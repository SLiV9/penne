//
// Part of penne
// Copyright (c) 2020 Sander in 't Veld
// License: MIT
//

use penne::common;
use penne::lexer;
use penne::parser;
use penne::scoper;
use penne::typer;

use anyhow::anyhow;

fn do_type(
	filename: &str,
) -> Result<Result<Vec<common::Declaration>, anyhow::Error>, anyhow::Error>
{
	let source = std::fs::read_to_string(filename)?;
	let tokens = lexer::lex(&source, filename);
	let declarations = parser::parse(tokens)?;
	let declarations = scoper::analyze(declarations)?;
	Ok(typer::analyze(declarations))
}

#[test]
fn allow_differing_local_variable_types() -> Result<(), anyhow::Error>
{
	let analysis_result =
		do_type("tests/samples/valid/local_variable_types.pn")?;
	match analysis_result
	{
		Ok(_) => Ok(()),
		Err(error) => Err(error),
	}
}

#[test]
fn fail_to_type_return_type_mismatch() -> Result<(), anyhow::Error>
{
	let analysis_result =
		do_type("tests/samples/invalid/return_type_mismatch.pn")?;
	match analysis_result
	{
		Ok(_) => Err(anyhow!("broken test")),
		Err(_) => Ok(()),
	}
}

#[test]
fn fail_to_type_missing_return() -> Result<(), anyhow::Error>
{
	let analysis_result = do_type("tests/samples/invalid/missing_return.pn")?;
	match analysis_result
	{
		Ok(_) => Err(anyhow!("broken test")),
		Err(_) => Ok(()),
	}
}

#[test]
fn fail_to_type_mismatched_assign() -> Result<(), anyhow::Error>
{
	let analysis_result =
		do_type("tests/samples/invalid/mismatched_assign.pn")?;
	match analysis_result
	{
		Ok(_) => Err(anyhow!("broken test")),
		Err(_) => Ok(()),
	}
}

#[test]
fn fail_to_type_mismatched_array_elements() -> Result<(), anyhow::Error>
{
	let analysis_result =
		do_type("tests/samples/invalid/mismatched_array_elements.pn")?;
	match analysis_result
	{
		Ok(_) => Err(anyhow!("broken test")),
		Err(_) => Ok(()),
	}
}

#[test]
fn fail_to_type_mismatched_array_type() -> Result<(), anyhow::Error>
{
	let analysis_result =
		do_type("tests/samples/invalid/mismatched_array_type.pn")?;
	match analysis_result
	{
		Ok(_) => Err(anyhow!("broken test")),
		Err(_) => Ok(()),
	}
}

#[test]
fn fail_to_type_length_of_int() -> Result<(), anyhow::Error>
{
	let analysis_result = do_type("tests/samples/invalid/length_of_int.pn")?;
	match analysis_result
	{
		Ok(_) => Err(anyhow!("broken test")),
		Err(_) => Ok(()),
	}
}
