//
// Part of penne
// Copyright (c) 2020 Sander in 't Veld
// License: MIT
//

use penne::common;
use penne::lexer;
use penne::parser;
use penne::resolver;
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

fn resolve(
	filename: &str,
) -> Result<Result<Vec<common::Declaration>, anyhow::Error>, anyhow::Error>
{
	let source = std::fs::read_to_string(filename)?;
	let tokens = lexer::lex(&source, filename);
	let declarations = parser::parse(tokens)?;
	let declarations = scoper::analyze(declarations)?;
	let declarations = typer::analyze(declarations)?;
	Ok(resolver::analyze(declarations))
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

#[test]
fn fail_to_resolve_bitshift_without_types() -> Result<(), anyhow::Error>
{
	let analysis_result =
		resolve("tests/samples/invalid/bitshift_without_types.pn")?;
	match analysis_result
	{
		Ok(_) => Err(anyhow!("broken test")),
		Err(_) => Ok(()),
	}
}

#[test]
fn fail_to_resolve_bitshift_type_mismatch() -> Result<(), anyhow::Error>
{
	let analysis_result =
		resolve("tests/samples/invalid/bitshift_type_mismatch.pn")?;
	match analysis_result
	{
		Ok(_) => Err(anyhow!("broken test")),
		Err(_) => Ok(()),
	}
}

#[test]
fn fail_to_resolve_bitshift_non_integer() -> Result<(), anyhow::Error>
{
	let analysis_result =
		resolve("tests/samples/invalid/bitshift_non_integer.pn")?;
	match analysis_result
	{
		Ok(_) => Err(anyhow!("broken test")),
		Err(_) => Ok(()),
	}
}

#[test]
fn fail_to_resolve_negative_u32() -> Result<(), anyhow::Error>
{
	let analysis_result = resolve("tests/samples/invalid/negative_u32.pn")?;
	match analysis_result
	{
		Ok(_) => Err(anyhow!("broken test")),
		Err(_) => Ok(()),
	}
}

#[test]
fn fail_to_resolve_bitwise_not_usize() -> Result<(), anyhow::Error>
{
	let analysis_result =
		resolve("tests/samples/invalid/bitwise_not_usize.pn")?;
	match analysis_result
	{
		Ok(_) => Err(anyhow!("broken test")),
		Err(_) => Ok(()),
	}
}

#[test]
fn fail_to_resolve_comparison_on_arrays() -> Result<(), anyhow::Error>
{
	let analysis_result =
		resolve("tests/samples/invalid/comparison_on_arrays.pn")?;
	match analysis_result
	{
		Ok(_) => Err(anyhow!("broken test")),
		Err(_) => Ok(()),
	}
}

#[test]
fn fail_to_resolve_comparison_ge_pointer() -> Result<(), anyhow::Error>
{
	let analysis_result =
		resolve("tests/samples/invalid/comparison_ge_pointer.pn")?;
	match analysis_result
	{
		Ok(_) => Err(anyhow!("broken test")),
		Err(_) => Ok(()),
	}
}
