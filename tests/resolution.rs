//
// Part of penne
// Copyright (c) 2020 Sander in 't Veld
// License: MIT
//

use penne::analyzer;
use penne::lexer;
use penne::parser;
use penne::resolved;
use penne::resolver;
use penne::scoper;
use penne::typer;

use anyhow::anyhow;

fn resolve(
	filename: &str,
) -> Result<Result<Vec<resolved::Declaration>, resolver::Errors>, anyhow::Error>
{
	let source = std::fs::read_to_string(filename)?;
	let tokens = lexer::lex(&source, filename);
	let declarations = parser::parse(tokens);
	let declarations = scoper::analyze(declarations)?;
	let declarations = typer::analyze(declarations)?;
	analyzer::analyze(&declarations)?;
	Ok(resolver::resolve(declarations))
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
