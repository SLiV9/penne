//
// Part of penne
// Copyright (c) 2020 Sander in 't Veld
// License: MIT
//

use penne::*;

use anyhow::anyhow;

fn compile(filename: &str) -> Result<Vec<Declaration>, Errors>
{
	let source = std::fs::read_to_string(filename).unwrap();
	penne::compile_source(&source, &filename)
}

#[test]
fn fail_to_resolve_bitshift_without_types() -> Result<(), anyhow::Error>
{
	let analysis_result =
		compile("tests/samples/invalid/bitshift_without_types.pn");
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
		compile("tests/samples/invalid/bitshift_type_mismatch.pn");
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
		compile("tests/samples/invalid/bitshift_non_integer.pn");
	match analysis_result
	{
		Ok(_) => Err(anyhow!("broken test")),
		Err(_) => Ok(()),
	}
}

#[test]
fn fail_to_resolve_negative_u32() -> Result<(), anyhow::Error>
{
	let analysis_result = compile("tests/samples/invalid/negative_u32.pn");
	match analysis_result
	{
		Ok(_) => Err(anyhow!("broken test")),
		Err(_) => Ok(()),
	}
}

#[test]
fn fail_to_resolve_bitwise_not_usize() -> Result<(), anyhow::Error>
{
	let analysis_result = compile("tests/samples/invalid/bitwise_not_usize.pn");
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
		compile("tests/samples/invalid/comparison_on_arrays.pn");
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
		compile("tests/samples/invalid/comparison_ge_pointer.pn");
	match analysis_result
	{
		Ok(_) => Err(anyhow!("broken test")),
		Err(_) => Ok(()),
	}
}

#[test]
fn fail_to_resolve_invalid_casts() -> Result<(), anyhow::Error>
{
	let analysis_result = compile("tests/samples/invalid/invalid_casts.pn");
	match analysis_result
	{
		Ok(_) => Err(anyhow!("broken test")),
		Err(_) => Ok(()),
	}
}
