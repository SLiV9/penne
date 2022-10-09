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
fn allow_differing_local_variable_types() -> Result<(), anyhow::Error>
{
	let analysis_result =
		compile("tests/samples/valid/local_variable_types.pn");
	match analysis_result
	{
		Ok(_) => Ok(()),
		#[allow(unreachable_code)]
		Err(errors) => match errors.panic() {},
	}
}

#[test]
fn fail_to_type_return_type_mismatch() -> Result<(), anyhow::Error>
{
	let analysis_result =
		compile("tests/samples/invalid/return_type_mismatch.pn");
	match analysis_result
	{
		Ok(_) => Err(anyhow!("broken test")),
		Err(_) => Ok(()),
	}
}

#[test]
fn fail_to_type_constant_type_mismatch() -> Result<(), anyhow::Error>
{
	let analysis_result =
		compile("tests/samples/invalid/constant_type_mismatch.pn");
	match analysis_result
	{
		Ok(_) => Err(anyhow!("broken test")),
		Err(_) => Ok(()),
	}
}

#[test]
fn fail_to_type_missing_return() -> Result<(), anyhow::Error>
{
	let analysis_result = compile("tests/samples/invalid/missing_return.pn");
	match analysis_result
	{
		Ok(_) => Err(anyhow!("broken test")),
		Err(_) => Ok(()),
	}
}

#[test]
fn fail_to_type_mismatched_variable_type() -> Result<(), anyhow::Error>
{
	let analysis_result =
		compile("tests/samples/invalid/mismatched_variable_type.pn");
	match analysis_result
	{
		Ok(_) => Err(anyhow!("broken test")),
		Err(_) => Ok(()),
	}
}

#[test]
fn fail_to_type_mismatched_assign() -> Result<(), anyhow::Error>
{
	let analysis_result = compile("tests/samples/invalid/mismatched_assign.pn");
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
		compile("tests/samples/invalid/mismatched_array_elements.pn");
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
		compile("tests/samples/invalid/mismatched_array_type.pn");
	match analysis_result
	{
		Ok(_) => Err(anyhow!("broken test")),
		Err(_) => Ok(()),
	}
}

#[test]
fn fail_to_type_length_of_int() -> Result<(), anyhow::Error>
{
	let analysis_result = compile("tests/samples/invalid/length_of_int.pn");
	match analysis_result
	{
		Ok(_) => Err(anyhow!("broken test")),
		Err(_) => Ok(()),
	}
}
