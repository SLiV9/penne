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
fn fail_to_scope_duplicate_label() -> Result<(), anyhow::Error>
{
	let analysis_result = compile("tests/samples/invalid/duplicate_label.pn");
	match analysis_result
	{
		Ok(_) => Err(anyhow!("broken test")),
		Err(_) => Ok(()),
	}
}

#[test]
fn fail_to_scope_misplaced_variable() -> Result<(), anyhow::Error>
{
	let analysis_result =
		compile("tests/samples/invalid/misplaced_variable.pn");
	match analysis_result
	{
		Ok(_) => Err(anyhow!("broken test")),
		Err(_) => Ok(()),
	}
}

#[test]
fn fail_to_scope_foobar_variable() -> Result<(), anyhow::Error>
{
	let analysis_result = compile("tests/samples/invalid/foobar_variable.pn");
	match analysis_result
	{
		Ok(_) => Err(anyhow!("broken test")),
		Err(_) => Ok(()),
	}
}

#[test]
fn fail_to_scope_illegal_jump_back() -> Result<(), anyhow::Error>
{
	let analysis_result = compile("tests/samples/invalid/illegal_jump_back.pn");
	match analysis_result
	{
		Ok(_) => Err(anyhow!("broken test")),
		Err(_) => Ok(()),
	}
}

#[test]
fn fail_to_scope_label_in_else() -> Result<(), anyhow::Error>
{
	let analysis_result = compile("tests/samples/invalid/label_in_else.pn");
	match analysis_result
	{
		Ok(_) => Err(anyhow!("broken test")),
		Err(_) => Ok(()),
	}
}

#[test]
fn fail_to_scope_missing_label() -> Result<(), anyhow::Error>
{
	let analysis_result = compile("tests/samples/invalid/missing_label.pn");
	match analysis_result
	{
		Ok(_) => Err(anyhow!("broken test")),
		Err(_) => Ok(()),
	}
}

#[test]
fn fail_to_scope_missing_variable() -> Result<(), anyhow::Error>
{
	let analysis_result = compile("tests/samples/invalid/missing_variable.pn");
	match analysis_result
	{
		Ok(_) => Err(anyhow!("broken test")),
		Err(_) => Ok(()),
	}
}

#[test]
fn fail_to_scope_missing_function() -> Result<(), anyhow::Error>
{
	let analysis_result = compile("tests/samples/invalid/missing_function.pn");
	match analysis_result
	{
		Ok(_) => Err(anyhow!("broken test")),
		Err(_) => Ok(()),
	}
}

#[test]
fn fail_to_scope_outscoped_variable() -> Result<(), anyhow::Error>
{
	let analysis_result =
		compile("tests/samples/invalid/outscoped_variable.pn");
	match analysis_result
	{
		Ok(_) => Err(anyhow!("broken test")),
		Err(_) => Ok(()),
	}
}
