//
// Part of penne
// Copyright (c) 2020 Sander in 't Veld
// License: MIT
//

use penne::common;
use penne::lexer;
use penne::parser;
use penne::scoper;

use anyhow::anyhow;

fn do_scope(
	filename: &str,
) -> Result<Result<Vec<common::Declaration>, anyhow::Error>, anyhow::Error>
{
	let source = std::fs::read_to_string(filename)?;
	let tokens = lexer::lex(&source, filename);
	let declarations = parser::parse(tokens);
	Ok(scoper::analyze(declarations))
}

#[test]
fn fail_to_scope_duplicate_label() -> Result<(), anyhow::Error>
{
	let analysis_result = do_scope("tests/samples/invalid/duplicate_label.pn")?;
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
		do_scope("tests/samples/invalid/misplaced_variable.pn")?;
	match analysis_result
	{
		Ok(_) => Err(anyhow!("broken test")),
		Err(_) => Ok(()),
	}
}

#[test]
fn fail_to_scope_foobar_variable() -> Result<(), anyhow::Error>
{
	let analysis_result = do_scope("tests/samples/invalid/foobar_variable.pn")?;
	match analysis_result
	{
		Ok(_) => Err(anyhow!("broken test")),
		Err(_) => Ok(()),
	}
}

#[test]
fn fail_to_scope_illegal_jump_back() -> Result<(), anyhow::Error>
{
	let analysis_result =
		do_scope("tests/samples/invalid/illegal_jump_back.pn")?;
	match analysis_result
	{
		Ok(_) => Err(anyhow!("broken test")),
		Err(_) => Ok(()),
	}
}

#[test]
fn fail_to_scope_label_in_else() -> Result<(), anyhow::Error>
{
	let analysis_result = do_scope("tests/samples/invalid/label_in_else.pn")?;
	match analysis_result
	{
		Ok(_) => Err(anyhow!("broken test")),
		Err(_) => Ok(()),
	}
}

#[test]
fn fail_to_scope_missing_label() -> Result<(), anyhow::Error>
{
	let analysis_result = do_scope("tests/samples/invalid/missing_label.pn")?;
	match analysis_result
	{
		Ok(_) => Err(anyhow!("broken test")),
		Err(_) => Ok(()),
	}
}

#[test]
fn fail_to_scope_missing_variable() -> Result<(), anyhow::Error>
{
	let analysis_result =
		do_scope("tests/samples/invalid/missing_variable.pn")?;
	match analysis_result
	{
		Ok(_) => Err(anyhow!("broken test")),
		Err(_) => Ok(()),
	}
}

#[test]
fn fail_to_scope_missing_function() -> Result<(), anyhow::Error>
{
	let analysis_result =
		do_scope("tests/samples/invalid/missing_function.pn")?;
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
		do_scope("tests/samples/invalid/outscoped_variable.pn")?;
	match analysis_result
	{
		Ok(_) => Err(anyhow!("broken test")),
		Err(_) => Ok(()),
	}
}
