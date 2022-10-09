//
// Part of penne
// Copyright (c) 2020 Sander in 't Veld
// License: MIT
//

use penne::*;

use anyhow::anyhow;

fn analyze(filename: &str) -> Result<Vec<Declaration>, Errors>
{
	let source = std::fs::read_to_string(filename).unwrap();
	penne::compile_source(&source, &filename)
}

#[test]
fn fail_to_analyze_nested_naked_if() -> Result<(), anyhow::Error>
{
	let analysis_result = analyze("tests/samples/invalid/nested_naked_if.pn");
	match analysis_result
	{
		Ok(_) => Err(anyhow!("broken test")),
		Err(_) => Ok(()),
	}
}

#[test]
fn fail_to_analyze_loop_in_function() -> Result<(), anyhow::Error>
{
	let analysis_result = analyze("tests/samples/invalid/loop_in_function.pn");
	match analysis_result
	{
		Ok(_) => Err(anyhow!("broken test")),
		Err(_) => Ok(()),
	}
}

#[test]
fn fail_to_analyze_loop_in_naked_branch() -> Result<(), anyhow::Error>
{
	let analysis_result =
		analyze("tests/samples/invalid/loop_in_naked_branch.pn");
	match analysis_result
	{
		Ok(_) => Err(anyhow!("broken test")),
		Err(_) => Ok(()),
	}
}

#[test]
fn fail_to_analyze_loop_nonfinal() -> Result<(), anyhow::Error>
{
	let analysis_result = analyze("tests/samples/invalid/loop_nonfinal.pn");
	match analysis_result
	{
		Ok(_) => Err(anyhow!("broken test")),
		Err(_) => Ok(()),
	}
}

#[test]
fn fail_to_analyze_argument_type_mismatch() -> Result<(), anyhow::Error>
{
	let analysis_result =
		analyze("tests/samples/invalid/argument_type_mismatch.pn");
	match analysis_result
	{
		Ok(_) => Err(anyhow!("broken test")),
		Err(_) => Ok(()),
	}
}

#[test]
fn fail_to_analyze_too_few_arguments() -> Result<(), anyhow::Error>
{
	let analysis_result = analyze("tests/samples/invalid/too_few_arguments.pn");
	match analysis_result
	{
		Ok(_) => Err(anyhow!("broken test")),
		Err(_) => Ok(()),
	}
}

#[test]
fn fail_to_analyze_too_many_arguments() -> Result<(), anyhow::Error>
{
	let analysis_result =
		analyze("tests/samples/invalid/too_many_arguments.pn");
	match analysis_result
	{
		Ok(_) => Err(anyhow!("broken test")),
		Err(_) => Ok(()),
	}
}

#[test]
fn fail_to_analyze_assign_array_to_array() -> Result<(), anyhow::Error>
{
	let analysis_result =
		analyze("tests/samples/invalid/assign_array_to_array.pn");
	match analysis_result
	{
		Ok(_) => Err(anyhow!("broken test")),
		Err(_) => Ok(()),
	}
}

#[test]
fn fail_to_analyze_skip_declaration() -> Result<(), anyhow::Error>
{
	let analysis_result = analyze("tests/samples/invalid/skip_declaration.pn");
	match analysis_result
	{
		Ok(_) => Err(anyhow!("broken test")),
		Err(_) => Ok(()),
	}
}

#[test]
fn fail_to_analyze_conditional_declaration() -> Result<(), anyhow::Error>
{
	let analysis_result =
		analyze("tests/samples/invalid/conditional_declaration.pn");
	match analysis_result
	{
		Ok(_) => Err(anyhow!("broken test")),
		Err(_) => Ok(()),
	}
}

#[test]
fn fail_to_analyze_var_in_naked_branch() -> Result<(), anyhow::Error>
{
	let analysis_result =
		analyze("tests/samples/invalid/var_in_naked_branch.pn");
	match analysis_result
	{
		Ok(_) => Err(anyhow!("broken test")),
		Err(_) => Ok(()),
	}
}

#[test]
fn fail_to_analyze_missing_address() -> Result<(), anyhow::Error>
{
	let analysis_result = analyze("tests/samples/invalid/missing_address.pn");
	match analysis_result
	{
		Ok(_) => Err(anyhow!("broken test")),
		Err(_) => Ok(()),
	}
}

#[test]
fn fail_to_analyze_pointer_to_temporary_pointer() -> Result<(), anyhow::Error>
{
	let analysis_result =
		analyze("tests/samples/invalid/pointer_to_temporary_pointer.pn");
	match analysis_result
	{
		Ok(_) => Err(anyhow!("broken test")),
		Err(_) => Ok(()),
	}
}

#[test]
fn fail_to_analyze_pointer_to_parameter() -> Result<(), anyhow::Error>
{
	let analysis_result =
		analyze("tests/samples/invalid/pointer_to_parameter.pn");
	match analysis_result
	{
		Ok(_) => Err(anyhow!("broken test")),
		Err(_) => Ok(()),
	}
}

#[test]
fn fail_to_analyze_pointer_to_const() -> Result<(), anyhow::Error>
{
	let analysis_result = analyze("tests/samples/invalid/pointer_to_const.pn");
	match analysis_result
	{
		Ok(_) => Err(anyhow!("broken test")),
		Err(_) => Ok(()),
	}
}

#[test]
fn fail_to_analyze_assign_to_view() -> Result<(), anyhow::Error>
{
	let analysis_result = analyze("tests/samples/invalid/assign_to_view.pn");
	match analysis_result
	{
		Ok(_) => Err(anyhow!("broken test")),
		Err(_) => Ok(()),
	}
}

#[test]
fn fail_to_analyze_assign_to_view_parameter() -> Result<(), anyhow::Error>
{
	let analysis_result =
		analyze("tests/samples/invalid/assign_to_view_parameter.pn");
	match analysis_result
	{
		Ok(_) => Err(anyhow!("broken test")),
		Err(_) => Ok(()),
	}
}

#[test]
fn fail_to_analyze_change_view_address() -> Result<(), anyhow::Error>
{
	let analysis_result =
		analyze("tests/samples/invalid/change_view_address.pn");
	match analysis_result
	{
		Ok(_) => Err(anyhow!("broken test")),
		Err(_) => Ok(()),
	}
}

#[test]
fn fail_to_analyze_null_view() -> Result<(), anyhow::Error>
{
	let analysis_result = analyze("tests/samples/invalid/null_view.pn");
	match analysis_result
	{
		Ok(_) => Err(anyhow!("broken test")),
		Err(_) => Ok(()),
	}
}

#[test]
fn fail_to_analyze_assign_to_constant() -> Result<(), anyhow::Error>
{
	let analysis_result =
		analyze("tests/samples/invalid/assign_to_constant.pn");
	match analysis_result
	{
		Ok(_) => Err(anyhow!("broken test")),
		Err(_) => Ok(()),
	}
}

#[test]
fn fail_to_analyze_assign_to_array_slice() -> Result<(), anyhow::Error>
{
	let analysis_result =
		analyze("tests/samples/invalid/assign_to_array_slice.pn");
	match analysis_result
	{
		Ok(_) => Err(anyhow!("broken test")),
		Err(_) => Ok(()),
	}
}

#[test]
fn fail_to_analyze_assign_to_pointer_parameter() -> Result<(), anyhow::Error>
{
	let analysis_result =
		analyze("tests/samples/invalid/assign_to_pointer_parameter.pn");
	match analysis_result
	{
		Ok(_) => Err(anyhow!("broken test")),
		Err(_) => Ok(()),
	}
}
