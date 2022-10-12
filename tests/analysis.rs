//
// Part of penne
// Copyright (c) 2020 Sander in 't Veld
// License: MIT
//

use penne::*;

use pretty_assertions::assert_eq;

fn compile(filename: &str) -> Result<Vec<Declaration>, Errors>
{
	let source = std::fs::read_to_string(filename).unwrap();
	penne::compile_source(&source, &filename)
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
fn fail_to_analyze_nested_naked_if()
{
	compile_to_fail(&[840], "tests/samples/invalid/nested_naked_if.pn");
}

#[test]
fn fail_to_analyze_loop_in_function()
{
	compile_to_fail(&[801], "tests/samples/invalid/loop_in_function.pn");
}

#[test]
fn fail_to_analyze_loop_in_naked_branch()
{
	compile_to_fail(&[840], "tests/samples/invalid/loop_in_naked_branch.pn");
}

#[test]
fn fail_to_analyze_loop_nonfinal()
{
	compile_to_fail(&[800], "tests/samples/invalid/loop_nonfinal.pn");
}

#[test]
fn fail_to_analyze_argument_type_mismatch()
{
	compile_to_fail(&[512], "tests/samples/invalid/argument_type_mismatch.pn");
}

#[test]
fn fail_to_analyze_too_few_arguments()
{
	compile_to_fail(&[510], "tests/samples/invalid/too_few_arguments.pn");
}

#[test]
fn fail_to_analyze_too_many_arguments()
{
	compile_to_fail(&[511], "tests/samples/invalid/too_many_arguments.pn");
}

#[test]
fn fail_to_analyze_assign_array_to_array()
{
	compile_to_fail(&[531], "tests/samples/invalid/assign_array_to_array.pn");
}

#[test]
fn fail_to_analyze_return_array_by_value()
{
	compile_to_fail(&[355], "tests/samples/invalid/return_array_by_value.pn");
}

#[test]
fn fail_to_analyze_skip_declaration()
{
	compile_to_fail(&[820], "tests/samples/invalid/skip_declaration.pn");
}

#[test]
fn fail_to_analyze_conditional_declaration()
{
	compile_to_fail(&[820], "tests/samples/invalid/conditional_declaration.pn");
}

#[test]
fn fail_to_analyze_var_in_naked_branch()
{
	compile_to_fail(&[840], "tests/samples/invalid/var_in_naked_branch.pn");
}

#[test]
fn fail_to_analyze_missing_address()
{
	compile_to_fail(&[513], "tests/samples/invalid/missing_address.pn");
}

#[test]
fn fail_to_analyze_missing_address_variable()
{
	compile_to_fail(
		&[500],
		"tests/samples/invalid/missing_address_variable.pn",
	);
}

#[test]
fn fail_to_analyze_pointer_to_temporary_pointer()
{
	compile_to_fail(
		&[533],
		"tests/samples/invalid/pointer_to_temporary_pointer.pn",
	);
}

#[test]
fn fail_to_analyze_pointer_to_parameter()
{
	compile_to_fail(&[530], "tests/samples/invalid/pointer_to_parameter.pn");
}

#[test]
fn fail_to_analyze_pointer_to_const()
{
	compile_to_fail(&[530], "tests/samples/invalid/pointer_to_const.pn");
}

#[test]
fn fail_to_analyze_assign_to_view_parameter()
{
	compile_to_fail(
		&[530],
		"tests/samples/invalid/assign_to_view_parameter.pn",
	);
}

#[test]
fn fail_to_analyze_assign_to_constant()
{
	compile_to_fail(&[530], "tests/samples/invalid/assign_to_constant.pn");
}

#[test]
fn fail_to_analyze_assign_to_array_slice()
{
	compile_to_fail(&[530], "tests/samples/invalid/assign_to_array_slice.pn");
}

#[test]
fn fail_to_analyze_assign_to_pointer_parameter()
{
	compile_to_fail(
		&[530],
		"tests/samples/invalid/assign_to_pointer_parameter.pn",
	);
}
