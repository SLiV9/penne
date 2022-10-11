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

#[test]
fn allow_differing_local_variable_types()
{
	match compile("tests/samples/valid/local_variable_types.pn")
	{
		Ok(_) => (),
		#[allow(unreachable_code)]
		Err(errors) => match errors.panic() {},
	}
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
fn fail_to_type_assignment_type_mismatch()
{
	compile_to_fail(&[500], "tests/samples/invalid/assignment_type_mismatch.pn")
}

#[test]
fn fail_to_type_declaration_type_mismatch()
{
	compile_to_fail(
		&[500],
		"tests/samples/invalid/declaration_type_mismatch.pn",
	)
}

#[test]
fn fail_to_type_initialization_type_mismatch()
{
	compile_to_fail(
		&[500],
		"tests/samples/invalid/initialization_type_mismatch.pn",
	)
}

#[test]
fn fail_to_type_return_type_mismatch()
{
	compile_to_fail(&[333], "tests/samples/invalid/return_type_mismatch.pn")
}

#[test]
fn fail_to_type_constant_type_mismatch()
{
	compile_to_fail(&[500], "tests/samples/invalid/constant_type_mismatch.pn")
}

#[test]
fn fail_to_type_missing_return()
{
	compile_to_fail(&[334], "tests/samples/invalid/missing_return.pn")
}

#[test]
fn fail_to_type_mismatched_variable_type()
{
	compile_to_fail(&[500], "tests/samples/invalid/mismatched_variable_type.pn")
}

#[test]
fn fail_to_type_mismatched_assign()
{
	compile_to_fail(&[500], "tests/samples/invalid/mismatched_assign.pn")
}

#[test]
fn fail_to_type_mismatched_array_elements()
{
	compile_to_fail(
		&[500],
		"tests/samples/invalid/mismatched_array_elements.pn",
	)
}

#[test]
fn fail_to_type_mismatched_array_type()
{
	compile_to_fail(&[500], "tests/samples/invalid/mismatched_array_type.pn")
}

#[test]
fn fail_to_type_length_of_int()
{
	compile_to_fail(&[502], "tests/samples/invalid/length_of_int.pn")
}

#[test]
fn fail_to_type_index_into_int()
{
	compile_to_fail(&[501, 501], "tests/samples/invalid/index_into_int.pn")
}

#[test]
fn fail_to_type_arraylike_of_arraylike()
{
	compile_to_fail(&[350], "tests/samples/invalid/arraylike_of_arraylike.pn")
}

#[test]
fn fail_to_type_array_of_endless_array()
{
	compile_to_fail(&[350], "tests/samples/invalid/array_of_endless_array.pn")
}
