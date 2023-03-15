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

#[test]
fn allow_omit_type_of_array()
{
	match compile("tests/samples/valid/implicit_array_type.pn")
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
	compile_to_fail(
		&[504, 504, 504, 504],
		"tests/samples/invalid/assignment_type_mismatch.pn",
	)
}

#[test]
fn fail_to_type_member_assignment_type_mismatch()
{
	compile_to_fail(
		&[504, 504, 504, 507, 507, 504, 504, 507],
		"tests/samples/invalid/member_assignment_type_mismatch.pn",
	)
}

#[test]
fn fail_to_type_assignment_address_and_type_mismatch()
{
	compile_to_fail(
		&[507],
		"tests/samples/invalid/assignment_address_and_type_mismatch.pn",
	)
}

#[test]
fn fail_to_type_assignment_address_mismatch()
{
	compile_to_fail(
		&[507, 507, 507, 507, 507, 507, 507, 507, 507, 507],
		"tests/samples/invalid/assignment_address_mismatch.pn",
	)
}

#[test]
fn fail_to_type_assignment_excess_address()
{
	compile_to_fail(
		&[506, 506, 506, 506],
		"tests/samples/invalid/assignment_excess_address.pn",
	)
}

#[test]
fn fail_to_type_declaration_type_mismatch()
{
	compile_to_fail(
		&[504],
		"tests/samples/invalid/declaration_type_mismatch.pn",
	)
}

#[test]
fn fail_to_type_initialization_type_mismatch()
{
	compile_to_fail(
		&[504],
		"tests/samples/invalid/initialization_type_mismatch.pn",
	)
}

#[test]
fn fail_to_type_assign_from_void()
{
	compile_to_fail(&[504], "tests/samples/invalid/assign_from_void.pn")
}

#[test]
fn fail_to_type_missing_return_type()
{
	compile_to_fail(
		&[330, 582, 331],
		"tests/samples/invalid/missing_return_type.pn",
	)
}

#[test]
fn fail_to_type_return_type_mismatch()
{
	compile_to_fail(&[333], "tests/samples/invalid/return_type_mismatch.pn")
}

#[test]
fn fail_to_type_missing_mandatory_type()
{
	compile_to_fail(
		&[343, 344, 346],
		"tests/samples/invalid/missing_mandatory_type.pn",
	)
}

#[test]
fn fail_to_type_implicit_pointer_type()
{
	compile_to_fail(
		&[585, 585, 585],
		"tests/samples/invalid/implicit_pointer_type.pn",
	)
}

#[test]
fn fail_to_type_implicit_uninitialized_pointer_type()
{
	compile_to_fail(
		&[585],
		"tests/samples/invalid/implicit_uninitialized_pointer_type.pn",
	)
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
fn fail_to_type_mismatched_assign()
{
	compile_to_fail(&[504], "tests/samples/invalid/mismatched_assign.pn")
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
	compile_to_fail(&[504], "tests/samples/invalid/mismatched_array_type.pn")
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

#[test]
fn fail_to_type_endless_array_as_variable()
{
	compile_to_fail(
		&[352],
		"tests/samples/invalid/endless_array_as_variable.pn",
	)
}

#[test]
fn fail_to_type_endless_array_as_constant()
{
	compile_to_fail(
		&[353],
		"tests/samples/invalid/endless_array_as_constant.pn",
	)
}

#[test]
fn fail_to_type_endless_array_as_member()
{
	compile_to_fail(&[356], "tests/samples/invalid/endless_array_as_member.pn")
}

#[test]
fn fail_to_type_view_as_member()
{
	compile_to_fail(&[356], "tests/samples/invalid/view_as_member.pn")
}

#[test]
fn fail_to_type_void_as_parameter()
{
	compile_to_fail(&[354], "tests/samples/invalid/void_as_parameter.pn")
}

#[test]
fn fail_to_type_word_size_exceeded()
{
	compile_to_fail(&[380, 380], "tests/samples/invalid/word_size_exceeded.pn")
}

#[test]
fn fail_to_type_array_in_word()
{
	compile_to_fail(&[356], "tests/samples/invalid/array_in_word.pn")
}

#[test]
fn fail_to_type_pointer_in_word()
{
	compile_to_fail(&[356], "tests/samples/invalid/pointer_in_word.pn")
}

#[test]
fn fail_to_scope_member_of_int()
{
	compile_to_fail(&[505], "tests/samples/invalid/member_of_int.pn")
}

#[test]
fn fail_to_scope_member_of_int_literal()
{
	compile_to_fail(&[300], "tests/samples/invalid/member_of_int_literal.pn")
}

#[test]
fn fail_to_type_word_casting()
{
	compile_to_fail(&[552, 552], "tests/samples/invalid/word_casting.pn")
}

#[test]
fn fail_to_type_non_abi_in_extern()
{
	compile_to_fail(&[358, 358], "tests/samples/invalid/non_abi_in_extern.pn")
}

#[test]
fn fail_to_type_ambiguous_bit_integer()
{
	compile_to_fail(&[582], "tests/samples/invalid/ambiguous_bit_integer.pn")
}
