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

fn allow_to_compile(filename: &str)
{
	match compile(filename)
	{
		Ok(_) => (),
		#[allow(unreachable_code)]
		Err(errors) => match errors.panic() {},
	}
}

#[test]
fn allow_use_of_untyped_array_in_memset()
{
	allow_to_compile("tests/samples/valid/wasm4_memset_without_parentheses.pn")
}

#[test]
fn allow_skipped_region_in_wasm4_pixel()
{
	allow_to_compile("tests/samples/valid/wasm4_pixel.pn")
}

#[test]
fn allow_dubious_dangling_pointers()
{
	allow_to_compile("tests/samples/valid/dubious_dangling_pointers.pn")
}

#[test]
fn allow_dubious_conditional_definition()
{
	allow_to_compile("tests/samples/valid/dubious_conditional_definition.pn")
}

#[test]
fn allow_skip_shortlived_var()
{
	allow_to_compile("tests/samples/valid/skip_shortlived_var.pn")
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
fn fail_to_resolve_bitshift_without_types()
{
	compile_to_fail(&[581], "tests/samples/invalid/bitshift_without_types.pn")
}

#[test]
fn fail_to_resolve_bitshift_type_mismatch()
{
	compile_to_fail(
		&[581, 551],
		"tests/samples/invalid/bitshift_type_mismatch.pn",
	)
}

#[test]
fn fail_to_resolve_bitshift_non_integer()
{
	compile_to_fail(&[550], "tests/samples/invalid/bitshift_non_integer.pn")
}

#[test]
fn fail_to_resolve_negative_u32()
{
	compile_to_fail(&[550], "tests/samples/invalid/negative_u32.pn")
}

#[test]
fn fail_to_resolve_bitwise_not_usize()
{
	compile_to_fail(&[550], "tests/samples/invalid/bitwise_not_usize.pn")
}

#[test]
fn fail_to_resolve_comparison_on_arrays()
{
	compile_to_fail(&[550], "tests/samples/invalid/comparison_on_arrays.pn")
}

#[test]
fn fail_to_resolve_comparison_ge_pointer()
{
	compile_to_fail(&[550], "tests/samples/invalid/comparison_ge_pointer.pn")
}

#[test]
fn fail_to_resolve_invalid_casts()
{
	compile_to_fail(
		&[552, 552, 552, 552],
		"tests/samples/invalid/invalid_casts.pn",
	)
}

#[test]
fn fail_to_resolve_multiple_errors()
{
	compile_to_fail(
		&[581, 581, 402, 402],
		"tests/samples/invalid/multiple_errors.pn",
	)
}
