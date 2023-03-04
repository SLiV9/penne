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
fn allow_empty_file_with_comment()
{
	allow_to_compile("tests/samples/valid/empty_file_with_comment.pn")
}

#[test]
fn allow_empty_file_with_space()
{
	allow_to_compile("tests/samples/valid/empty_file_with_space.pn")
}

#[test]
fn allow_empty_file_with_tab()
{
	allow_to_compile("tests/samples/valid/empty_file_with_tab.pn")
}

#[test]
fn allow_empty_file_with_newline()
{
	allow_to_compile("tests/samples/valid/empty_file_with_newline.pn")
}

#[test]
fn allow_use_of_untyped_array_in_wasm4_memset()
{
	allow_to_compile("tests/samples/valid/wasm4_memset_without_parentheses.pn")
}

#[test]
fn allow_bitwise_expression_in_wasm4_memset()
{
	allow_to_compile("tests/samples/valid/wasm4_memset.pn")
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

#[test]
fn allow_sketch_of_listen_to_client()
{
	allow_to_compile("examples/listen_to_client.pn")
}

#[test]
fn investigate_nominal_typing()
{
	let declarations = match compile("tests/samples/valid/nominal_typing.pn")
	{
		Ok(declarations) => declarations,
		#[allow(unreachable_code)]
		Err(errors) => match errors.panic() {},
	};
	let members = match declarations.into_iter().next()
	{
		Some(Declaration::Structure { members, .. }) => members,
		_ => panic!("broken test"),
	};
	assert_eq!(members.len(), 3);
	let member_types: Vec<value_type::ValueType<resolved::Identifier>> =
		members.into_iter().map(|m| m.value_type).collect();
	assert_ne!(member_types[0], member_types[1]);
	assert_eq!(member_types[0], member_types[2]);
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
		&[552, 552, 552, 552, 552],
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
