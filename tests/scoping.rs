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
fn fail_to_scope_duplicate_label()
{
	compile_to_fail(&[420], "tests/samples/invalid/duplicate_label.pn")
}

#[test]
fn fail_to_scope_duplicate_declaration()
{
	compile_to_fail(
		&[423, 423, 422, 422, 421, 421, 424, 424, 426, 426, 425, 425],
		"tests/samples/invalid/duplicate_declaration.pn",
	)
}

#[test]
fn fail_to_scope_misplaced_variable()
{
	compile_to_fail(&[402], "tests/samples/invalid/misplaced_variable.pn")
}

#[test]
fn fail_to_scope_foobar_variable()
{
	compile_to_fail(&[402], "tests/samples/invalid/foobar_variable.pn")
}

#[test]
fn fail_to_scope_illegal_jump_back()
{
	compile_to_fail(&[400], "tests/samples/invalid/illegal_jump_back.pn")
}

#[test]
fn fail_to_scope_label_in_else()
{
	compile_to_fail(&[400], "tests/samples/invalid/label_in_else.pn")
}

#[test]
fn fail_to_scope_missing_label()
{
	compile_to_fail(&[400], "tests/samples/invalid/missing_label.pn")
}

#[test]
fn fail_to_scope_jump_to_inner()
{
	compile_to_fail(&[400], "tests/samples/invalid/jump_to_inner.pn")
}

#[test]
fn fail_to_scope_missing_variable()
{
	compile_to_fail(&[402], "tests/samples/invalid/missing_variable.pn")
}

#[test]
fn fail_to_scope_missing_function()
{
	compile_to_fail(&[401], "tests/samples/invalid/missing_function.pn")
}

#[test]
fn fail_to_scope_outscoped_variable()
{
	compile_to_fail(&[402], "tests/samples/invalid/outscoped_variable.pn")
}

#[test]
fn fail_to_scope_missing_structure()
{
	compile_to_fail(&[405], "tests/samples/invalid/missing_structure.pn")
}

#[test]
fn fail_to_scope_assign_to_unknown_struct()
{
	compile_to_fail(
		&[405, 405, 405, 405],
		"tests/samples/invalid/assign_to_unknown_struct.pn",
	)
}

#[test]
fn fail_to_scope_cyclical_structures()
{
	compile_to_fail(
		&[415, 415, 415],
		"tests/samples/invalid/cyclical_structures.pn",
	)
}

#[test]
fn fail_to_scope_cyclical_constants()
{
	compile_to_fail(
		&[413, 413, 413],
		"tests/samples/invalid/cyclical_constants.pn",
	)
}

#[test]
fn fail_to_scope_cyclical_constants_and_structures()
{
	compile_to_fail(
		&[416, 416],
		"tests/samples/invalid/cyclical_constants_and_structures.pn",
	)
}

#[test]
fn fail_to_scope_constant_confusion()
{
	compile_to_fail(&[423], "tests/samples/invalid/constant_confusion.pn")
}

#[test]
fn fail_to_scope_missing_member()
{
	compile_to_fail(&[406], "tests/samples/invalid/missing_member.pn")
}

#[test]
fn fail_to_scope_misspelled_member()
{
	compile_to_fail(&[406, 406], "tests/samples/invalid/misspelled_member.pn")
}

#[test]
fn fail_to_parse_unresolved_import()
{
	compile_to_fail(&[470], "tests/samples/invalid/unresolved_import.pn")
}
