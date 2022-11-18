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
	compile_to_fail(&[404], "tests/samples/invalid/missing_structure.pn")
}

#[test]
fn fail_to_scope_cyclical_structures()
{
	compile_to_fail(
		&[414, 414, 414],
		"tests/samples/invalid/cyclical_structures.pn",
	)
}

#[test]
fn fail_to_scope_missing_member()
{
	compile_to_fail(&[406], "tests/samples/invalid/missing_member.pn")
}
