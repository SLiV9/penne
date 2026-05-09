//
// Part of penne
// Copyright (c) 2020 Sander in 't Veld
// License: MIT
//

use penne::test_suite::{lint_to_fail, lint_to_nothing};

#[test]
fn trigger_lint_for_loop_in_branch()
{
	lint_to_fail(&[1800], "examples/loop_in_branch.pn");
}

#[test]
fn trigger_multiple_lints()
{
	lint_to_fail(&[1800, 1800], "tests/samples/valid/multiple_lints.pn");
}

#[test]
fn trigger_lint_for_unintentional_integer_truncation()
{
	lint_to_fail(
		&[1142, 1142, 1142, 1142, 1142, 1142, 1142, 1142, 1142, 1142],
		"tests/samples/valid/unintentional_integer_truncation.pn",
	);
}

#[test]
fn no_false_positive_lints()
{
	lint_to_nothing("examples/collatz.pn")
}
