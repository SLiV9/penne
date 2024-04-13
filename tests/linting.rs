//
// Part of penne
// Copyright (c) 2020 Sander in 't Veld
// License: MIT
//

use penne::expander;
use penne::lexer;
use penne::linter::Lint;
use penne::parser;
use penne::resolver;
use penne::scoper;
use penne::Compiler;
use penne::DefaultGenerator;

use pretty_assertions::assert_eq;

fn lint(filename: &str) -> Vec<Lint>
{
	let source = std::fs::read_to_string(filename).unwrap();
	let tokens = lexer::lex(&source, filename);
	let declarations = parser::parse(tokens);
	let declarations = expander::expand_one(filename, declarations);
	match resolver::check_surface_level_errors(&declarations)
	{
		Ok(_) => (),
		#[allow(unreachable_code)]
		Err(errors) => match errors.panic() {},
	}
	let declarations = scoper::analyze(declarations);
	let mut compiler = Compiler::<DefaultGenerator>::default();
	match compiler.analyze_and_resolve(declarations).unwrap()
	{
		Ok(_) => (),
		#[allow(unreachable_code)]
		Err(errors) => match errors.panic() {},
	}
	compiler.take_lints()
}

fn lint_to_fail(codes: &[u16], filename: &str)
{
	let lints = lint(filename);
	let lint_codes: Vec<u16> = lints.iter().map(|x| x.code()).collect();
	assert_eq!(lint_codes, codes, "unexpected {:?}", lints);
}

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

fn lint_to_nothing(filename: &str)
{
	let lints = lint(filename);
	assert!(lints.is_empty(), "unexpected {:?}", lints);
}

#[test]
fn no_false_positive_lints()
{
	lint_to_nothing("examples/collatz.pn")
}
