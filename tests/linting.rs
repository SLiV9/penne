//
// Part of penne
// Copyright (c) 2020 Sander in 't Veld
// License: MIT
//

use penne::analyzer;
use penne::expander;
use penne::lexer;
use penne::linter;
use penne::linter::Lint;
use penne::parser;
use penne::resolver;
use penne::scoper;
use penne::typer;

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
	let declarations = typer::analyze(declarations);
	let declarations = analyzer::analyze(declarations);
	let lints = linter::lint(&declarations);
	match resolver::resolve(declarations)
	{
		Ok(_) => (),
		#[allow(unreachable_code)]
		Err(errors) => match errors.panic() {},
	}
	lints
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
