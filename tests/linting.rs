//
// Part of penne
// Copyright (c) 2020 Sander in 't Veld
// License: MIT
//

use penne::lexer;
use penne::linter;
use penne::parser;
use penne::scoper;
use penne::typer;

use anyhow::anyhow;

fn lint(filename: &str) -> Result<Vec<anyhow::Error>, anyhow::Error>
{
	let source = std::fs::read_to_string(filename)?;
	let tokens = lexer::lex(&source, filename);
	let declarations = parser::parse(tokens)?;
	let declarations = scoper::analyze(declarations)?;
	let declarations = typer::analyze(declarations)?;
	Ok(linter::lint(&declarations))
}

#[test]
fn trigger_lint_for_loop_in_branch() -> Result<(), anyhow::Error>
{
	let analysis_result = lint("examples/loop_in_branch.pn")?;
	match analysis_result.iter().next()
	{
		Some(_) => Ok(()),
		None => Err(anyhow!("broken test")),
	}
}
