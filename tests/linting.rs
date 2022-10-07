//
// Part of penne
// Copyright (c) 2020 Sander in 't Veld
// License: MIT
//

use penne::analyzer;
use penne::lexer;
use penne::linter;
use penne::linter::Lint;
use penne::parser;
use penne::scoper;
use penne::typer;

use anyhow::anyhow;

fn lint(filename: &str) -> Result<Vec<Lint>, anyhow::Error>
{
	let source = std::fs::read_to_string(filename)?;
	let tokens = lexer::lex(&source, filename);
	let declarations = parser::parse(tokens);
	let declarations = scoper::analyze(declarations);
	let declarations = typer::analyze(declarations);
	analyzer::analyze(&declarations)?;
	Ok(linter::lint(&declarations))
}

#[test]
fn trigger_lint_for_loop_in_branch() -> Result<(), anyhow::Error>
{
	let analysis_result = lint("examples/loop_in_branch.pn")?;
	let mut lints = analysis_result.iter();
	match lints.next()
	{
		Some(Lint::LoopAsFirstStatement { .. }) => (),
		Some(lint) => Err(anyhow!("unexpected {:?}", lint))?,
		None => Err(anyhow!("broken test"))?,
	}
	match lints.next()
	{
		Some(lint) => Err(anyhow!("unexpected {:?}", lint))?,
		None => Ok(()),
	}
}

#[test]
fn trigger_multiple_lints() -> Result<(), anyhow::Error>
{
	let analysis_result = lint("tests/samples/valid/multiple_lints.pn")?;
	let mut lints = analysis_result.iter();
	match lints.next()
	{
		Some(Lint::LoopAsFirstStatement { .. }) => (),
		Some(lint) => Err(anyhow!("unexpected {:?}", lint))?,
		None => Err(anyhow!("broken test"))?,
	}
	match lints.next()
	{
		Some(Lint::LoopAsFirstStatement { .. }) => (),
		Some(lint) => Err(anyhow!("unexpected {:?}", lint))?,
		None => Err(anyhow!("broken test"))?,
	}
	match lints.next()
	{
		Some(lint) => Err(anyhow!("unexpected {:?}", lint))?,
		None => Ok(()),
	}
}
