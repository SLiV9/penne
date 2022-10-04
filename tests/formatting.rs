//
// Part of penne
// Copyright (c) 2020 Sander in 't Veld
// License: MIT
//

use penne::lexer;
use penne::parser;
use penne::rebuilder;

use pretty_assertions::assert_eq;

#[test]
fn rebuild_goto_end() -> Result<(), anyhow::Error>
{
	let filename = "examples/goto_end.pn";
	let source = std::fs::read_to_string(&filename)?;
	let tokens = lexer::lex(&source, filename);
	let declarations = parser::parse(tokens)?;
	let indentation = rebuilder::Indentation {
		value: "\t",
		amount: 0,
	};
	let code = rebuilder::rebuild(&declarations, &indentation)?;
	let code_lines: Vec<&str> = code.lines().collect();
	let source_lines: Vec<&str> = source.lines().collect();
	assert_eq!(code_lines, source_lines);
	Ok(())
}
