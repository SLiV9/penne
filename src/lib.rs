//
// Part of penne
// Copyright (c) 2020 Sander in 't Veld
// License: MIT
//

pub mod analyzer;
pub mod common;
pub mod error;
pub mod generator;
pub mod lexer;
pub mod linter;
pub mod parser;
pub mod rebuilder;
pub mod resolved;
pub mod resolver;
pub mod scoper;
pub mod typer;
pub mod value_type;

pub use error::Error;
pub use error::Errors;
pub use resolved::Declaration;

pub fn compile_source(
	source: &str,
	filename: &str,
) -> Result<Vec<Declaration>, Errors>
{
	let tokens = lexer::lex(source, filename);
	let declarations = parser::parse(tokens);
	let declarations = scoper::analyze(declarations);
	let declarations = typer::analyze(declarations);
	let declarations = analyzer::analyze(declarations);
	resolver::resolve(declarations)
}
