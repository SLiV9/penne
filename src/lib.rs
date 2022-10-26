//
// Part of penne
// Copyright (c) 2020 Sander in 't Veld
// License: MIT
//

//! The compiler for the Penne programming language.
//!
//! This is the internal documentation for the Penne compiler.
//! General information about the Penne programming language,
//! its syntax and semantics
//! is found in the [README](https://github.com/SLiV9/penne#readme).
//! Usage instructions for the command line interface can be found
//! by running `penne help` from the command line.
//!
//! The Abstract Syntax Tree is detailed in the modules [common], [value_type]
//! and [resolved]. Errors are laid out in [error]. The other modules contain
//! the various compiler stages. In order:
//! [lexer], [parser], [scoper], [typer], [analyzer], [linter], [resolver],
//! and [generator].
//! The [rebuilder] module allows turning the AST back into (annotated) code.

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

/// Convience method that parses source code and runs it through each of the
/// compiler stages.
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
