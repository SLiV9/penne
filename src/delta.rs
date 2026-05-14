//
// Part of penne
// Copyright (c) 2026 Sander in 't Veld
// License: MIT
//

//! The second generation compiler for the Penne programming language.

pub mod fuzzer;
pub mod lexer;
pub mod scanner;

pub mod test_suite
{
	#![expect(unused)]

	use super::*;

	use crate::alpha::error::Errors;
	use crate::alpha::error::Location;

	use crate::execution_test_tools::execute_ir;

	pub use crate::execution_test_tools::calculation_result_from_output;
	pub use crate::execution_test_tools::stdout_from_output;

	fn compile(filenames: &[&str]) -> Errors
	{
		let mut all_errors = Errors { errors: Vec::new() };
		for filename in filenames
		{
			let source = std::fs::read_to_string(&filename).unwrap();
			let tokens = lexer::lex(source.as_bytes(), filename);
			if let Some(errors) = tokens.errors()
			{
				all_errors = all_errors.combined_with(errors);
				continue;
			}
		}
		all_errors.sorted()
	}

	pub fn allow_to_compile(filename: &str)
	{
		let result = compile(&[filename]);
		if !result.errors.is_empty()
		{
			result.panic();
		}
	}

	pub fn compile_to_fail(codes: &[u16], filename: &str)
	{
		let result = compile(&[filename]);
		if result.errors.is_empty()
		{
			panic!("broken test");
		}
		let errors = result;
		assert_eq!(errors.codes(), codes, "unexpected {:?}", errors);
	}

	pub fn lint_to_fail(codes: &[u16], filename: &str)
	{
		todo!()
	}

	pub fn lint_to_nothing(filename: &str)
	{
		todo!()
	}

	pub fn assert_formatted_correctly(
		filename: &str,
	) -> Result<(), anyhow::Error>
	{
		todo!()
	}

	pub fn execute(
		filename: &str,
	) -> Result<std::process::Output, anyhow::Error>
	{
		todo!()
	}

	pub fn execute_with_imports(
		filenames: &[&str],
	) -> Result<std::process::Output, anyhow::Error>
	{
		todo!()
	}
}
