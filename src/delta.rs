//
// Part of penne
// Copyright (c) 2026 Sander in 't Veld
// License: MIT
//

//! The second generation compiler for the Penne programming language.

#![expect(unused)]

pub mod lexer;

pub mod test_suite
{
	use super::*;

	use crate::execution_test_tools::execute_ir;

	pub use crate::execution_test_tools::calculation_result_from_output;
	pub use crate::execution_test_tools::stdout_from_output;

	pub fn allow_to_compile(filename: &str)
	{
		todo!()
	}

	pub fn compile_to_fail(codes: &[u16], filename: &str)
	{
		todo!()
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
