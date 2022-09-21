/**/

pub mod analyzer;
pub mod common;
pub mod generator;
pub mod lexer;
pub mod linter;
pub mod parser;
pub mod rebuilder;
pub mod scoper;
pub mod typer;

#[cfg(test)]
mod tests
{
	use super::*;
	use anyhow::anyhow;
	use pretty_assertions::assert_eq;
	use std::io::Write;

	#[test]
	fn parse_do_nothing() -> Result<(), anyhow::Error>
	{
		let filename = "src/samples/do_nothing.pn";
		let source = std::fs::read_to_string(&filename)?;
		let tokens = lexer::lex(&source, filename);
		let declarations = parser::parse(tokens)?;
		assert_eq!(declarations.len(), 1);
		Ok(())
	}

	fn parse(
		filename: &str,
	) -> Result<Result<Vec<common::Declaration>, anyhow::Error>, anyhow::Error>
	{
		let source = std::fs::read_to_string(filename)?;
		let tokens = lexer::lex(&source, filename);
		Ok(parser::parse(tokens))
	}

	#[test]
	fn fail_to_parse_invalid_character() -> Result<(), anyhow::Error>
	{
		let parse_result = parse("src/samples/invalid_character.pn")?;
		match parse_result
		{
			Ok(_) => Err(anyhow!("broken test")),
			Err(_) => Ok(()),
		}
	}

	#[test]
	fn fail_to_parse_integer_too_big() -> Result<(), anyhow::Error>
	{
		let parse_result = parse("src/samples/integer_too_big.pn")?;
		match parse_result
		{
			Ok(_) => Err(anyhow!("broken test")),
			Err(_) => Ok(()),
		}
	}

	#[test]
	fn fail_to_parse_duplicate_return() -> Result<(), anyhow::Error>
	{
		let parse_result = parse("src/samples/duplicate_return.pn")?;
		match parse_result
		{
			Ok(_) => Err(anyhow!("broken test")),
			Err(_) => Ok(()),
		}
	}

	fn do_scope(
		filename: &str,
	) -> Result<Result<Vec<common::Declaration>, anyhow::Error>, anyhow::Error>
	{
		let source = std::fs::read_to_string(filename)?;
		let tokens = lexer::lex(&source, filename);
		let declarations = parser::parse(tokens)?;
		Ok(scoper::analyze(declarations))
	}

	fn do_type(
		filename: &str,
	) -> Result<Result<Vec<common::Declaration>, anyhow::Error>, anyhow::Error>
	{
		let source = std::fs::read_to_string(filename)?;
		let tokens = lexer::lex(&source, filename);
		let declarations = parser::parse(tokens)?;
		let declarations = scoper::analyze(declarations)?;
		Ok(typer::analyze(declarations))
	}

	#[test]
	fn allow_differing_local_variable_types() -> Result<(), anyhow::Error>
	{
		let analysis_result = do_type("src/samples/local_variable_types.pn")?;
		match analysis_result
		{
			Ok(_) => Ok(()),
			Err(error) => Err(error),
		}
	}

	#[test]
	fn fail_to_type_return_type_mismatch() -> Result<(), anyhow::Error>
	{
		let analysis_result = do_type("src/samples/return_type_mismatch.pn")?;
		match analysis_result
		{
			Ok(_) => Err(anyhow!("broken test")),
			Err(_) => Ok(()),
		}
	}

	#[test]
	fn fail_to_type_mismatched_assign() -> Result<(), anyhow::Error>
	{
		let analysis_result = do_type("src/samples/mismatched_assign.pn")?;
		match analysis_result
		{
			Ok(_) => Err(anyhow!("broken test")),
			Err(_) => Ok(()),
		}
	}

	#[test]
	fn fail_to_type_mismatched_array_elements() -> Result<(), anyhow::Error>
	{
		let analysis_result =
			do_type("src/samples/mismatched_array_elements.pn")?;
		match analysis_result
		{
			Ok(_) => Err(anyhow!("broken test")),
			Err(_) => Ok(()),
		}
	}

	#[test]
	fn fail_to_type_mismatched_array_type() -> Result<(), anyhow::Error>
	{
		let analysis_result = do_type("src/samples/mismatched_array_type.pn")?;
		match analysis_result
		{
			Ok(_) => Err(anyhow!("broken test")),
			Err(_) => Ok(()),
		}
	}

	#[test]
	fn fail_to_scope_duplicate_label() -> Result<(), anyhow::Error>
	{
		let analysis_result = do_scope("src/samples/duplicate_label.pn")?;
		match analysis_result
		{
			Ok(_) => Err(anyhow!("broken test")),
			Err(_) => Ok(()),
		}
	}

	#[test]
	fn fail_to_scope_foobar_variable() -> Result<(), anyhow::Error>
	{
		let analysis_result = do_scope("src/samples/foobar_variable.pn")?;
		match analysis_result
		{
			Ok(_) => Err(anyhow!("broken test")),
			Err(_) => Ok(()),
		}
	}

	#[test]
	fn fail_to_scope_illegal_jump_back() -> Result<(), anyhow::Error>
	{
		let analysis_result = do_scope("src/samples/illegal_jump_back.pn")?;
		match analysis_result
		{
			Ok(_) => Err(anyhow!("broken test")),
			Err(_) => Ok(()),
		}
	}

	#[test]
	fn fail_to_scope_label_in_else() -> Result<(), anyhow::Error>
	{
		let analysis_result = do_scope("src/samples/label_in_else.pn")?;
		match analysis_result
		{
			Ok(_) => Err(anyhow!("broken test")),
			Err(_) => Ok(()),
		}
	}

	fn analyze(
		filename: &str,
	) -> Result<Result<(), anyhow::Error>, anyhow::Error>
	{
		let source = std::fs::read_to_string(filename)?;
		let tokens = lexer::lex(&source, filename);
		let declarations = parser::parse(tokens)?;
		let declarations = scoper::analyze(declarations)?;
		let declarations = typer::analyze(declarations)?;
		Ok(analyzer::analyze(&declarations))
	}

	#[test]
	fn fail_to_analyze_loop_in_function() -> Result<(), anyhow::Error>
	{
		let analysis_result = analyze("src/samples/loop_in_function.pn")?;
		match analysis_result
		{
			Ok(_) => Err(anyhow!("broken test")),
			Err(_) => Ok(()),
		}
	}

	#[test]
	fn fail_to_analyze_loop_in_naked_branch() -> Result<(), anyhow::Error>
	{
		let analysis_result = analyze("src/samples/loop_in_naked_branch.pn")?;
		match analysis_result
		{
			Ok(_) => Err(anyhow!("broken test")),
			Err(_) => Ok(()),
		}
	}

	#[test]
	fn fail_to_analyze_loop_nonfinal() -> Result<(), anyhow::Error>
	{
		let analysis_result = analyze("src/samples/loop_nonfinal.pn")?;
		match analysis_result
		{
			Ok(_) => Err(anyhow!("broken test")),
			Err(_) => Ok(()),
		}
	}

	#[test]
	fn fail_to_scope_missing_label() -> Result<(), anyhow::Error>
	{
		let analysis_result = do_scope("src/samples/missing_label.pn")?;
		match analysis_result
		{
			Ok(_) => Err(anyhow!("broken test")),
			Err(_) => Ok(()),
		}
	}

	#[test]
	fn fail_to_scope_missing_variable() -> Result<(), anyhow::Error>
	{
		let analysis_result = do_scope("src/samples/missing_variable.pn")?;
		match analysis_result
		{
			Ok(_) => Err(anyhow!("broken test")),
			Err(_) => Ok(()),
		}
	}

	#[test]
	fn fail_to_scope_missing_function() -> Result<(), anyhow::Error>
	{
		let analysis_result = do_scope("src/samples/missing_function.pn")?;
		match analysis_result
		{
			Ok(_) => Err(anyhow!("broken test")),
			Err(_) => Ok(()),
		}
	}

	#[test]
	fn fail_to_scope_outscoped_variable() -> Result<(), anyhow::Error>
	{
		let analysis_result = do_scope("src/samples/outscoped_variable.pn")?;
		match analysis_result
		{
			Ok(_) => Err(anyhow!("broken test")),
			Err(_) => Ok(()),
		}
	}

	#[test]
	fn fail_to_analyze_argument_type_mismatch() -> Result<(), anyhow::Error>
	{
		let analysis_result = analyze("src/samples/argument_type_mismatch.pn")?;
		match analysis_result
		{
			Ok(_) => Err(anyhow!("broken test")),
			Err(_) => Ok(()),
		}
	}

	#[test]
	fn fail_to_analyze_too_few_arguments() -> Result<(), anyhow::Error>
	{
		let analysis_result = analyze("src/samples/too_few_arguments.pn")?;
		match analysis_result
		{
			Ok(_) => Err(anyhow!("broken test")),
			Err(_) => Ok(()),
		}
	}

	#[test]
	fn fail_to_analyze_too_many_arguments() -> Result<(), anyhow::Error>
	{
		let analysis_result = analyze("src/samples/too_many_arguments.pn")?;
		match analysis_result
		{
			Ok(_) => Err(anyhow!("broken test")),
			Err(_) => Ok(()),
		}
	}

	#[test]
	fn fail_to_analyze_assign_array_to_array() -> Result<(), anyhow::Error>
	{
		let analysis_result = analyze("src/samples/assign_array_to_array.pn")?;
		match analysis_result
		{
			Ok(_) => Err(anyhow!("broken test")),
			Err(_) => Ok(()),
		}
	}

	#[test]
	fn fail_to_analyze_skip_declaration() -> Result<(), anyhow::Error>
	{
		let analysis_result = analyze("src/samples/skip_declaration.pn")?;
		match analysis_result
		{
			Ok(_) => Err(anyhow!("broken test")),
			Err(_) => Ok(()),
		}
	}

	#[test]
	fn fail_to_analyze_conditional_declaration() -> Result<(), anyhow::Error>
	{
		let analysis_result =
			analyze("src/samples/conditional_declaration.pn")?;
		match analysis_result
		{
			Ok(_) => Err(anyhow!("broken test")),
			Err(_) => Ok(()),
		}
	}

	#[test]
	fn fail_to_analyze_var_in_naked_branch() -> Result<(), anyhow::Error>
	{
		let analysis_result = analyze("src/samples/var_in_naked_branch.pn")?;
		match analysis_result
		{
			Ok(_) => Err(anyhow!("broken test")),
			Err(_) => Ok(()),
		}
	}

	#[test]
	fn fail_to_analyze_missing_address() -> Result<(), anyhow::Error>
	{
		let analysis_result = analyze("src/samples/missing_address.pn")?;
		match analysis_result
		{
			Ok(_) => Err(anyhow!("broken test")),
			Err(_) => Ok(()),
		}
	}

	#[test]
	fn fail_to_analyze_pointer_to_temporary_pointer(
	) -> Result<(), anyhow::Error>
	{
		let analysis_result =
			analyze("src/samples/pointer_to_temporary_pointer.pn")?;
		match analysis_result
		{
			Ok(_) => Err(anyhow!("broken test")),
			Err(_) => Ok(()),
		}
	}

	#[test]
	fn fail_to_analyze_pointer_to_parameter() -> Result<(), anyhow::Error>
	{
		let analysis_result = analyze("src/samples/pointer_to_parameter.pn")?;
		match analysis_result
		{
			Ok(_) => Err(anyhow!("broken test")),
			Err(_) => Ok(()),
		}
	}

	#[test]
	fn fail_to_analyze_pointer_to_const() -> Result<(), anyhow::Error>
	{
		let analysis_result = analyze("src/samples/pointer_to_const.pn")?;
		match analysis_result
		{
			Ok(_) => Err(anyhow!("broken test")),
			Err(_) => Ok(()),
		}
	}

	#[test]
	fn fail_to_analyze_assign_to_view() -> Result<(), anyhow::Error>
	{
		let analysis_result = analyze("src/samples/assign_to_view.pn")?;
		match analysis_result
		{
			Ok(_) => Err(anyhow!("broken test")),
			Err(_) => Ok(()),
		}
	}

	#[test]
	fn fail_to_analyze_change_view_address() -> Result<(), anyhow::Error>
	{
		let analysis_result = analyze("src/samples/change_view_address.pn")?;
		match analysis_result
		{
			Ok(_) => Err(anyhow!("broken test")),
			Err(_) => Ok(()),
		}
	}

	#[test]
	fn fail_to_analyze_assign_to_array_slice() -> Result<(), anyhow::Error>
	{
		let analysis_result = analyze("src/samples/assign_to_array_slice.pn")?;
		match analysis_result
		{
			Ok(_) => Err(anyhow!("broken test")),
			Err(_) => Ok(()),
		}
	}

	#[test]
	fn fail_to_analyze_assign_to_pointer_parameter() -> Result<(), anyhow::Error>
	{
		let analysis_result =
			analyze("src/samples/assign_to_pointer_parameter.pn")?;
		match analysis_result
		{
			Ok(_) => Err(anyhow!("broken test")),
			Err(_) => Ok(()),
		}
	}

	#[test]
	fn rebuild_foo_bar() -> Result<(), anyhow::Error>
	{
		let filename = "src/samples/foo_bar.pn";
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

	#[test]
	fn execute_foo_bar() -> Result<(), anyhow::Error>
	{
		let result = execute_calculation("src/samples/foo_bar.pn")?;
		assert_eq!(result, 34);
		Ok(())
	}

	#[test]
	fn execute_five() -> Result<(), anyhow::Error>
	{
		let result = execute_calculation("src/samples/five.pn")?;
		assert_eq!(result, 5);
		Ok(())
	}

	#[test]
	fn execute_two_plus_seven() -> Result<(), anyhow::Error>
	{
		let result = execute_calculation("src/samples/two_plus_seven.pn")?;
		assert_eq!(result, 9);
		Ok(())
	}

	#[test]
	fn execute_addition() -> Result<(), anyhow::Error>
	{
		let result = execute_calculation("src/samples/addition.pn")?;
		assert_eq!(result, 10);
		Ok(())
	}

	#[test]
	fn execute_scoped_variables() -> Result<(), anyhow::Error>
	{
		let result = execute_calculation("src/samples/scoped_variables.pn")?;
		assert_eq!(result, 200);
		Ok(())
	}

	#[test]
	fn execute_scoped_labels() -> Result<(), anyhow::Error>
	{
		let result = execute_calculation("src/samples/scoped_labels.pn")?;
		assert_eq!(result, 200);
		Ok(())
	}

	#[test]
	fn execute_forward_declare_function() -> Result<(), anyhow::Error>
	{
		let result =
			execute_calculation("src/samples/forward_declare_function.pn")?;
		assert_eq!(result, 200);
		Ok(())
	}

	#[test]
	fn execute_void_function() -> Result<(), anyhow::Error>
	{
		let result = execute_calculation("src/samples/void_function.pn")?;
		assert_eq!(result, 200);
		Ok(())
	}

	#[test]
	fn execute_array_by_reference() -> Result<(), anyhow::Error>
	{
		let result = execute_calculation("src/samples/array_by_reference.pn")?;
		assert_eq!(result, 200);
		Ok(())
	}

	#[test]
	fn execute_array_reference_by_reference() -> Result<(), anyhow::Error>
	{
		let result =
			execute_calculation("src/samples/array_reference_by_reference.pn")?;
		assert_eq!(result, 200);
		Ok(())
	}

	#[test]
	fn execute_mutable_array() -> Result<(), anyhow::Error>
	{
		let result = execute_calculation("src/samples/mutable_array.pn")?;
		assert_eq!(result, 200);
		Ok(())
	}

	#[test]
	fn execute_multidimensional_array() -> Result<(), anyhow::Error>
	{
		let result =
			execute_calculation("src/samples/multidimensional_array.pn")?;
		assert_eq!(result, 200);
		Ok(())
	}

	#[test]
	fn execute_pointers() -> Result<(), anyhow::Error>
	{
		let result = execute_calculation("src/samples/pointers.pn")?;
		assert_eq!(result, 200);
		Ok(())
	}

	#[test]
	fn execute_pointer_to_mut() -> Result<(), anyhow::Error>
	{
		let result = execute_calculation("src/samples/pointer_to_mut.pn")?;
		assert_eq!(result, 200);
		Ok(())
	}

	#[test]
	fn execute_mutable_pointer() -> Result<(), anyhow::Error>
	{
		let result = execute_calculation("src/samples/mutable_pointer.pn")?;
		assert_eq!(result, 200);
		Ok(())
	}

	#[test]
	fn execute_pointer_to_pointer() -> Result<(), anyhow::Error>
	{
		let result = execute_calculation("src/samples/pointer_to_pointer.pn")?;
		assert_eq!(result, 200);
		Ok(())
	}

	fn execute_calculation(filename: &str) -> Result<i32, anyhow::Error>
	{
		let source = std::fs::read_to_string(&filename)?;
		let tokens = lexer::lex(&source, filename);
		let declarations = parser::parse(tokens)?;
		let declarations = scoper::analyze(declarations)?;
		let declarations = typer::analyze(declarations)?;
		analyzer::analyze(&declarations)?;
		let ir = generator::generate(&declarations, filename)?;
		let mut cmd = std::process::Command::new("lli")
			.stdin(std::process::Stdio::piped())
			.spawn()?;
		cmd.stdin.as_mut().unwrap().write_all(ir.as_bytes())?;
		let status = cmd.wait()?;
		let exitcode = status.code().unwrap();
		Ok(exitcode)
	}
}
