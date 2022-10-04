//
// Part of penne
// Copyright (c) 2020 Sander in 't Veld
// License: MIT
//

pub mod analyzer;
pub mod common;
pub mod generator;
pub mod lexer;
pub mod linter;
pub mod parser;
pub mod rebuilder;
pub mod resolver;
pub mod scoper;
pub mod typer;

#[cfg(test)]
mod tests
{
	use super::*;
	use anyhow::anyhow;
	use pretty_assertions::assert_eq;
	use std::io::Write;

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
		let parse_result = parse("tests/samples/invalid/invalid_character.pn")?;
		match parse_result
		{
			Ok(_) => Err(anyhow!("broken test")),
			Err(_) => Ok(()),
		}
	}

	#[test]
	fn fail_to_parse_invalid_escape() -> Result<(), anyhow::Error>
	{
		let parse_result = parse("tests/samples/invalid/invalid_escape.pn")?;
		match parse_result
		{
			Ok(_) => Err(anyhow!("broken test")),
			Err(_) => Ok(()),
		}
	}

	#[test]
	fn fail_to_parse_invalid_trailing_slash_in_string(
	) -> Result<(), anyhow::Error>
	{
		let parse_result =
			parse("tests/samples/invalid/invalid_trailing_slash_in_string.pn")?;
		match parse_result
		{
			Ok(_) => Err(anyhow!("broken test")),
			Err(_) => Ok(()),
		}
	}

	#[test]
	fn fail_to_parse_missing_closing_quote() -> Result<(), anyhow::Error>
	{
		let parse_result =
			parse("tests/samples/invalid/missing_closing_quote.pn")?;
		match parse_result
		{
			Ok(_) => Err(anyhow!("broken test")),
			Err(_) => Ok(()),
		}
	}

	#[test]
	fn fail_to_parse_integer_too_big() -> Result<(), anyhow::Error>
	{
		let parse_result = parse("tests/samples/invalid/integer_too_big.pn")?;
		match parse_result
		{
			Ok(_) => Err(anyhow!("broken test")),
			Err(_) => Ok(()),
		}
	}

	#[test]
	fn fail_to_parse_duplicate_return() -> Result<(), anyhow::Error>
	{
		let parse_result = parse("tests/samples/invalid/duplicate_return.pn")?;
		match parse_result
		{
			Ok(_) => Err(anyhow!("broken test")),
			Err(_) => Ok(()),
		}
	}

	#[test]
	fn fail_to_parse_empty_return() -> Result<(), anyhow::Error>
	{
		let parse_result = parse("tests/samples/invalid/empty_return.pn")?;
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

	fn resolve(
		filename: &str,
	) -> Result<Result<Vec<common::Declaration>, anyhow::Error>, anyhow::Error>
	{
		let source = std::fs::read_to_string(filename)?;
		let tokens = lexer::lex(&source, filename);
		let declarations = parser::parse(tokens)?;
		let declarations = scoper::analyze(declarations)?;
		let declarations = typer::analyze(declarations)?;
		Ok(resolver::analyze(declarations))
	}

	#[test]
	fn allow_differing_local_variable_types() -> Result<(), anyhow::Error>
	{
		let analysis_result =
			do_type("tests/samples/valid/local_variable_types.pn")?;
		match analysis_result
		{
			Ok(_) => Ok(()),
			Err(error) => Err(error),
		}
	}

	#[test]
	fn fail_to_type_return_type_mismatch() -> Result<(), anyhow::Error>
	{
		let analysis_result =
			do_type("tests/samples/invalid/return_type_mismatch.pn")?;
		match analysis_result
		{
			Ok(_) => Err(anyhow!("broken test")),
			Err(_) => Ok(()),
		}
	}

	#[test]
	fn fail_to_type_missing_return() -> Result<(), anyhow::Error>
	{
		let analysis_result =
			do_type("tests/samples/invalid/missing_return.pn")?;
		match analysis_result
		{
			Ok(_) => Err(anyhow!("broken test")),
			Err(_) => Ok(()),
		}
	}

	#[test]
	fn fail_to_type_mismatched_assign() -> Result<(), anyhow::Error>
	{
		let analysis_result =
			do_type("tests/samples/invalid/mismatched_assign.pn")?;
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
			do_type("tests/samples/invalid/mismatched_array_elements.pn")?;
		match analysis_result
		{
			Ok(_) => Err(anyhow!("broken test")),
			Err(_) => Ok(()),
		}
	}

	#[test]
	fn fail_to_type_mismatched_array_type() -> Result<(), anyhow::Error>
	{
		let analysis_result =
			do_type("tests/samples/invalid/mismatched_array_type.pn")?;
		match analysis_result
		{
			Ok(_) => Err(anyhow!("broken test")),
			Err(_) => Ok(()),
		}
	}

	#[test]
	fn fail_to_type_length_of_int() -> Result<(), anyhow::Error>
	{
		let analysis_result =
			do_type("tests/samples/invalid/length_of_int.pn")?;
		match analysis_result
		{
			Ok(_) => Err(anyhow!("broken test")),
			Err(_) => Ok(()),
		}
	}

	#[test]
	fn fail_to_resolve_bitshift_without_types() -> Result<(), anyhow::Error>
	{
		let analysis_result =
			resolve("tests/samples/invalid/bitshift_without_types.pn")?;
		match analysis_result
		{
			Ok(_) => Err(anyhow!("broken test")),
			Err(_) => Ok(()),
		}
	}

	#[test]
	fn fail_to_resolve_bitshift_type_mismatch() -> Result<(), anyhow::Error>
	{
		let analysis_result =
			resolve("tests/samples/invalid/bitshift_type_mismatch.pn")?;
		match analysis_result
		{
			Ok(_) => Err(anyhow!("broken test")),
			Err(_) => Ok(()),
		}
	}

	#[test]
	fn fail_to_resolve_bitshift_non_integer() -> Result<(), anyhow::Error>
	{
		let analysis_result =
			resolve("tests/samples/invalid/bitshift_non_integer.pn")?;
		match analysis_result
		{
			Ok(_) => Err(anyhow!("broken test")),
			Err(_) => Ok(()),
		}
	}

	#[test]
	fn fail_to_resolve_negative_u32() -> Result<(), anyhow::Error>
	{
		let analysis_result = resolve("tests/samples/invalid/negative_u32.pn")?;
		match analysis_result
		{
			Ok(_) => Err(anyhow!("broken test")),
			Err(_) => Ok(()),
		}
	}

	#[test]
	fn fail_to_resolve_bitwise_not_usize() -> Result<(), anyhow::Error>
	{
		let analysis_result =
			resolve("tests/samples/invalid/bitwise_not_usize.pn")?;
		match analysis_result
		{
			Ok(_) => Err(anyhow!("broken test")),
			Err(_) => Ok(()),
		}
	}

	#[test]
	fn fail_to_resolve_comparison_on_arrays() -> Result<(), anyhow::Error>
	{
		let analysis_result =
			resolve("tests/samples/invalid/comparison_on_arrays.pn")?;
		match analysis_result
		{
			Ok(_) => Err(anyhow!("broken test")),
			Err(_) => Ok(()),
		}
	}

	#[test]
	fn fail_to_resolve_comparison_ge_pointer() -> Result<(), anyhow::Error>
	{
		let analysis_result =
			resolve("tests/samples/invalid/comparison_ge_pointer.pn")?;
		match analysis_result
		{
			Ok(_) => Err(anyhow!("broken test")),
			Err(_) => Ok(()),
		}
	}

	#[test]
	fn fail_to_scope_duplicate_label() -> Result<(), anyhow::Error>
	{
		let analysis_result =
			do_scope("tests/samples/invalid/duplicate_label.pn")?;
		match analysis_result
		{
			Ok(_) => Err(anyhow!("broken test")),
			Err(_) => Ok(()),
		}
	}

	#[test]
	fn fail_to_scope_misplaced_variable() -> Result<(), anyhow::Error>
	{
		let analysis_result =
			do_scope("tests/samples/invalid/misplaced_variable.pn")?;
		match analysis_result
		{
			Ok(_) => Err(anyhow!("broken test")),
			Err(_) => Ok(()),
		}
	}

	#[test]
	fn fail_to_scope_foobar_variable() -> Result<(), anyhow::Error>
	{
		let analysis_result =
			do_scope("tests/samples/invalid/foobar_variable.pn")?;
		match analysis_result
		{
			Ok(_) => Err(anyhow!("broken test")),
			Err(_) => Ok(()),
		}
	}

	#[test]
	fn fail_to_scope_illegal_jump_back() -> Result<(), anyhow::Error>
	{
		let analysis_result =
			do_scope("tests/samples/invalid/illegal_jump_back.pn")?;
		match analysis_result
		{
			Ok(_) => Err(anyhow!("broken test")),
			Err(_) => Ok(()),
		}
	}

	#[test]
	fn fail_to_scope_label_in_else() -> Result<(), anyhow::Error>
	{
		let analysis_result =
			do_scope("tests/samples/invalid/label_in_else.pn")?;
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
	fn fail_to_analyze_nested_naked_if() -> Result<(), anyhow::Error>
	{
		let analysis_result =
			analyze("tests/samples/invalid/nested_naked_if.pn")?;
		match analysis_result
		{
			Ok(_) => Err(anyhow!("broken test")),
			Err(_) => Ok(()),
		}
	}

	#[test]
	fn fail_to_analyze_loop_in_function() -> Result<(), anyhow::Error>
	{
		let analysis_result =
			analyze("tests/samples/invalid/loop_in_function.pn")?;
		match analysis_result
		{
			Ok(_) => Err(anyhow!("broken test")),
			Err(_) => Ok(()),
		}
	}

	#[test]
	fn fail_to_analyze_loop_in_naked_branch() -> Result<(), anyhow::Error>
	{
		let analysis_result =
			analyze("tests/samples/invalid/loop_in_naked_branch.pn")?;
		match analysis_result
		{
			Ok(_) => Err(anyhow!("broken test")),
			Err(_) => Ok(()),
		}
	}

	#[test]
	fn fail_to_analyze_loop_nonfinal() -> Result<(), anyhow::Error>
	{
		let analysis_result =
			analyze("tests/samples/invalid/loop_nonfinal.pn")?;
		match analysis_result
		{
			Ok(_) => Err(anyhow!("broken test")),
			Err(_) => Ok(()),
		}
	}

	#[test]
	fn fail_to_scope_missing_label() -> Result<(), anyhow::Error>
	{
		let analysis_result =
			do_scope("tests/samples/invalid/missing_label.pn")?;
		match analysis_result
		{
			Ok(_) => Err(anyhow!("broken test")),
			Err(_) => Ok(()),
		}
	}

	#[test]
	fn fail_to_scope_missing_variable() -> Result<(), anyhow::Error>
	{
		let analysis_result =
			do_scope("tests/samples/invalid/missing_variable.pn")?;
		match analysis_result
		{
			Ok(_) => Err(anyhow!("broken test")),
			Err(_) => Ok(()),
		}
	}

	#[test]
	fn fail_to_scope_missing_function() -> Result<(), anyhow::Error>
	{
		let analysis_result =
			do_scope("tests/samples/invalid/missing_function.pn")?;
		match analysis_result
		{
			Ok(_) => Err(anyhow!("broken test")),
			Err(_) => Ok(()),
		}
	}

	#[test]
	fn fail_to_scope_outscoped_variable() -> Result<(), anyhow::Error>
	{
		let analysis_result =
			do_scope("tests/samples/invalid/outscoped_variable.pn")?;
		match analysis_result
		{
			Ok(_) => Err(anyhow!("broken test")),
			Err(_) => Ok(()),
		}
	}

	#[test]
	fn fail_to_analyze_argument_type_mismatch() -> Result<(), anyhow::Error>
	{
		let analysis_result =
			analyze("tests/samples/invalid/argument_type_mismatch.pn")?;
		match analysis_result
		{
			Ok(_) => Err(anyhow!("broken test")),
			Err(_) => Ok(()),
		}
	}

	#[test]
	fn fail_to_analyze_too_few_arguments() -> Result<(), anyhow::Error>
	{
		let analysis_result =
			analyze("tests/samples/invalid/too_few_arguments.pn")?;
		match analysis_result
		{
			Ok(_) => Err(anyhow!("broken test")),
			Err(_) => Ok(()),
		}
	}

	#[test]
	fn fail_to_analyze_too_many_arguments() -> Result<(), anyhow::Error>
	{
		let analysis_result =
			analyze("tests/samples/invalid/too_many_arguments.pn")?;
		match analysis_result
		{
			Ok(_) => Err(anyhow!("broken test")),
			Err(_) => Ok(()),
		}
	}

	#[test]
	fn fail_to_analyze_assign_array_to_array() -> Result<(), anyhow::Error>
	{
		let analysis_result =
			analyze("tests/samples/invalid/assign_array_to_array.pn")?;
		match analysis_result
		{
			Ok(_) => Err(anyhow!("broken test")),
			Err(_) => Ok(()),
		}
	}

	#[test]
	fn fail_to_analyze_skip_declaration() -> Result<(), anyhow::Error>
	{
		let analysis_result =
			analyze("tests/samples/invalid/skip_declaration.pn")?;
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
			analyze("tests/samples/invalid/conditional_declaration.pn")?;
		match analysis_result
		{
			Ok(_) => Err(anyhow!("broken test")),
			Err(_) => Ok(()),
		}
	}

	#[test]
	fn fail_to_analyze_var_in_naked_branch() -> Result<(), anyhow::Error>
	{
		let analysis_result =
			analyze("tests/samples/invalid/var_in_naked_branch.pn")?;
		match analysis_result
		{
			Ok(_) => Err(anyhow!("broken test")),
			Err(_) => Ok(()),
		}
	}

	#[test]
	fn fail_to_analyze_missing_address() -> Result<(), anyhow::Error>
	{
		let analysis_result =
			analyze("tests/samples/invalid/missing_address.pn")?;
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
			analyze("tests/samples/invalid/pointer_to_temporary_pointer.pn")?;
		match analysis_result
		{
			Ok(_) => Err(anyhow!("broken test")),
			Err(_) => Ok(()),
		}
	}

	#[test]
	fn fail_to_analyze_pointer_to_parameter() -> Result<(), anyhow::Error>
	{
		let analysis_result =
			analyze("tests/samples/invalid/pointer_to_parameter.pn")?;
		match analysis_result
		{
			Ok(_) => Err(anyhow!("broken test")),
			Err(_) => Ok(()),
		}
	}

	#[test]
	fn fail_to_analyze_pointer_to_const() -> Result<(), anyhow::Error>
	{
		let analysis_result =
			analyze("tests/samples/invalid/pointer_to_const.pn")?;
		match analysis_result
		{
			Ok(_) => Err(anyhow!("broken test")),
			Err(_) => Ok(()),
		}
	}

	#[test]
	fn fail_to_analyze_assign_to_view() -> Result<(), anyhow::Error>
	{
		let analysis_result =
			analyze("tests/samples/invalid/assign_to_view.pn")?;
		match analysis_result
		{
			Ok(_) => Err(anyhow!("broken test")),
			Err(_) => Ok(()),
		}
	}

	#[test]
	fn fail_to_analyze_assign_to_view_parameter() -> Result<(), anyhow::Error>
	{
		let analysis_result =
			analyze("tests/samples/invalid/assign_to_view_parameter.pn")?;
		match analysis_result
		{
			Ok(_) => Err(anyhow!("broken test")),
			Err(_) => Ok(()),
		}
	}

	#[test]
	fn fail_to_analyze_change_view_address() -> Result<(), anyhow::Error>
	{
		let analysis_result =
			analyze("tests/samples/invalid/change_view_address.pn")?;
		match analysis_result
		{
			Ok(_) => Err(anyhow!("broken test")),
			Err(_) => Ok(()),
		}
	}

	#[test]
	fn fail_to_analyze_null_view() -> Result<(), anyhow::Error>
	{
		let analysis_result = analyze("tests/samples/invalid/null_view.pn")?;
		match analysis_result
		{
			Ok(_) => Err(anyhow!("broken test")),
			Err(_) => Ok(()),
		}
	}

	#[test]
	fn fail_to_analyze_assign_to_constant() -> Result<(), anyhow::Error>
	{
		let analysis_result =
			analyze("tests/samples/invalid/assign_to_constant.pn")?;
		match analysis_result
		{
			Ok(_) => Err(anyhow!("broken test")),
			Err(_) => Ok(()),
		}
	}

	#[test]
	fn fail_to_analyze_assign_to_array_slice() -> Result<(), anyhow::Error>
	{
		let analysis_result =
			analyze("tests/samples/invalid/assign_to_array_slice.pn")?;
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
			analyze("tests/samples/invalid/assign_to_pointer_parameter.pn")?;
		match analysis_result
		{
			Ok(_) => Err(anyhow!("broken test")),
			Err(_) => Ok(()),
		}
	}

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

	#[test]
	fn execute_five() -> Result<(), anyhow::Error>
	{
		let result = execute_calculation("tests/samples/valid/five.pn")?;
		assert_eq!(result, 5);
		Ok(())
	}

	#[test]
	fn execute_two_plus_seven() -> Result<(), anyhow::Error>
	{
		let result =
			execute_calculation("tests/samples/valid/two_plus_seven.pn")?;
		assert_eq!(result, 9);
		Ok(())
	}

	#[test]
	fn execute_addition() -> Result<(), anyhow::Error>
	{
		let result = execute_calculation("examples/addition.pn")?;
		assert_eq!(result, 10);
		Ok(())
	}

	#[test]
	fn execute_bitwise_operations() -> Result<(), anyhow::Error>
	{
		let result = execute_calculation("examples/bitwise_operations.pn")?;
		assert_eq!(result, 200);
		Ok(())
	}

	#[test]
	fn execute_scoped_variables() -> Result<(), anyhow::Error>
	{
		let result = execute_calculation("examples/scoped_variables.pn")?;
		assert_eq!(result, 200);
		Ok(())
	}

	#[test]
	fn execute_scoped_labels() -> Result<(), anyhow::Error>
	{
		let result = execute_calculation("examples/scoped_labels.pn")?;
		assert_eq!(result, 200);
		Ok(())
	}

	#[test]
	fn execute_forward_declare_function() -> Result<(), anyhow::Error>
	{
		let result = execute_calculation(
			"tests/samples/valid/forward_declare_function.pn",
		)?;
		assert_eq!(result, 200);
		Ok(())
	}

	#[test]
	fn execute_void_function() -> Result<(), anyhow::Error>
	{
		let result =
			execute_calculation("tests/samples/valid/void_function.pn")?;
		assert_eq!(result, 200);
		Ok(())
	}

	#[test]
	fn execute_array_length() -> Result<(), anyhow::Error>
	{
		let result =
			execute_calculation("tests/samples/valid/array_length.pn")?;
		assert_eq!(result, 200);
		Ok(())
	}

	#[test]
	fn execute_length_of_slice() -> Result<(), anyhow::Error>
	{
		let result =
			execute_calculation("tests/samples/valid/length_of_slice.pn")?;
		assert_eq!(result, 200);
		Ok(())
	}

	#[test]
	fn execute_length_of_array_by_pointer() -> Result<(), anyhow::Error>
	{
		let result = execute_calculation(
			"tests/samples/valid/length_of_array_by_pointer.pn",
		)?;
		assert_eq!(result, 200);
		Ok(())
	}

	#[test]
	fn execute_array_by_reference() -> Result<(), anyhow::Error>
	{
		let result = execute_calculation("examples/array_by_reference.pn")?;
		assert_eq!(result, 200);
		Ok(())
	}

	#[test]
	fn execute_array_reference_by_reference() -> Result<(), anyhow::Error>
	{
		let result = execute_calculation(
			"tests/samples/valid/array_reference_by_reference.pn",
		)?;
		assert_eq!(result, 200);
		Ok(())
	}

	#[test]
	fn execute_mutable_array() -> Result<(), anyhow::Error>
	{
		let result =
			execute_calculation("tests/samples/valid/mutable_array.pn")?;
		assert_eq!(result, 200);
		Ok(())
	}

	#[test]
	fn execute_multidimensional_array() -> Result<(), anyhow::Error>
	{
		let result = execute_calculation(
			"tests/samples/valid/multidimensional_array.pn",
		)?;
		assert_eq!(result, 200);
		Ok(())
	}

	#[test]
	fn execute_pointers() -> Result<(), anyhow::Error>
	{
		let result = execute_calculation("examples/pointers.pn")?;
		assert_eq!(result, 200);
		Ok(())
	}

	#[test]
	fn execute_pointer_to_mut() -> Result<(), anyhow::Error>
	{
		let result =
			execute_calculation("tests/samples/valid/pointer_to_mut.pn")?;
		assert_eq!(result, 200);
		Ok(())
	}

	#[test]
	fn execute_mutable_pointer() -> Result<(), anyhow::Error>
	{
		let result =
			execute_calculation("tests/samples/valid/mutable_pointer.pn")?;
		assert_eq!(result, 200);
		Ok(())
	}

	#[test]
	fn execute_pointer_to_pointer() -> Result<(), anyhow::Error>
	{
		let result = execute_calculation("examples/pointer_to_pointer.pn")?;
		assert_eq!(result, 200);
		Ok(())
	}

	#[test]
	fn execute_pointer_to_array() -> Result<(), anyhow::Error>
	{
		let result =
			execute_calculation("tests/samples/valid/pointer_to_array.pn")?;
		assert_eq!(result, 200);
		Ok(())
	}

	#[test]
	fn execute_array_by_pointer() -> Result<(), anyhow::Error>
	{
		let result = execute_calculation("examples/array_by_pointer.pn")?;
		assert_eq!(result, 200);
		Ok(())
	}

	#[test]
	fn execute_array_in_extern() -> Result<(), anyhow::Error>
	{
		let result =
			execute_calculation("tests/samples/valid/array_in_extern.pn")?;
		assert_eq!(result, 200);
		Ok(())
	}

	#[test]
	fn execute_pointer_aliasing() -> Result<(), anyhow::Error>
	{
		let result =
			execute_calculation("tests/samples/valid/pointer_aliasing.pn")?;
		assert_eq!(result, 200);
		Ok(())
	}

	#[test]
	fn execute_array_aliasing() -> Result<(), anyhow::Error>
	{
		let result =
			execute_calculation("tests/samples/valid/array_aliasing.pn")?;
		assert_eq!(result, 200);
		Ok(())
	}

	#[test]
	fn execute_pointer_to_slice() -> Result<(), anyhow::Error>
	{
		let result =
			execute_calculation("tests/samples/valid/pointer_to_slice.pn")?;
		assert_eq!(result, 200);
		Ok(())
	}

	#[test]
	fn execute_bitshift_type_inference() -> Result<(), anyhow::Error>
	{
		let result = execute_calculation(
			"tests/samples/valid/bitshift_type_inference.pn",
		)?;
		assert_eq!(result, 200);
		Ok(())
	}

	#[test]
	fn execute_true_is_not_false() -> Result<(), anyhow::Error>
	{
		let result =
			execute_calculation("tests/samples/valid/true_is_not_false.pn")?;
		assert_eq!(result, 200);
		Ok(())
	}

	#[test]
	fn execute_comparison_eq_pointer() -> Result<(), anyhow::Error>
	{
		let result = execute_calculation(
			"tests/samples/valid/comparison_eq_pointer.pn",
		)?;
		assert_eq!(result, 200);
		Ok(())
	}

	#[test]
	fn execute_is_even() -> Result<(), anyhow::Error>
	{
		let result = execute_calculation("examples/is_even.pn")?;
		assert_eq!(result, 200);
		Ok(())
	}

	#[test]
	fn execute_label_hijacking() -> Result<(), anyhow::Error>
	{
		let result =
			execute_calculation("tests/samples/valid/label_hijacking.pn")?;
		assert_eq!(result, 200);
		Ok(())
	}

	#[test]
	fn execute_length_of_array_pointer() -> Result<(), anyhow::Error>
	{
		let result = execute_calculation(
			"tests/samples/valid/length_of_array_pointer.pn",
		)?;
		assert_eq!(result, 200);
		Ok(())
	}

	#[test]
	fn execute_integer_casting() -> Result<(), anyhow::Error>
	{
		let result = execute_calculation("examples/integer_casting.pn")?;
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
		let ir = generator::generate(&declarations, filename, false)?;
		let llistr: std::borrow::Cow<str> = match std::env::var("PENNE_LLI")
		{
			Ok(value) => value.into(),
			Err(std::env::VarError::NotPresent) => "lli".into(),
			Err(e) => return Err(e.into()),
		};
		let mut cmd = std::process::Command::new(llistr.as_ref())
			.stdin(std::process::Stdio::piped())
			.spawn()?;
		cmd.stdin.as_mut().unwrap().write_all(ir.as_bytes())?;
		let status = cmd.wait()?;
		let exitcode = status.code().unwrap();
		Ok(exitcode)
	}
}
