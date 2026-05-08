//
// Part of penne
// Copyright (c) 2020 Sander in 't Veld
// License: MIT
//

use penne::execution_test_tools::*;

use pretty_assertions::assert_eq;

#[test]
fn execute_five() -> Result<(), anyhow::Error>
{
	let output = execute("tests/samples/valid/five.pn")?;
	let result = calculation_result_from_output(output)?;
	assert_eq!(result, 5);
	Ok(())
}

#[test]
fn execute_two_plus_seven() -> Result<(), anyhow::Error>
{
	let output = execute("tests/samples/valid/two_plus_seven.pn")?;
	let result = calculation_result_from_output(output)?;
	assert_eq!(result, 9);
	Ok(())
}

#[test]
fn execute_addition() -> Result<(), anyhow::Error>
{
	let output = execute("examples/addition.pn")?;
	let result = calculation_result_from_output(output)?;
	assert_eq!(result, 10);
	Ok(())
}

#[test]
fn execute_collatz() -> Result<(), anyhow::Error>
{
	let output = execute("examples/collatz.pn")?;
	let result = calculation_result_from_output(output)?;
	assert_eq!(result, 111);
	Ok(())
}

#[test]
fn execute_constants() -> Result<(), anyhow::Error>
{
	let output = execute("examples/constants.pn")?;
	let result = calculation_result_from_output(output)?;
	assert_eq!(result, 200);
	Ok(())
}

#[test]
fn execute_bitwise_operations() -> Result<(), anyhow::Error>
{
	let output = execute("examples/bitwise_operations.pn")?;
	let result = calculation_result_from_output(output)?;
	assert_eq!(result, 200);
	Ok(())
}

#[test]
fn execute_bitwise_expression() -> Result<(), anyhow::Error>
{
	let output = execute("tests/samples/valid/bitwise_expression.pn")?;
	let result = calculation_result_from_output(output)?;
	assert_eq!(result, 200);
	Ok(())
}

#[test]
fn execute_comparisons() -> Result<(), anyhow::Error>
{
	let output = execute("examples/comparisons.pn")?;
	let result = calculation_result_from_output(output)?;
	assert_eq!(result, 200);
	Ok(())
}

#[test]
fn execute_scoped_variables() -> Result<(), anyhow::Error>
{
	let output = execute("examples/scoped_variables.pn")?;
	let result = calculation_result_from_output(output)?;
	assert_eq!(result, 200);
	Ok(())
}

#[test]
fn execute_scoped_labels() -> Result<(), anyhow::Error>
{
	let output = execute("examples/scoped_labels.pn")?;
	let result = calculation_result_from_output(output)?;
	assert_eq!(result, 200);
	Ok(())
}

#[test]
fn execute_forward_declare_function() -> Result<(), anyhow::Error>
{
	let output = execute("tests/samples/valid/forward_declare_function.pn")?;
	let result = calculation_result_from_output(output)?;
	assert_eq!(result, 200);
	Ok(())
}

#[test]
fn execute_void_function() -> Result<(), anyhow::Error>
{
	let output = execute("tests/samples/valid/void_function.pn")?;
	let result = calculation_result_from_output(output)?;
	assert_eq!(result, 200);
	Ok(())
}

#[test]
fn execute_array_length() -> Result<(), anyhow::Error>
{
	let output = execute("tests/samples/valid/array_length.pn")?;
	let result = calculation_result_from_output(output)?;
	assert_eq!(result, 200);
	Ok(())
}

#[test]
fn execute_length_of_slice() -> Result<(), anyhow::Error>
{
	let output = execute("tests/samples/valid/length_of_slice.pn")?;
	let result = calculation_result_from_output(output)?;
	assert_eq!(result, 200);
	Ok(())
}

#[test]
fn execute_length_of_string() -> Result<(), anyhow::Error>
{
	let output = execute("tests/samples/valid/length_of_string.pn")?;
	let result = calculation_result_from_output(output)?;
	assert_eq!(result, 200);
	Ok(())
}

#[test]
fn execute_length_of_array_by_pointer() -> Result<(), anyhow::Error>
{
	let output = execute("tests/samples/valid/length_of_array_by_pointer.pn")?;
	let result = calculation_result_from_output(output)?;
	assert_eq!(result, 200);
	Ok(())
}

#[test]
fn execute_array_by_reference() -> Result<(), anyhow::Error>
{
	let output = execute("examples/array_by_reference.pn")?;
	let result = calculation_result_from_output(output)?;
	assert_eq!(result, 200);
	Ok(())
}

#[test]
fn execute_array_reference_by_reference() -> Result<(), anyhow::Error>
{
	let output =
		execute("tests/samples/valid/array_reference_by_reference.pn")?;
	let result = calculation_result_from_output(output)?;
	assert_eq!(result, 200);
	Ok(())
}

#[test]
fn execute_mutable_array() -> Result<(), anyhow::Error>
{
	let output = execute("tests/samples/valid/mutable_array.pn")?;
	let result = calculation_result_from_output(output)?;
	assert_eq!(result, 200);
	Ok(())
}

#[test]
fn execute_multidimensional_array() -> Result<(), anyhow::Error>
{
	let output = execute("tests/samples/valid/multidimensional_array.pn")?;
	let result = calculation_result_from_output(output)?;
	assert_eq!(result, 200);
	Ok(())
}

#[test]
fn execute_reassign_array() -> Result<(), anyhow::Error>
{
	let output = execute("tests/samples/valid/reassign_array.pn")?;
	let result = calculation_result_from_output(output)?;
	assert_eq!(result, 200);
	Ok(())
}

#[test]
fn execute_pointers() -> Result<(), anyhow::Error>
{
	let output = execute("examples/pointers.pn")?;
	let result = calculation_result_from_output(output)?;
	assert_eq!(result, 200);
	Ok(())
}

#[test]
fn execute_pointer_to_member() -> Result<(), anyhow::Error>
{
	let output = execute("tests/samples/valid/pointer_to_member.pn")?;
	let result = calculation_result_from_output(output)?;
	assert_eq!(result, 200);
	Ok(())
}

#[test]
fn execute_pointer_to_mut() -> Result<(), anyhow::Error>
{
	let output = execute("tests/samples/valid/pointer_to_mut.pn")?;
	let result = calculation_result_from_output(output)?;
	assert_eq!(result, 200);
	Ok(())
}

#[test]
fn execute_mutable_pointer() -> Result<(), anyhow::Error>
{
	let output = execute("tests/samples/valid/mutable_pointer.pn")?;
	let result = calculation_result_from_output(output)?;
	assert_eq!(result, 200);
	Ok(())
}

#[test]
fn execute_pointer_to_pointer() -> Result<(), anyhow::Error>
{
	let output = execute("examples/pointer_to_pointer.pn")?;
	let result = calculation_result_from_output(output)?;
	assert_eq!(result, 200);
	Ok(())
}

#[test]
fn execute_pointer_to_array() -> Result<(), anyhow::Error>
{
	let output = execute("tests/samples/valid/pointer_to_array.pn")?;
	let result = calculation_result_from_output(output)?;
	assert_eq!(result, 200);
	Ok(())
}

#[test]
fn execute_array_by_pointer() -> Result<(), anyhow::Error>
{
	let output = execute("examples/array_by_pointer.pn")?;
	let result = calculation_result_from_output(output)?;
	assert_eq!(result, 200);
	Ok(())
}

#[test]
fn execute_array_in_extern() -> Result<(), anyhow::Error>
{
	let output = execute("tests/samples/valid/array_in_extern.pn")?;
	let result = calculation_result_from_output(output)?;
	assert_eq!(result, 200);
	Ok(())
}

#[test]
fn execute_pointer_aliasing() -> Result<(), anyhow::Error>
{
	let output = execute("tests/samples/valid/pointer_aliasing.pn")?;
	let result = calculation_result_from_output(output)?;
	assert_eq!(result, 200);
	Ok(())
}

#[test]
fn execute_view_aliasing() -> Result<(), anyhow::Error>
{
	let output = execute("tests/samples/valid/view_aliasing.pn")?;
	let result = calculation_result_from_output(output)?;
	assert_eq!(result, 200);
	Ok(())
}

#[test]
fn execute_array_aliasing() -> Result<(), anyhow::Error>
{
	let output = execute("tests/samples/valid/array_aliasing.pn")?;
	let result = calculation_result_from_output(output)?;
	assert_eq!(result, 200);
	Ok(())
}

#[test]
fn execute_pointer_to_endless_array() -> Result<(), anyhow::Error>
{
	let output = execute("tests/samples/valid/pointer_to_endless_array.pn")?;
	let result = calculation_result_from_output(output)?;
	assert_eq!(result, 200);
	Ok(())
}

#[test]
fn execute_bitshift_type_inference() -> Result<(), anyhow::Error>
{
	let output = execute("tests/samples/valid/bitshift_type_inference.pn")?;
	let result = calculation_result_from_output(output)?;
	assert_eq!(result, 200);
	Ok(())
}

#[test]
fn execute_true_is_not_false() -> Result<(), anyhow::Error>
{
	let output = execute("tests/samples/valid/true_is_not_false.pn")?;
	let result = calculation_result_from_output(output)?;
	assert_eq!(result, 200);
	Ok(())
}

#[test]
fn execute_u8_to_u16() -> Result<(), anyhow::Error>
{
	let output = execute("tests/samples/valid/u8_to_u16.pn")?;
	let result = calculation_result_from_output(output)?;
	assert_eq!(result, 200);
	Ok(())
}

#[test]
fn execute_comparison_eq_pointer() -> Result<(), anyhow::Error>
{
	let output = execute("tests/samples/valid/comparison_eq_pointer.pn")?;
	let result = calculation_result_from_output(output)?;
	assert_eq!(result, 200);
	Ok(())
}

#[test]
fn execute_is_even() -> Result<(), anyhow::Error>
{
	let output = execute("examples/is_even.pn")?;
	let result = calculation_result_from_output(output)?;
	assert_eq!(result, 200);
	Ok(())
}

#[test]
fn execute_label_hijacking() -> Result<(), anyhow::Error>
{
	let output = execute("tests/samples/valid/label_hijacking.pn")?;
	let result = calculation_result_from_output(output)?;
	assert_eq!(result, 200);
	Ok(())
}

#[test]
fn execute_array_of_pointers() -> Result<(), anyhow::Error>
{
	let output = execute("examples/array_of_pointers.pn")?;
	let result = calculation_result_from_output(output)?;
	assert_eq!(result, 200);
	Ok(())
}

#[test]
fn execute_length_of_array_pointer() -> Result<(), anyhow::Error>
{
	let output = execute("tests/samples/valid/length_of_array_pointer.pn")?;
	let result = calculation_result_from_output(output)?;
	assert_eq!(result, 200);
	Ok(())
}

#[test]
fn execute_integer_casting() -> Result<(), anyhow::Error>
{
	let output = execute("examples/integer_casting.pn")?;
	let result = calculation_result_from_output(output)?;
	assert_eq!(result, 200);
	Ok(())
}

#[test]
fn execute_identity_casting() -> Result<(), anyhow::Error>
{
	let output = execute("tests/samples/valid/identity_casting.pn")?;
	let result = calculation_result_from_output(output)?;
	assert_eq!(result, 200);
	Ok(())
}

#[test]
fn execute_cast_without_as() -> Result<(), anyhow::Error>
{
	let output = execute("tests/samples/valid/cast_without_as.pn")?;
	let result = calculation_result_from_output(output)?;
	assert_eq!(result, 200);
	Ok(())
}

#[test]
fn execute_char8_cast() -> Result<(), anyhow::Error>
{
	let output = execute("tests/samples/valid/char8_cast.pn")?;
	let result = calculation_result_from_output(output)?;
	assert_eq!(result, 200);
	Ok(())
}

#[test]
fn execute_structs_and_words() -> Result<(), anyhow::Error>
{
	let output = execute("examples/structs_and_words.pn")?;
	let result = calculation_result_from_output(output)?;
	assert_eq!(result, 200);
	Ok(())
}

#[test]
fn execute_disorganized_structures() -> Result<(), anyhow::Error>
{
	let output = execute("tests/samples/valid/disorganized_structures.pn")?;
	let result = calculation_result_from_output(output)?;
	assert_eq!(result, 200);
	Ok(())
}

#[test]
fn execute_same_size_structures() -> Result<(), anyhow::Error>
{
	let output = execute("tests/samples/valid/same_size_structures.pn")?;
	let result = calculation_result_from_output(output)?;
	assert_eq!(result, 200);
	Ok(())
}

#[test]
fn execute_size_of_struct() -> Result<(), anyhow::Error>
{
	let output = execute("tests/samples/valid/size_of_struct.pn")?;
	let result = calculation_result_from_output(output)?;
	assert_eq!(result, 200);
	Ok(())
}

#[test]
fn execute_size_of_other_types() -> Result<(), anyhow::Error>
{
	let output = execute("tests/samples/valid/size_of_other_types.pn")?;
	let result = calculation_result_from_output(output)?;
	assert_eq!(result, 200);
	Ok(())
}

#[test]
fn execute_array_literal_as_argument() -> Result<(), anyhow::Error>
{
	let output = execute("tests/samples/valid/array_literal_as_argument.pn")?;
	let result = calculation_result_from_output(output)?;
	assert_eq!(result, 200);
	Ok(())
}

#[test]
fn execute_word_literal_as_argument() -> Result<(), anyhow::Error>
{
	let output = execute("tests/samples/valid/word_literal_as_argument.pn")?;
	let result = calculation_result_from_output(output)?;
	assert_eq!(result, 200);
	Ok(())
}

#[test]
fn execute_struct_literal_as_argument() -> Result<(), anyhow::Error>
{
	let output = execute("tests/samples/valid/struct_literal_as_argument.pn")?;
	let result = calculation_result_from_output(output)?;
	assert_eq!(result, 200);
	Ok(())
}

#[test]
fn execute_constant_array() -> Result<(), anyhow::Error>
{
	let output = execute("tests/samples/valid/constant_array.pn")?;
	let result = calculation_result_from_output(output)?;
	assert_eq!(result, 200);
	Ok(())
}

#[test]
fn execute_minus_128() -> Result<(), anyhow::Error>
{
	let output = execute("tests/samples/valid/minus_128.pn")?;
	let result = calculation_result_from_output(output)?;
	assert_eq!(result, 200);
	Ok(())
}

#[test]
fn execute_binary_integer_66_bit() -> Result<(), anyhow::Error>
{
	let output = execute("tests/samples/valid/binary_integer_66_bit.pn")?;
	let result = calculation_result_from_output(output)?;
	assert_eq!(result, 200);
	Ok(())
}

#[test]
fn execute_typed_integer_too_big_for_u64() -> Result<(), anyhow::Error>
{
	let output =
		execute("tests/samples/valid/typed_integer_too_big_for_u64.pn")?;
	let result = calculation_result_from_output(output)?;
	assert_eq!(result, 200);
	Ok(())
}

#[test]
fn execute_i32_to_le_bytes() -> Result<(), anyhow::Error>
{
	let output = execute("tests/samples/valid/i32_to_le_bytes.pn")?;
	let result = calculation_result_from_output(output)?;
	assert_eq!(result, 200);
	Ok(())
}

#[test]
fn execute_get_from_pointer() -> Result<(), anyhow::Error>
{
	let output = execute("tests/samples/valid/get_from_pointer.pn")?;
	let result = calculation_result_from_output(output)?;
	assert_eq!(result, 200);
	Ok(())
}

#[test]
fn execute_infer_integer_type_from_param() -> Result<(), anyhow::Error>
{
	let output =
		execute("tests/samples/valid/infer_integer_type_from_param.pn")?;
	let result = calculation_result_from_output(output)?;
	assert_eq!(result, 200);
	Ok(())
}

#[test]
fn execute_infer_variable_type_from_param() -> Result<(), anyhow::Error>
{
	let output =
		execute("tests/samples/valid/infer_variable_type_from_param.pn")?;
	let result = calculation_result_from_output(output)?;
	assert_eq!(result, 200);
	Ok(())
}

#[test]
fn execute_pointer_stability_variable() -> Result<(), anyhow::Error>
{
	let output = execute("tests/samples/valid/pointer_stability_variable.pn")?;
	let result = calculation_result_from_output(output)?;
	assert_eq!(result, 200);
	Ok(())
}

#[test]
fn execute_pointer_stability_struct() -> Result<(), anyhow::Error>
{
	let output = execute("tests/samples/valid/pointer_stability_struct.pn")?;
	let result = calculation_result_from_output(output)?;
	assert_eq!(result, 200);
	Ok(())
}

#[test]
fn execute_pointer_arithmetic() -> Result<(), anyhow::Error>
{
	let output = execute("tests/samples/valid/pointer_arithmetic.pn")?;
	let result = calculation_result_from_output(output)?;
	assert_eq!(result, 200);
	Ok(())
}

#[test]
fn execute_empty_array() -> Result<(), anyhow::Error>
{
	let output = execute("tests/samples/valid/empty_array.pn")?;
	let result = calculation_result_from_output(output)?;
	assert_eq!(result, 200);
	Ok(())
}

#[test]
fn execute_pass_view_as_view() -> Result<(), anyhow::Error>
{
	let output = execute("tests/samples/valid/pass_view_as_view.pn")?;
	let result = calculation_result_from_output(output)?;
	assert_eq!(result, 200);
	Ok(())
}

#[test]
fn execute_pass_endless_array_as_endless_array() -> Result<(), anyhow::Error>
{
	let output =
		execute("tests/samples/valid/pass_endless_array_as_endless_array.pn")?;
	let result = calculation_result_from_output(output)?;
	assert_eq!(result, 200);
	Ok(())
}

#[test]
fn execute_identifier_starting_with_keyword() -> Result<(), anyhow::Error>
{
	let output =
		execute("tests/samples/valid/identifier_starting_with_keyword.pn")?;
	let result = calculation_result_from_output(output)?;
	assert_eq!(result, 200);
	Ok(())
}

#[test]
fn execute_unused_forward_declaration() -> Result<(), anyhow::Error>
{
	let output = execute("tests/samples/valid/unused_forward_declaration.pn")?;
	let result = calculation_result_from_output(output)?;
	assert_eq!(result, 200);
	Ok(())
}

#[test]
fn execute_import_position_and_line() -> Result<(), anyhow::Error>
{
	let output = execute_with_imports(&[
		"tests/samples/valid/import_position_and_line.pn",
		"tests/samples/valid/position.pn",
		"tests/samples/valid/line.pn",
	])?;
	let result = calculation_result_from_output(output)?;
	assert_eq!(result, 200);
	Ok(())
}

#[test]
fn execute_import_sum_of_squares() -> Result<(), anyhow::Error>
{
	let output = execute_with_imports(&[
		"tests/samples/valid/import_sum_of_squares.pn",
		"tests/samples/valid/position.pn",
		"tests/samples/valid/sum_of_squares.pn",
	])?;
	let result = calculation_result_from_output(output)?;
	assert_eq!(result, 200);
	Ok(())
}

#[test]
fn execute_hello_world() -> Result<(), anyhow::Error>
{
	let output = execute("examples/hello_world.pn")?;
	let stdout = stdout_from_output(output)?;
	assert_eq!(stdout, "Hello world!\n");
	Ok(())
}

#[test]
fn execute_builtin_print() -> Result<(), anyhow::Error>
{
	let output = execute("tests/samples/valid/builtin_print.pn")?;
	let stdout = stdout_from_output(output)?;
	assert_eq!(
		stdout,
		"Hello world!\nHello Alice!\nHello a\nb!\nHello 255!\nHello \
		 -173!\nHello true!\nHello false!\nHello 32000!\nHello 0x7f00!\nHello \
		 world!\nHello ...!\n"
	);
	Ok(())
}

#[test]
fn execute_builtin_file_and_line() -> Result<(), anyhow::Error>
{
	let filename = "tests/samples/valid/builtin_file_and_line.pn";
	let output = execute(filename)?;
	let stdout = stdout_from_output(output)?;
	assert_eq!(stdout, format!("Hello from {filename}:4!\n"));
	Ok(())
}

#[test]
fn execute_builtin_format() -> Result<(), anyhow::Error>
{
	let output = execute("tests/samples/valid/builtin_format.pn")?;
	let result = calculation_result_from_output(output)?;
	assert_eq!(result, 200);
	Ok(())
}

#[test]
fn execute_builtin_format_i128() -> Result<(), anyhow::Error>
{
	let mut x: i128 = -170141183460469231731687303715884105723;
	let output = execute("tests/samples/valid/builtin_format_i128.pn")?;
	let stdout = stdout_from_output(output)?;
	for line in stdout.lines()
	{
		assert_eq!(line, format!("{x}"));
		x /= 10;
	}
	assert_eq!(x, 0);
	Ok(())
}

#[test]
fn execute_builtin_format_u128() -> Result<(), anyhow::Error>
{
	let mut x: u128 = 245897425987205987254872359832589484829;
	let output = execute("tests/samples/valid/builtin_format_u128.pn")?;
	let stdout = stdout_from_output(output)?;
	for line in stdout.lines()
	{
		assert_eq!(line, format!("{x}"));
		x /= 10;
	}
	assert_eq!(x, 0);
	Ok(())
}

#[test]
fn execute_log_10_u128() -> Result<(), anyhow::Error>
{
	let output = execute("tests/samples/valid/log_10_u128.pn")?;
	let result = calculation_result_from_output(output)?;
	assert_eq!(result, 39);
	Ok(())
}

#[ignore]
#[test]
fn execute_builtin_format_array() -> Result<(), anyhow::Error>
{
	let output = execute("tests/samples/valid/builtin_format_array.pn")?;
	let stdout = stdout_from_output(output)?;
	let expected = "[]\n[200]\n[12, 34]\n[1, 22, 333, 4444, 55555, -6]\n";
	assert_eq!(stdout, expected);
	Ok(())
}

#[ignore]
#[test]
fn execute_builtin_format_slice() -> Result<(), anyhow::Error>
{
	let output = execute("tests/samples/valid/builtin_format_slice.pn")?;
	let stdout = stdout_from_output(output)?;
	let expected = "[]\n[200]\n[12, 34]\n[1, 22, 333, 4444, 55555, -6]\n";
	assert_eq!(stdout, expected);
	Ok(())
}

#[ignore]
#[test]
fn execute_builtin_format_struct() -> Result<(), anyhow::Error>
{
	let output = execute("tests/samples/valid/builtin_format_struct.pn")?;
	let stdout = stdout_from_output(output)?;
	let expected = "Empty {}\nPoint { x: 12, y: 34 }\nFoo { point: Point { x: \
	                12, y: 34 }, length: 128, other: 0x0 }\n";
	assert_eq!(stdout, expected);
	Ok(())
}

#[test]
fn execute_and_crash_builtin_abort() -> Result<(), anyhow::Error>
{
	const POSIX_SIGABRT: i32 = 6;
	let output = execute("tests/samples/valid/builtin_abort.pn")?;
	if cfg!(not(target_os = "windows"))
	{
		use std::os::unix::process::ExitStatusExt;
		let signal = output.status.signal().expect("Expected signal");
		assert_eq!(signal, POSIX_SIGABRT);
	}
	assert!(!output.status.success());
	Ok(())
}

#[test]
fn execute_invalid_pointer_escape_ub() -> Result<(), anyhow::Error>
{
	let output = execute("tests/samples/invalid/pointer_escape_ub.pn")?;
	let result = calculation_result_from_output(output)?;
	// This test is not entirely portable or precise, but it works.
	assert_ne!(result, 123);
	Ok(())
}
