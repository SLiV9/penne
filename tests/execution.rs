//
// Part of penne
// Copyright (c) 2020 Sander in 't Veld
// License: MIT
//

use penne::*;

use anyhow::anyhow;
use anyhow::Context;
use pretty_assertions::assert_eq;
use std::io::Write;

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
	let result = execute_calculation("tests/samples/valid/two_plus_seven.pn")?;
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
fn execute_collatz() -> Result<(), anyhow::Error>
{
	let result = execute_calculation("examples/collatz.pn")?;
	assert_eq!(result, 111);
	Ok(())
}

#[test]
fn execute_constants() -> Result<(), anyhow::Error>
{
	let result = execute_calculation("examples/constants.pn")?;
	assert_eq!(result, 200);
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
fn execute_bitwise_expression() -> Result<(), anyhow::Error>
{
	let result =
		execute_calculation("tests/samples/valid/bitwise_expression.pn")?;
	assert_eq!(result, 200);
	Ok(())
}

#[test]
fn execute_comparisons() -> Result<(), anyhow::Error>
{
	let result = execute_calculation("examples/comparisons.pn")?;
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
	let result =
		execute_calculation("tests/samples/valid/forward_declare_function.pn")?;
	assert_eq!(result, 200);
	Ok(())
}

#[test]
fn execute_void_function() -> Result<(), anyhow::Error>
{
	let result = execute_calculation("tests/samples/valid/void_function.pn")?;
	assert_eq!(result, 200);
	Ok(())
}

#[test]
fn execute_array_length() -> Result<(), anyhow::Error>
{
	let result = execute_calculation("tests/samples/valid/array_length.pn")?;
	assert_eq!(result, 200);
	Ok(())
}

#[test]
fn execute_length_of_slice() -> Result<(), anyhow::Error>
{
	let result = execute_calculation("tests/samples/valid/length_of_slice.pn")?;
	assert_eq!(result, 200);
	Ok(())
}

#[test]
fn execute_length_of_string() -> Result<(), anyhow::Error>
{
	let result =
		execute_calculation("tests/samples/valid/length_of_string.pn")?;
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
	let result = execute_calculation("tests/samples/valid/mutable_array.pn")?;
	assert_eq!(result, 200);
	Ok(())
}

#[test]
fn execute_multidimensional_array() -> Result<(), anyhow::Error>
{
	let result =
		execute_calculation("tests/samples/valid/multidimensional_array.pn")?;
	assert_eq!(result, 200);
	Ok(())
}

#[test]
fn execute_reassign_array() -> Result<(), anyhow::Error>
{
	let result = execute_calculation("tests/samples/valid/reassign_array.pn")?;
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
fn execute_pointer_to_member() -> Result<(), anyhow::Error>
{
	let result =
		execute_calculation("tests/samples/valid/pointer_to_member.pn")?;
	assert_eq!(result, 200);
	Ok(())
}

#[test]
fn execute_pointer_to_mut() -> Result<(), anyhow::Error>
{
	let result = execute_calculation("tests/samples/valid/pointer_to_mut.pn")?;
	assert_eq!(result, 200);
	Ok(())
}

#[test]
fn execute_mutable_pointer() -> Result<(), anyhow::Error>
{
	let result = execute_calculation("tests/samples/valid/mutable_pointer.pn")?;
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
	let result = execute_calculation("tests/samples/valid/array_in_extern.pn")?;
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
	let result = execute_calculation("tests/samples/valid/array_aliasing.pn")?;
	assert_eq!(result, 200);
	Ok(())
}

#[test]
fn execute_pointer_to_endless_array() -> Result<(), anyhow::Error>
{
	let result =
		execute_calculation("tests/samples/valid/pointer_to_endless_array.pn")?;
	assert_eq!(result, 200);
	Ok(())
}

#[test]
fn execute_bitshift_type_inference() -> Result<(), anyhow::Error>
{
	let result =
		execute_calculation("tests/samples/valid/bitshift_type_inference.pn")?;
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
fn execute_u8_to_u16() -> Result<(), anyhow::Error>
{
	let result = execute_calculation("tests/samples/valid/u8_to_u16.pn")?;
	assert_eq!(result, 200);
	Ok(())
}

#[test]
fn execute_comparison_eq_pointer() -> Result<(), anyhow::Error>
{
	let result =
		execute_calculation("tests/samples/valid/comparison_eq_pointer.pn")?;
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
	let result = execute_calculation("tests/samples/valid/label_hijacking.pn")?;
	assert_eq!(result, 200);
	Ok(())
}

#[test]
fn execute_array_of_pointers() -> Result<(), anyhow::Error>
{
	let result = execute_calculation("examples/array_of_pointers.pn")?;
	assert_eq!(result, 200);
	Ok(())
}

#[test]
fn execute_length_of_array_pointer() -> Result<(), anyhow::Error>
{
	let result =
		execute_calculation("tests/samples/valid/length_of_array_pointer.pn")?;
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

#[test]
fn execute_identity_casting() -> Result<(), anyhow::Error>
{
	let result =
		execute_calculation("tests/samples/valid/identity_casting.pn")?;
	assert_eq!(result, 200);
	Ok(())
}

#[test]
fn execute_cast_without_as() -> Result<(), anyhow::Error>
{
	let result = execute_calculation("tests/samples/valid/cast_without_as.pn")?;
	assert_eq!(result, 200);
	Ok(())
}

#[test]
fn execute_char8_cast() -> Result<(), anyhow::Error>
{
	let result = execute_calculation("tests/samples/valid/char8_cast.pn")?;
	assert_eq!(result, 200);
	Ok(())
}

#[test]
fn execute_structs_and_words() -> Result<(), anyhow::Error>
{
	let result = execute_calculation("examples/structs_and_words.pn")?;
	assert_eq!(result, 200);
	Ok(())
}

#[test]
fn execute_disorganized_structures() -> Result<(), anyhow::Error>
{
	let result =
		execute_calculation("tests/samples/valid/disorganized_structures.pn")?;
	assert_eq!(result, 200);
	Ok(())
}

#[test]
fn execute_same_size_structures() -> Result<(), anyhow::Error>
{
	let result =
		execute_calculation("tests/samples/valid/same_size_structures.pn")?;
	assert_eq!(result, 200);
	Ok(())
}

#[test]
fn execute_size_of_struct() -> Result<(), anyhow::Error>
{
	let result = execute_calculation("tests/samples/valid/size_of_struct.pn")?;
	assert_eq!(result, 200);
	Ok(())
}

#[test]
fn execute_size_of_other_types() -> Result<(), anyhow::Error>
{
	let result =
		execute_calculation("tests/samples/valid/size_of_other_types.pn")?;
	assert_eq!(result, 200);
	Ok(())
}

#[test]
fn execute_array_literal_as_argument() -> Result<(), anyhow::Error>
{
	let result = execute_calculation(
		"tests/samples/valid/array_literal_as_argument.pn",
	)?;
	assert_eq!(result, 200);
	Ok(())
}

#[test]
fn execute_word_literal_as_argument() -> Result<(), anyhow::Error>
{
	let result =
		execute_calculation("tests/samples/valid/word_literal_as_argument.pn")?;
	assert_eq!(result, 200);
	Ok(())
}

#[test]
fn execute_struct_literal_as_argument() -> Result<(), anyhow::Error>
{
	let result = execute_calculation(
		"tests/samples/valid/struct_literal_as_argument.pn",
	)?;
	assert_eq!(result, 200);
	Ok(())
}

#[test]
fn execute_constant_array() -> Result<(), anyhow::Error>
{
	let result = execute_calculation("tests/samples/valid/constant_array.pn")?;
	assert_eq!(result, 200);
	Ok(())
}

#[test]
fn execute_minus_128() -> Result<(), anyhow::Error>
{
	let result = execute_calculation("tests/samples/valid/minus_128.pn")?;
	assert_eq!(result, 200);
	Ok(())
}

#[test]
fn execute_binary_integer_66_bit() -> Result<(), anyhow::Error>
{
	let result =
		execute_calculation("tests/samples/valid/binary_integer_66_bit.pn")?;
	assert_eq!(result, 200);
	Ok(())
}

#[test]
fn execute_typed_integer_too_big_for_u64() -> Result<(), anyhow::Error>
{
	let result = execute_calculation(
		"tests/samples/valid/typed_integer_too_big_for_u64.pn",
	)?;
	assert_eq!(result, 200);
	Ok(())
}

#[test]
fn execute_i32_to_le_bytes() -> Result<(), anyhow::Error>
{
	let result = execute_calculation("tests/samples/valid/i32_to_le_bytes.pn")?;
	assert_eq!(result, 200);
	Ok(())
}

#[test]
fn execute_get_from_pointer() -> Result<(), anyhow::Error>
{
	let result =
		execute_calculation("tests/samples/valid/get_from_pointer.pn")?;
	assert_eq!(result, 200);
	Ok(())
}

#[test]
fn execute_infer_integer_type_from_param() -> Result<(), anyhow::Error>
{
	let result = execute_calculation(
		"tests/samples/valid/infer_integer_type_from_param.pn",
	)?;
	assert_eq!(result, 200);
	Ok(())
}

#[test]
fn execute_infer_variable_type_from_param() -> Result<(), anyhow::Error>
{
	let result = execute_calculation(
		"tests/samples/valid/infer_variable_type_from_param.pn",
	)?;
	assert_eq!(result, 200);
	Ok(())
}

#[test]
fn execute_pointer_stability_variable() -> Result<(), anyhow::Error>
{
	let result = execute_calculation(
		"tests/samples/valid/pointer_stability_variable.pn",
	)?;
	assert_eq!(result, 200);
	Ok(())
}

#[test]
fn execute_pointer_stability_struct() -> Result<(), anyhow::Error>
{
	let result =
		execute_calculation("tests/samples/valid/pointer_stability_struct.pn")?;
	assert_eq!(result, 200);
	Ok(())
}

#[test]
fn execute_empty_array() -> Result<(), anyhow::Error>
{
	let result = execute_calculation("tests/samples/valid/empty_array.pn")?;
	assert_eq!(result, 200);
	Ok(())
}

#[test]
fn execute_pass_view_as_view() -> Result<(), anyhow::Error>
{
	let result =
		execute_calculation("tests/samples/valid/pass_view_as_view.pn")?;
	assert_eq!(result, 200);
	Ok(())
}

#[test]
fn execute_pass_endless_array_as_endless_array() -> Result<(), anyhow::Error>
{
	let result = execute_calculation(
		"tests/samples/valid/pass_endless_array_as_endless_array.pn",
	)?;
	assert_eq!(result, 200);
	Ok(())
}

#[test]
fn execute_identifier_starting_with_keyword() -> Result<(), anyhow::Error>
{
	let result = execute_calculation(
		"tests/samples/valid/identifier_starting_with_keyword.pn",
	)?;
	assert_eq!(result, 200);
	Ok(())
}

#[test]
fn execute_unused_forward_declaration() -> Result<(), anyhow::Error>
{
	let result = execute_calculation(
		"tests/samples/valid/unused_forward_declaration.pn",
	)?;
	assert_eq!(result, 200);
	Ok(())
}

#[test]
fn execute_import_position_and_line() -> Result<(), anyhow::Error>
{
	let result = execute_calculation_with_imports(&[
		"tests/samples/valid/import_position_and_line.pn",
		"tests/samples/valid/position.pn",
		"tests/samples/valid/line.pn",
	])?;
	assert_eq!(result, 200);
	Ok(())
}

#[test]
fn execute_import_sum_of_squares() -> Result<(), anyhow::Error>
{
	let result = execute_calculation_with_imports(&[
		"tests/samples/valid/import_sum_of_squares.pn",
		"tests/samples/valid/position.pn",
		"tests/samples/valid/sum_of_squares.pn",
	])?;
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
	let result = execute_calculation("tests/samples/valid/builtin_format.pn")?;
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
	let result = execute_calculation("tests/samples/valid/log_10_u128.pn")?;
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
	if cfg!(target_os = "unix")
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
	let result =
		execute_calculation("tests/samples/invalid/pointer_escape_ub.pn")?;
	// This test is not entirely portable or precise, but it works.
	assert_ne!(result, 123);
	Ok(())
}

fn execute_calculation(filename: &str) -> Result<i32, anyhow::Error>
{
	let output = execute(filename)?;
	calculation_result_from_output(output)
}

fn execute(filename: &str) -> Result<std::process::Output, anyhow::Error>
{
	let source = std::fs::read_to_string(filename)?;
	let tokens = lexer::lex(&source, filename);
	let declarations = parser::parse(tokens);
	let declarations = expander::expand_one(filename, declarations);
	match resolver::check_surface_level_errors(&declarations)
	{
		Ok(_) => (),
		#[allow(unreachable_code)]
		Err(errors) => match errors.panic() {},
	}
	let declarations = scoper::analyze(declarations);
	let mut compiler = Compiler::default();
	compiler.add_module(&filename)?;
	let declarations = match compiler.analyze_and_resolve(declarations)?
	{
		Ok(declarations) => declarations,
		#[allow(unreachable_code)]
		Err(errors) => match errors.panic() {},
	};
	compiler.compile(&declarations)?;
	let ir = compiler.generate_ir()?;
	execute_ir(&ir)
}

fn execute_calculation_with_imports(
	filenames: &[&str],
) -> Result<i32, anyhow::Error>
{
	let output = execute_with_imports(filenames)?;
	calculation_result_from_output(output)
}

fn execute_with_imports(
	filenames: &[&str],
) -> Result<std::process::Output, anyhow::Error>
{
	let mut modules = Vec::new();
	for filename in filenames
	{
		let source = std::fs::read_to_string(&filename)?;
		let tokens = lexer::lex(&source, filename);
		let declarations = parser::parse(tokens);
		let filepath = filename.parse()?;
		modules.push((filepath, declarations));
	}
	expander::expand(&mut modules);
	for (_filepath, declarations) in &modules
	{
		match resolver::check_surface_level_errors(&declarations)
		{
			Ok(_) => declarations,
			#[allow(unreachable_code)]
			Err(errors) => match errors.panic() {},
		};
	}
	let mut compiler = Compiler::default();
	for (filepath, declarations) in modules
	{
		let filename = filepath.to_str().unwrap();
		compiler.add_module(&filename)?;
		let declarations = scoper::analyze(declarations);
		let declarations = match compiler.analyze_and_resolve(declarations)?
		{
			Ok(declarations) => declarations,
			#[allow(unreachable_code)]
			Err(errors) => match errors.panic() {},
		};
		compiler.compile(&declarations)?;
	}
	compiler.link_modules()?;
	let ir = compiler.generate_ir()?;
	execute_ir(&ir)
}

fn execute_ir(ir: &str) -> Result<std::process::Output, anyhow::Error>
{
	let llistr: std::borrow::Cow<str> = match std::env::var("PENNE_LLI")
	{
		Ok(value) => value.into(),
		Err(std::env::VarError::NotPresent) => "lli".into(),
		Err(e) => return Err(e.into()),
	};
	let mut cmd = std::process::Command::new(llistr.as_ref())
		.stdin(std::process::Stdio::piped())
		.stdout(std::process::Stdio::piped())
		.stderr(std::process::Stdio::piped())
		.spawn()?;
	cmd.stdin.as_mut().unwrap().write_all(ir.as_bytes())?;
	let output = cmd.wait_with_output()?;
	Ok(output)
}

fn calculation_result_from_output(
	output: std::process::Output,
) -> Result<i32, anyhow::Error>
{
	let stdout = String::from_utf8(output.stdout)?;
	println!("STDOUT\n{}\nSTDOUT", stdout);
	if output.stderr.is_empty()
	{
		let exitcode = output.status.code().context("No status code")?;
		Ok(exitcode)
	}
	else
	{
		let stderr = std::str::from_utf8(&output.stderr);
		if let Ok(stderr) = stderr
		{
			eprintln!("STDERR\n{}\nSTDERR", stderr);
		}
		Err(anyhow!("Unexpected stderr: {:?}", stderr))
	}
}

fn stdout_from_output(
	output: std::process::Output,
) -> Result<String, anyhow::Error>
{
	let stdout = String::from_utf8(output.stdout)?;
	println!("STDOUT\n{}\nSTDOUT", stdout);
	if output.stderr.is_empty()
	{
		Ok(stdout)
	}
	else
	{
		let stderr = std::str::from_utf8(&output.stderr);
		if let Ok(stderr) = stderr
		{
			eprintln!("STDERR\n{}\nSTDERR", stderr);
		}
		Err(anyhow!("Unexpected stderr: {:?}", stderr))
	}
}
