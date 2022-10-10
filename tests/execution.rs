//
// Part of penne
// Copyright (c) 2020 Sander in 't Veld
// License: MIT
//

use penne::*;

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
fn execute_pointers() -> Result<(), anyhow::Error>
{
	let result = execute_calculation("examples/pointers.pn")?;
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
fn execute_pointer_to_slice() -> Result<(), anyhow::Error>
{
	let result =
		execute_calculation("tests/samples/valid/pointer_to_slice.pn")?;
	assert_eq!(result, 200);
	Ok(())
}

#[test]
fn execute_length_of_pointer_to_slice() -> Result<(), anyhow::Error>
{
	let result = execute_calculation(
		"tests/samples/valid/length_of_pointer_to_slice.pn",
	)?;
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

fn execute_calculation(filename: &str) -> Result<i32, anyhow::Error>
{
	let source = std::fs::read_to_string(&filename)?;
	let declarations = match penne::compile_source(&source, &filename)
	{
		Ok(declarations) => declarations,
		#[allow(unreachable_code)]
		Err(errors) => match errors.panic() {},
	};
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
