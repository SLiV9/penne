use anyhow::Context;
use anyhow::anyhow;
use std::io::Write;

pub(crate) fn execute_ir(
	ir: &str,
) -> Result<std::process::Output, anyhow::Error>
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

pub fn calculation_result_from_output(
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

pub fn stdout_from_output(
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
