//
// Part of penne
// Copyright (c) 2020-2023 Sander in 't Veld
// License: MIT
//

use crate::common;
use crate::lexer;
use crate::rebuilder;
use crate::resolved;

use std::io::Write;
use termcolor::{Color, ColorChoice, ColorSpec, StandardStream, WriteColor};

pub struct StdOut
{
	stdout: StandardStream,
	is_verbose: bool,
}

impl StdOut
{
	pub fn new(is_verbose: bool) -> StdOut
	{
		StdOut {
			stdout: StandardStream::stdout(ColorChoice::Auto),
			is_verbose,
		}
	}

	pub fn newline(&mut self) -> Result<(), std::io::Error>
	{
		writeln!(self.stdout)?;
		Ok(())
	}

	pub fn header(
		&mut self,
		preamble: &str,
		filename: &str,
	) -> Result<(), std::io::Error>
	{
		if self.is_verbose
		{
			let colorspec_header = ColorSpec::new();
			self.stdout.set_color(&colorspec_header)?;
			writeln!(self.stdout, "{} {}...", preamble, filename)?;
		}
		Ok(())
	}

	pub fn basic_header(
		&mut self,
		section_name: &str,
	) -> Result<(), std::io::Error>
	{
		if self.is_verbose
		{
			let colorspec_header = ColorSpec::new();
			self.stdout.set_color(&colorspec_header)?;
			writeln!(self.stdout, "{}...", section_name)?;
		}
		Ok(())
	}

	pub fn dump_tokens(
		&mut self,
		tokens: &[lexer::LexedToken],
	) -> Result<(), anyhow::Error>
	{
		if self.is_verbose
		{
			let colorspec_dump = ColorSpec::new().set_dimmed(true).to_owned();
			self.stdout.set_color(&colorspec_dump)?;
			writeln!(self.stdout, "{:?}", tokens)?;
			writeln!(self.stdout)?;
			for token in tokens
			{
				match &token.result
				{
					Result::Ok(token) => write!(self.stdout, "{:?}   ", token)?,
					Result::Err(_) => write!(self.stdout, "ERROR   ")?,
				}
			}
			writeln!(self.stdout)?;
			writeln!(self.stdout)?;
		}
		Ok(())
	}

	pub fn dump_code(
		&mut self,
		filename: &str,
		declarations: &[common::Declaration],
	) -> Result<(), anyhow::Error>
	{
		if self.is_verbose
		{
			let colorspec_dump = ColorSpec::new().set_dimmed(true).to_owned();
			self.stdout.set_color(&colorspec_dump)?;
			writeln!(self.stdout, "{:?}", declarations)?;
			writeln!(self.stdout)?;

			self.header("Rebuilding", filename)?;

			self.stdout.set_color(&colorspec_dump)?;
			let indentation = rebuilder::Indentation {
				value: "\u{00a6}   ",
				amount: 1,
			};
			let code = rebuilder::rebuild(declarations, &indentation)?;
			writeln!(self.stdout, "{}", code)?;
		}
		Ok(())
	}

	pub fn dump_resolved(
		&mut self,
		_filename: &str,
		declarations: &[resolved::Declaration],
	) -> Result<(), anyhow::Error>
	{
		if self.is_verbose
		{
			let colorspec_dump = ColorSpec::new().set_dimmed(true).to_owned();
			self.stdout.set_color(&colorspec_dump)?;
			writeln!(self.stdout, "{:?}", declarations)?;
			writeln!(self.stdout)?;
		}
		Ok(())
	}

	pub fn dump_text(&mut self, text: &str) -> Result<(), anyhow::Error>
	{
		if self.is_verbose
		{
			let colorspec_dump = ColorSpec::new().set_dimmed(true).to_owned();
			self.stdout.set_color(&colorspec_dump)?;
			writeln!(self.stdout, "{}", text)?;
			writeln!(self.stdout)?;
		}
		Ok(())
	}

	pub fn linting(
		&mut self,
		was_successful: bool,
	) -> Result<(), std::io::Error>
	{
		if self.is_verbose
		{
			if was_successful
			{
				let colorspec_success =
					ColorSpec::new().set_fg(Some(Color::Green)).to_owned();
				self.stdout.set_color(&colorspec_success)?;
				writeln!(self.stdout, "Linting complete.")?;
			}
			else
			{
				let colorspec_warning = ColorSpec::new()
					.set_fg(Some(Color::Yellow))
					.set_bold(true)
					.to_owned();
				self.stdout.set_color(&colorspec_warning)?;
				writeln!(self.stdout, "Linting raised some warnings.")?;
			}
		}
		Ok(())
	}

	pub fn prepare_for_errors(&mut self) -> Result<(), std::io::Error>
	{
		let colorspec_error = ColorSpec::new()
			.set_fg(Some(Color::Red))
			.set_bold(true)
			.to_owned();
		self.stdout.set_color(&colorspec_error)?;
		writeln!(self.stdout)?;
		Ok(())
	}

	pub fn output(&mut self, value: i32) -> Result<(), std::io::Error>
	{
		self.stdout.reset()?;
		writeln!(self.stdout, "Output: {}", value)?;
		Ok(())
	}

	pub fn done(&mut self) -> Result<(), std::io::Error>
	{
		self.stdout.reset()?;
		writeln!(self.stdout, "Done.")?;
		Ok(())
	}
}
