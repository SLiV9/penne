//
// Part of penne
// Copyright (c) 2020-2023 Sander in 't Veld
// License: MIT
//

//! The command line interface outputs colored dumps of intermediate code
//! when run with the _verbose_ flag.

use crate::common;
use crate::error;
use crate::lexer;
use crate::rebuilder;
use crate::resolved;

use serde::Deserialize;
use std::io::Write;
use termcolor::{Color, ColorSpec, StandardStream, WriteColor};

#[derive(Debug, Default, Deserialize, clap::Args)]
#[serde(default, deny_unknown_fields)]
pub struct Options
{
	/// Show a lot of intermediate output
	#[clap(short, long)]
	verbose: bool,

	/// When to use ANSI colors in error messages and intermediate output
	#[clap(long, value_name("WHEN"))]
	#[clap(value_enum, default_value_t=ColorChoice::Auto)]
	color: ColorChoice,

	/// Which character set to use to draw the arrows in error messages
	#[clap(long, value_name("CHARSET"))]
	#[clap(value_enum, default_value_t=CharSet::Unicode)]
	arrows: CharSet,
}

#[derive(Debug, Default, Clone, Copy, Deserialize, clap::ValueEnum)]
pub enum ColorChoice
{
	#[default]
	Auto,
	Always,
	Never,
}

impl From<ColorChoice> for termcolor::ColorChoice
{
	fn from(choice: ColorChoice) -> termcolor::ColorChoice
	{
		match choice
		{
			ColorChoice::Auto => termcolor::ColorChoice::Auto,
			ColorChoice::Always => termcolor::ColorChoice::Always,
			ColorChoice::Never => termcolor::ColorChoice::Never,
		}
	}
}

#[derive(Debug, Default, Clone, Copy, Deserialize, clap::ValueEnum)]
pub enum CharSet
{
	#[default]
	Unicode,
	Ascii,
}

impl From<CharSet> for ariadne::CharSet
{
	fn from(choice: CharSet) -> ariadne::CharSet
	{
		match choice
		{
			CharSet::Unicode => ariadne::CharSet::Unicode,
			CharSet::Ascii => ariadne::CharSet::Ascii,
		}
	}
}

pub struct StdOut
{
	stdout: StandardStream,
	is_verbose: bool,
	report_config: error::Config,
}

impl StdOut
{
	pub fn new(options: Options) -> StdOut
	{
		let stdout = StandardStream::stdout(options.color.into());
		let is_verbose = options.verbose;
		let with_color = match options.color
		{
			ColorChoice::Auto => stdout.supports_color(),
			ColorChoice::Always => true,
			ColorChoice::Never => false,
		};
		let ariadne_config = ariadne::Config::default()
			.with_color(with_color)
			.with_char_set(options.arrows.into());
		let report_config =
			error::Config::from(ariadne_config).with_color(with_color);
		StdOut {
			stdout,
			is_verbose,
			report_config,
		}
	}

	pub fn newline(&mut self) -> Result<(), std::io::Error>
	{
		if self.is_verbose
		{
			writeln!(self.stdout)?;
		}
		Ok(())
	}

	pub fn io_header(
		&mut self,
		preamble: &str,
		path: &std::path::Path,
	) -> Result<(), std::io::Error>
	{
		let colorspec_header = ColorSpec::new();
		self.stdout.set_color(&colorspec_header)?;
		writeln!(self.stdout, "{} {}...", preamble, path.to_string_lossy())?;
		Ok(())
	}

	pub fn cmd_header(
		&mut self,
		preamble: &str,
		command: String,
	) -> Result<(), std::io::Error>
	{
		let colorspec_header = ColorSpec::new();
		self.stdout.set_color(&colorspec_header)?;
		writeln!(self.stdout)?;
		writeln!(self.stdout, "{} {}...", preamble, &command)?;
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

	pub fn show_errors(
		&mut self,
		errors: impl IntoIterator<Item = crate::error::Error>,
		mut source_cache: impl ariadne::Cache<String>,
	) -> Result<(), std::io::Error>
	{
		for error in errors
		{
			writeln!(self.stdout)?;
			let report = error.build_report(self.report_config);
			report.eprint(&mut source_cache)?;
		}
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
