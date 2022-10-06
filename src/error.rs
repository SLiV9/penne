//
// Part of penne
// Copyright (c) 2020 Sander in 't Veld
// License: MIT
//

pub use crate::lexer;
pub use crate::lexer::Location;
pub use crate::value_type::ValueType;

use ariadne::{Report, ReportKind};

pub type Poisonable<T> = Result<T, Poison<T>>;

#[derive(Debug, Clone)]
pub enum Poison<T = ()>
{
	Error
	{
		error: Error,
		partial: Option<T>,
	},
	Poisoned,
}

impl<T> From<Error> for Poison<T>
{
	fn from(error: Error) -> Self
	{
		Poison::Error {
			error,
			partial: None,
		}
	}
}

#[derive(Debug, Clone)]
pub enum Error
{
	UnexpectedEndOfFile
	{
		last_location: Location,
		expectation: String,
	},
	Lexical
	{
		error: lexer::Error,
		location: Location,
		expectation: String,
	},
	UnexpectedToken
	{
		location: Location,
		expectation: String,
	},
	MaximumParseDepthExceeded
	{
		location: Location
	},
	TypeNotAllowedInExtern
	{
		value_type: ValueType,
		location_of_type: Location,
		location_of_declaration: Location,
	},
	MissingReturnValueAfterStatement
	{
		location: Location, after: Location
	},
}

impl Error
{
	pub fn report(&self) -> Report<(String, std::ops::Range<usize>)>
	{
		let mut colors = ariadne::ColorGenerator::new();
		let a = colors.next();
		let b = colors.next();
		let c = colors.next();

		match self
		{
			Error::UnexpectedEndOfFile {
				expectation,
				last_location,
			} => Report::build(
				ReportKind::Error,
				&last_location.source_filename,
				last_location.span.start,
			)
			.with_message("Unexpected end of file")
			.with_label(
				last_location
					.label_after_end()
					.with_message(expectation)
					.with_order(1)
					.with_color(a),
			)
			.with_label(
				last_location
					.label()
					.with_message(format!("after this"))
					.with_order(2)
					.with_color(b),
			)
			.finish(),
		}
	}
}
