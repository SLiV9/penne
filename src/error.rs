//
// Part of penne
// Copyright (c) 2020 Sander in 't Veld
// License: MIT
//

pub use crate::lexer;
pub use crate::lexer::Location;
pub use crate::value_type::ValueType;

use ariadne::{Fmt, Report, ReportKind};

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
					.with_message(format!("Expected more after this."))
					.with_order(2)
					.with_color(b),
			)
			.finish(),

			Error::Lexical {
				error: lexer::Error::UnexpectedCharacter,
				expectation,
				location,
			} => Report::build(
				ReportKind::Error,
				&location.source_filename,
				location.span.start,
			)
			.with_message("Unexpected character")
			.with_label(
				location
					.label_after_end()
					.with_message(expectation)
					.with_color(a),
			)
			.finish(),

			Error::Lexical {
				error: lexer::Error::InvalidIntegerLiteral(inner_error),
				expectation,
				location,
			} => Report::build(
				ReportKind::Error,
				&location.source_filename,
				location.span.start,
			)
			.with_message("Invalid integer literal")
			.with_label(
				location
					.label_after_end()
					.with_message(expectation)
					.with_color(a),
			)
			.with_note(format!("{}", inner_error))
			.finish(),

			Error::Lexical {
				error: lexer::Error::InvalidNakedIntegerLiteral,
				expectation,
				location,
			} => Report::build(
				ReportKind::Error,
				&location.source_filename,
				location.span.start,
			)
			.with_message("Invalid untyped integer literal")
			.with_label(
				location
					.label_after_end()
					.with_message(expectation)
					.with_color(a),
			)
			.with_note(format!("Consider adding a type suffix like {}.",
				"`i128`".fg(a)))
			.finish(),

			Error::Lexical {
				error: lexer::Error::InvalidIntegerTypeSuffix,
				expectation,
				location,
			} => Report::build(
				ReportKind::Error,
				&location.source_filename,
				location.span.start,
			)
			.with_message("Invalid integer literal type suffix")
			.with_label(
				location
					.label_after_end()
					.with_message(expectation)
					.with_color(a),
			)
			.finish(),

			Error::Lexical {
				error: lexer::Error::InvalidEscapeSequence,
				expectation,
				location,
			} => Report::build(
				ReportKind::Error,
				&location.source_filename,
				location.span.start,
			)
			.with_message("Invalid escape sequence")
			.with_label(
				location
					.label_after_end()
					.with_message(expectation)
					.with_color(a),
			)
			.finish(),

			Error::Lexical {
				error: lexer::Error::UnexpectedTrailingBackslash,
				expectation,
				location,
			} => Report::build(
				ReportKind::Error,
				&location.source_filename,
				location.span.start,
			)
			.with_message("Unexpected trailing backslash")
			.with_label(
				location
					.label_after_end()
					.with_message(expectation)
					.with_color(a),
			)
			.with_note("To continue a string across multiple lines, close it and then reopen it on the next line.")
			.finish(),

			Error::Lexical {
				error: lexer::Error::MissingClosingQuote,
				expectation,
				location,
			} => Report::build(
				ReportKind::Error,
				&location.source_filename,
				location.span.start,
			)
			.with_message("Missing closing quote")
			.with_label(
				location
					.label_after_end()
					.with_message(expectation)
					.with_color(a),
			)
			.with_note("To continue a string across multiple lines, close it and then reopen it on the next line.")
			.finish(),

			Error::Lexical {
				error: lexer::Error::InvalidMixedString,
				expectation,
				location,
			} => Report::build(
				ReportKind::Error,
				&location.source_filename,
				location.span.start,
			)
			.with_message("Invalid mixed string")
			.with_label(
				location
					.label_after_end()
					.with_message(expectation)
					.with_color(a),
			)
			.with_note("String literals cannot contain both ASCII control characters and non-ASCII characters.")
			.finish(),

			Error::UnexpectedToken {
				expectation,
				location,
			} => Report::build(
				ReportKind::Error,
				&location.source_filename,
				location.span.start,
			)
			.with_message("Unexpected token")
			.with_label(
				location
					.label_after_end()
					.with_message(expectation)
					.with_color(a),
			)
			.finish(),

			Error::MaximumParseDepthExceeded { location } => Report::build(
				ReportKind::Error,
				&location.source_filename,
				location.span.start,
			)
			.with_message("Maximum parse depth exceeded")
			.with_label(
				location
					.label_after_end()
					.with_message("This is too complex to parse.")
					.with_color(a),
			)
			.finish(),

			Error::TypeNotAllowedInExtern {
				value_type: _,
				location_of_type,
				location_of_declaration,
			} => Report::build(
				ReportKind::Error,
				&location_of_type.source_filename,
				location_of_type.span.start,
			)
			.with_message("Invalid external type")
			.with_label(
				location_of_type
					.label_after_end()
					.with_message(
						"This type is not allowed in external declarations.",
					)
					.with_color(a),
			)
			.with_label(
				location_of_declaration
					.label_after_end()
					.with_message("Declaration marked external here.")
					.with_color(a),
			)
			.finish(),

			Error::MissingReturnValueAfterStatement { location, after } =>
			{
				Report::build(
					ReportKind::Error,
					&location.source_filename,
					location.span.start,
				)
				.with_message("Missing return value")
				.with_label(
					location
						.label_after_end()
						.with_message("Expected return value...")
						.with_order(1)
						.with_color(a),
				)
				.with_label(
					after
						.label()
						.with_message(format!(
							"...after this {} statement.",
							"`return`".fg(b)
						))
						.with_order(2)
						.with_color(b),
				)
				.finish()
			}
		}
	}
}
