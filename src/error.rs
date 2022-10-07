//
// Part of penne
// Copyright (c) 2020 Sander in 't Veld
// License: MIT
//

pub use crate::lexer;
pub use crate::lexer::Location;
pub use crate::value_type::ValueType;

use ariadne::{Fmt, Report, ReportKind};

pub type Partiable<T> = Result<T, Partial<T>>;

#[must_use]
#[derive(Debug)]
pub struct Partial<T>
{
	pub error: Error,
	pub partial: T,
}

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

impl<T> From<Partial<T>> for Poison<T>
{
	fn from(partial: Partial<T>) -> Self
	{
		let Partial { error, partial } = partial;
		Poison::Error {
			error,
			partial: Some(partial),
		}
	}
}

#[must_use]
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
	MissingReturnType
	{
		inferred_type: ValueType,
		location_of_return_value: Location,
		location_of_declaration: Location,
	},
	MissingAmbiguousReturnType
	{
		location_of_return_value: Location,
		location_of_declaration: Location,
	},
	AmbiguousReturnValue
	{
		declared_type: ValueType,
		location_of_return_value: Location,
		location_of_declaration: Location,
	},
	ConflictingReturnValue
	{
		inferred_type: ValueType,
		declared_type: ValueType,
		location_of_return_value: Location,
		location_of_declaration: Location,
	},
	MissingReturnValue
	{
		declared_type: ValueType,
		location: Location,
		location_of_declaration: Location,
	},
	MissingReturnValueAfterStatement
	{
		location: Location, after: Location
	},
	DuplicateDeclarationVariable
	{
		name: String,
		location: Location,
		previous: Location,
	},
	DuplicateDeclarationConstant
	{
		name: String,
		location: Location,
		previous: Location,
	},
	DuplicateDeclarationFunction
	{
		name: String,
		location: Location,
		previous: Location,
	},
	DuplicateDeclarationLabel
	{
		name: String,
		location: Location,
		previous: Location,
	},
	UndefinedVariable
	{
		name: String, location: Location
	},
	UndefinedFunction
	{
		name: String, location: Location
	},
	UndefinedLabel
	{
		name: String, location: Location
	},
	ConflictingTypes
	{
		name: String,
		current_type: ValueType,
		previous_type: ValueType,
		location: Location,
		previous: Location,
	},
	NotAnArray
	{
		current_type: ValueType,
		location: Location,
		previous: Location,
	},
	NotAnArrayWithLength
	{
		current_type: ValueType,
		location: Location,
		previous: Location,
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
					.label()
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
					.label()
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
					.label()
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
					.label()
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
					.label()
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
					.label()
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
					.label()
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
					.label()
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
					.label()
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
					.label()
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
					.label()
					.with_message(
						"This type is not allowed in external declarations.",
					)
					.with_color(a),
			)
			.with_label(
				location_of_declaration
					.label()
					.with_message("Declaration marked external here.")
					.with_color(a),
			)
			.finish(),

			Error::MissingReturnType { inferred_type,
				location_of_return_value, location_of_declaration } =>
			{
				Report::build(
					ReportKind::Error,
					&location_of_return_value.source_filename,
					location_of_return_value.span.start,
				)
				.with_message("Missing return type")
				.with_label(
					location_of_return_value
						.label()
						.with_message(format!("Value of type {} returned here.",
							show_type(inferred_type).fg(a)))
						.with_color(a),
				)
				.with_label(
					location_of_declaration
						.label()
						.with_message(
							"Function declared here.",
						)
						.with_color(b),
				)
				.finish()
			}

			Error::MissingAmbiguousReturnType {
				location_of_return_value, location_of_declaration } =>
			{
				Report::build(
					ReportKind::Error,
					&location_of_return_value.source_filename,
					location_of_return_value.span.start,
				)
				.with_message("Missing return type")
				.with_label(
					location_of_return_value
						.label()
						.with_message("Value of indiscernible type returned here.")
						.with_color(a),
				)
				.with_label(
					location_of_declaration
						.label()
						.with_message(
							"Function declared here.",
						)
						.with_color(b),
				)
				.finish()
			}

			Error::AmbiguousReturnValue { declared_type, location_of_return_value, location_of_declaration } =>
			{
				Report::build(
					ReportKind::Error,
					&location_of_return_value.source_filename,
					location_of_return_value.span.start,
				)
				.with_message("Ambiguous return value")
				.with_label(
					location_of_return_value
						.label()
						.with_message("Failed to infer the type of this value.")
						.with_color(a),
				)
				.with_label(
					location_of_declaration
						.label()
						.with_message(format!(
							"Expected {} based on this declaration.",
							show_type(declared_type).fg(b)
						))
						.with_color(b),
				)
				.finish()
			}

			Error::ConflictingReturnValue { inferred_type, declared_type, location_of_return_value, location_of_declaration } =>
			{
				Report::build(
					ReportKind::Error,
					&location_of_return_value.source_filename,
					location_of_return_value.span.start,
				)
				.with_message("Conflicting return value")
				.with_label(
					location_of_return_value
						.label()
						.with_message(format!("Value of type {} returned here.",
							show_type(inferred_type).fg(a)))
						.with_color(a),
				)
				.with_label(
					location_of_declaration
						.label()
						.with_message(format!(
							"Expected {} based on this declaration.",
							show_type(declared_type).fg(b)
						))
						.with_color(b),
				)
				.finish()
			}

			Error::MissingReturnValue { declared_type, location, location_of_declaration } =>
			{
				Report::build(
					ReportKind::Error,
					&location.source_filename,
					location.span.start,
				)
				.with_message("Missing return value")
				.with_label(
					location
						.label()
						.with_message("No return value.")
						.with_color(a),
				)
				.with_label(
					location_of_declaration
						.label()
						.with_message(format!(
							"Expected {} based on this declaration.",
							show_type(declared_type).fg(b)
						))
						.with_color(b),
				)
				.finish()
			}

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
						.label()
						.with_message("Expected return value...")
						.with_color(a),
				)
				.with_label(
					after
						.label()
						.with_message(format!(
							"...after this {} statement.",
							"`return`".fg(b)
						))
						.with_priority(-10)
						.with_color(b),
				)
				.finish()
			}

			Error::DuplicateDeclarationVariable { name, location, previous } =>
			{
				Report::build(
					ReportKind::Error,
					&location.source_filename,
					location.span.start,
				)
				.with_message("Duplicate variable")
				.with_label(
					location
						.label()
						.with_message(format!("A variable named '{}' is already defined in this scope.",
							name.fg(a)))
						.with_color(a),
				)
				.with_label(
					previous
						.label()
						.with_message("Previously defined here.")
						.with_color(b),
				)
				.finish()
			}

			Error::DuplicateDeclarationConstant { name, location, previous } =>
			{
				Report::build(
					ReportKind::Error,
					&location.source_filename,
					location.span.start,
				)
				.with_message("Duplicate constant")
				.with_label(
					location
						.label()
						.with_message(format!("The constant '{}' is already defined.",
							name.fg(a)))
						.with_color(a),
				)
				.with_label(
					previous
						.label()
						.with_message("Previously defined here.")
						.with_color(b),
				)
				.finish()
			}

			Error::DuplicateDeclarationFunction { name, location, previous } =>
			{
				Report::build(
					ReportKind::Error,
					&location.source_filename,
					location.span.start,
				)
				.with_message("Duplicate function")
				.with_label(
					location
						.label()
						.with_message(format!("A function named '{}' is already defined.",
							name.fg(a)))
						.with_color(a),
				)
				.with_label(
					previous
						.label()
						.with_message("Previously defined here.")
						.with_color(b),
				)
				.finish()
			}

			Error::DuplicateDeclarationLabel { name, location, previous } =>
			{
				Report::build(
					ReportKind::Error,
					&location.source_filename,
					location.span.start,
				)
				.with_message("Duplicate label")
				.with_label(
					location
						.label()
						.with_message(format!("The label '{}' is already defined in this scope.",
							name.fg(a)))
						.with_color(a),
				)
				.with_label(
					previous
						.label()
						.with_message("Previously defined here.")
						.with_color(b),
				)
				.finish()
			}

			Error::UndefinedVariable { name, location } =>
			{
				Report::build(
					ReportKind::Error,
					&location.source_filename,
					location.span.start,
				)
				.with_message("Undefined reference")
				.with_label(
					location
						.label()
						.with_message(format!("Reference to undefined variable named '{}'.",
							name.fg(a)))
						.with_color(a),
				)
				.finish()
			}

			Error::UndefinedFunction { name, location } =>
			{
				Report::build(
					ReportKind::Error,
					&location.source_filename,
					location.span.start,
				)
				.with_message("Undefined reference")
				.with_label(
					location
						.label()
						.with_message(format!("Reference to undefined function named '{}'.",
							name.fg(a)))
						.with_color(a),
				)
				.finish()
			}

			Error::UndefinedLabel { name, location } =>
			{
				Report::build(
					ReportKind::Error,
					&location.source_filename,
					location.span.start,
				)
				.with_message("Undefined label")
				.with_label(
					location
						.label()
						.with_message(format!("Reference to undefined label '{}'.",
							name.fg(a)))
						.with_color(a),
				)
				.finish()
			}

			Error::ConflictingTypes { name, current_type, previous_type, location, previous } =>
			{
				Report::build(
					ReportKind::Error,
					&location.source_filename,
					location.span.start,
				)
				.with_message("Conflicting types")
				.with_label(
					location
						.label()
						.with_message(format!("'{}' has type {}.",
							name.fg(a),
							show_type(current_type).fg(a)))
						.with_color(a),
				)
				.with_label(
					previous
						.label()
						.with_message(format!("Previously determined to be {}.",
							show_type(previous_type).fg(b)))
						.with_color(b),
				)
				.finish()
			}

			Error::NotAnArray { current_type, location, previous } =>
			{
				Report::build(
					ReportKind::Error,
					&location.source_filename,
					location.span.start,
				)
				.with_message("Conflicting types")
				.with_label(
					location
						.label()
						.with_message(format!("Cannot index into value of non-array type {}.",
							show_type(current_type).fg(a)))
						.with_color(a),
				)
				.with_label(
					previous
						.label()
						.with_message("Type previously determined here.")
						.with_color(b),
				)
				.finish()
			}

			Error::NotAnArrayWithLength { current_type, location, previous } =>
			{
				Report::build(
					ReportKind::Error,
					&location.source_filename,
					location.span.start,
				)
				.with_message("Conflicting types")
				.with_label(
					location
						.label()
						.with_message(format!("Cannot take length of value of non-array type {}.",
							show_type(current_type).fg(a)))
						.with_color(a),
				)
				.with_label(
					previous
						.label()
						.with_message("Type previously determined here.")
						.with_color(b),
				)
				.finish()
			}

		}
	}
}

fn show_type(value_type: &ValueType) -> String
{
	format!("`{}`", show_type_inner(value_type))
}

fn show_type_inner(value_type: &ValueType) -> String
{
	match value_type
	{
		ValueType::Int8 => "i8".to_string(),
		ValueType::Int16 => "i16".to_string(),
		ValueType::Int32 => "i32".to_string(),
		ValueType::Int64 => "i64".to_string(),
		ValueType::Int128 => "i128".to_string(),
		ValueType::Uint8 => "u8".to_string(),
		ValueType::Uint16 => "u16".to_string(),
		ValueType::Uint32 => "u32".to_string(),
		ValueType::Uint64 => "u64".to_string(),
		ValueType::Uint128 => "u128".to_string(),
		ValueType::Usize => "usize".to_string(),
		ValueType::Bool => "bool".to_string(),
		ValueType::Char => "char".to_string(),
		ValueType::String => "STRING".to_string(),
		ValueType::Array {
			element_type,
			length,
		} => format!("[{}]{}", length, show_type_inner(&element_type)),
		ValueType::Slice { element_type } =>
		{
			format!("[]{}", show_type_inner(&element_type))
		}
		ValueType::ExtArray { element_type } =>
		{
			format!("[]{}", show_type_inner(&element_type))
		}
		ValueType::Pointer { deref_type } =>
		{
			format!("&{}", show_type_inner(&deref_type))
		}
		ValueType::View { deref_type } =>
		{
			format!("({})", show_type_inner(&deref_type))
		}
	}
}
