//
// Part of penne
// Copyright (c) 2020 Sander in 't Veld
// License: MIT
//

//! Compiler stages may generate syntax errors and semantical errors.

#![cfg_attr(coverage, feature(no_coverage))]

pub use crate::lexer;
pub use crate::lexer::Location;

use crate::common::Identifier;
use crate::value_type;

pub type OperandValueType = value_type::OperandValueType<Identifier>;
pub type ValueType = value_type::ValueType<Identifier>;

use ariadne::{Fmt, Report, ReportKind};

#[derive(Debug)]
pub struct Errors
{
	pub errors: Vec<Error>,
}

impl From<Error> for Errors
{
	fn from(error: Error) -> Self
	{
		Self {
			errors: vec![error],
		}
	}
}

impl From<Poison> for Errors
{
	fn from(poison: Poison) -> Self
	{
		match poison
		{
			Poison::Error(error) => error.into(),
			Poison::Poisoned =>
			{
				// Do not show any errors because this thing was poisoned by
				// a different error, and cascading errors are unreliable.
				Errors { errors: Vec::new() }
			}
		}
	}
}

impl<T1, T2> From<(T1, T2)> for Errors
where
	T1: Into<Errors>,
	T2: Into<Errors>,
{
	fn from(ab: (T1, T2)) -> Self
	{
		let (a, b) = ab;
		let mut errors = a.into();
		let mut more = b.into();
		errors.errors.append(&mut more.errors);
		errors
	}
}

impl Errors
{
	#[cfg_attr(coverage, no_coverage)]
	#[cfg(not(tarpaulin_include))]
	pub fn panic(self) -> Never
	{
		match self.errors.into_iter().next()
		{
			Some(error) => panic!("{:?}", error),
			None => panic!("empty errors"),
		}
	}

	pub fn codes(&self) -> Vec<u16>
	{
		self.errors.iter().map(|x| x.code()).collect()
	}
}

pub enum Never {}

impl IntoIterator for Errors
{
	type Item = Error;
	type IntoIter = <Vec<Error> as IntoIterator>::IntoIter;

	fn into_iter(self) -> Self::IntoIter
	{
		self.errors.into_iter()
	}
}

pub type Poisonable<T> = Result<T, Poison>;

#[derive(Debug, Clone)]
pub enum Poison
{
	Error(Error),
	Poisoned,
}

impl From<Error> for Poison
{
	fn from(error: Error) -> Self
	{
		Poison::Error(error)
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
	UnexpectedSemicolonAfterIdentifier
	{
		location: Location, after: Location
	},
	UnexpectedSemicolonAfterReturnValue
	{
		location: Location, after: Location
	},
	MaximumParseDepthExceeded
	{
		location: Location
	},
	MissingConstantType
	{
		location: Location
	},
	MissingParameterType
	{
		location: Location
	},
	MissingMemberType
	{
		location: Location
	},
	IllegalType
	{
		value_type: ValueType,
		location: Location,
	},
	IllegalConstantType
	{
		value_type: ValueType,
		location: Location,
	},
	IllegalVariableType
	{
		value_type: ValueType,
		location: Location,
	},
	IllegalParameterType
	{
		value_type: ValueType,
		location: Location,
	},
	IllegalReturnType
	{
		value_type: ValueType,
		location: Location,
	},
	IllegalMemberType
	{
		value_type: ValueType,
		in_word: bool,
		location: Location,
	},
	TypeNotAllowedInExtern
	{
		value_type: ValueType,
		location_of_type: Location,
		location_of_declaration: Location,
	},
	WordSizeMismatch
	{
		inferred_size_in_bits: usize,
		declared_size_in_bits: usize,
		location_of_identifier: Location,
		location_of_keyword: Location,
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
	DuplicateDeclarationParameter
	{
		name: String,
		location: Location,
		previous: Location,
	},
	DuplicateDeclarationStructure
	{
		name: String,
		location: Location,
		previous: Location,
	},
	DuplicateDeclarationMember
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
	CyclicalConstant
	{
		name: String, location: Location
	},
	CyclicalStructure
	{
		name: String,
		location_of_member: Location,
		location_of_declaration: Location,
	},
	UndefinedVariable
	{
		name: String, location: Location
	},
	UndefinedFunction
	{
		name: String, location: Location
	},
	UndefinedStructure
	{
		name: String, location: Location
	},
	UndefinedMember
	{
		name_of_member: String,
		name_of_structure: String,
		location: Location,
		location_of_declaration: Location,
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
	ConflictingTypesInAssignment
	{
		name: String,
		current_type: ValueType,
		previous_type: ValueType,
		location: Location,
		previous: Location,
	},
	ArgumentTypeMismatch
	{
		parameter_name: String,
		argument_type: ValueType,
		parameter_type: ValueType,
		location: Location,
		location_of_declaration: Location,
	},
	ArgumentMissingAddress
	{
		parameter_name: String,
		argument_type: ValueType,
		parameter_type: ValueType,
		location: Location,
		location_of_declaration: Location,
	},
	IndexTypeMismatch
	{
		argument_type: ValueType,
		index_type: ValueType,
		location: Location,
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
	NotAStructure
	{
		current_type: ValueType,
		location: Location,
		previous: Location,
	},
	TooFewArguments
	{
		location: Location,
		location_of_declaration: Location,
	},
	TooManyArguments
	{
		location: Location,
		location_of_declaration: Location,
	},
	UnsupportedInConstContext
	{
		location: Location
	},
	FunctionInConstContext
	{
		location: Location
	},
	CannotCopyArray
	{
		location: Location
	},
	CannotCopySlice
	{
		location: Location
	},
	CannotCopyStruct
	{
		location: Location
	},
	NotMutable
	{
		location: Location,
		location_of_declaration: Location,
	},
	AddressOfTemporaryAddress
	{
		location: Location,
		location_of_declaration: Location,
	},
	MissingBraces
	{
		location: Location
	},
	NonFinalLoopStatement
	{
		location: Location,
		location_of_block: Location,
	},
	MisplacedLoopStatement
	{
		location: Location
	},
	VariableDeclarationMayBeSkipped
	{
		label: String,
		location: Location,
		location_of_goto: Location,
		location_of_label: Location,
	},
	AmbiguousType
	{
		location: Location
	},
	AmbiguousTypeOfDeclaration
	{
		location: Location
	},
	AmbiguousTypeOfNakedIntegerLiteral
	{
		suggested_type: ValueType,
		location: Location,
	},
	AmbiguousTypeOfArrayLiteral
	{
		location: Location
	},
	AmbiguousTypeOfStringLiteral
	{
		location: Location
	},
	MismatchedOperandTypes
	{
		type_of_left: ValueType,
		type_of_right: ValueType,
		location_of_op: Location,
		location_of_left: Location,
		location_of_right: Location,
	},
	InvalidOperandType
	{
		value_type: ValueType,
		possible_types: Vec<OperandValueType>,
		location_of_op: Location,
		location_of_operand: Location,
	},
	InvalidPrimitiveCast
	{
		value_type: ValueType,
		coerced_type: ValueType,
		possible_value_types: Vec<OperandValueType>,
		possible_coerced_types: Vec<OperandValueType>,
		location_of_operand: Location,
		location_of_type: Location,
	},
}

impl Error
{
	pub fn code(&self) -> u16
	{
		match self
		{
			Error::UnexpectedEndOfFile { .. } => 100,
			Error::Lexical {
				error: lexer::Error::UnexpectedCharacter,
				..
			} => 110,
			Error::Lexical {
				error: lexer::Error::InvalidIntegerLiteral(..),
				..
			} => 140,
			Error::Lexical {
				error: lexer::Error::InvalidIntegerTypeSuffix,
				..
			} => 141,
			Error::Lexical {
				error: lexer::Error::InvalidNakedIntegerLiteral,
				..
			} => 142,
			Error::Lexical {
				error: lexer::Error::InvalidBitIntegerLiteral,
				..
			} => 143,
			Error::Lexical {
				error: lexer::Error::MissingClosingQuote,
				..
			} => 160,
			Error::Lexical {
				error: lexer::Error::UnexpectedTrailingBackslash,
				..
			} => 161,
			Error::Lexical {
				error: lexer::Error::InvalidEscapeSequence,
				..
			} => 162,
			Error::Lexical {
				error: lexer::Error::InvalidMixedString,
				..
			} => 163,
			Error::UnexpectedToken { .. } => 300,
			Error::UnexpectedSemicolonAfterIdentifier { .. } => 301,
			Error::UnexpectedSemicolonAfterReturnValue { .. } => 302,
			Error::MissingReturnType { .. } => 330,
			Error::MissingAmbiguousReturnType { .. } => 331,
			Error::AmbiguousReturnValue { .. } => 332,
			Error::ConflictingReturnValue { .. } => 333,
			Error::MissingReturnValue { .. } => 334,
			Error::MissingReturnValueAfterStatement { .. } => 335,
			Error::MissingConstantType { .. } => 343,
			Error::MissingParameterType { .. } => 344,
			Error::MissingMemberType { .. } => 346,
			Error::IllegalType { .. } => 350,
			Error::IllegalReturnType { .. } => 351,
			Error::IllegalVariableType { .. } => 352,
			Error::IllegalConstantType { .. } => 353,
			Error::IllegalParameterType { .. } => 354,
			Error::IllegalMemberType { .. } => 356,
			Error::TypeNotAllowedInExtern { .. } => 358,
			Error::UnsupportedInConstContext { .. } => 360,
			Error::FunctionInConstContext { .. } => 361,
			Error::WordSizeMismatch { .. } => 380,
			Error::MaximumParseDepthExceeded { .. } => 390,
			Error::UndefinedLabel { .. } => 400,
			Error::UndefinedFunction { .. } => 401,
			Error::UndefinedVariable { .. } => 402,
			Error::UndefinedStructure { .. } => 405,
			Error::UndefinedMember { .. } => 406,
			Error::CyclicalConstant { .. } => 413,
			Error::CyclicalStructure { .. } => 415,
			Error::DuplicateDeclarationLabel { .. } => 420,
			Error::DuplicateDeclarationFunction { .. } => 421,
			Error::DuplicateDeclarationVariable { .. } => 422,
			Error::DuplicateDeclarationConstant { .. } => 423,
			Error::DuplicateDeclarationParameter { .. } => 424,
			Error::DuplicateDeclarationStructure { .. } => 425,
			Error::DuplicateDeclarationMember { .. } => 426,
			Error::ConflictingTypes { .. } => 500,
			Error::NotAnArray { .. } => 501,
			Error::NotAnArrayWithLength { .. } => 502,
			Error::IndexTypeMismatch { .. } => 503,
			Error::ConflictingTypesInAssignment { .. } => 504,
			Error::NotAStructure { .. } => 505,
			Error::TooFewArguments { .. } => 510,
			Error::TooManyArguments { .. } => 511,
			Error::ArgumentTypeMismatch { .. } => 512,
			Error::ArgumentMissingAddress { .. } => 513,
			Error::NotMutable { .. } => 530,
			Error::CannotCopyArray { .. } => 531,
			Error::CannotCopySlice { .. } => 532,
			Error::CannotCopyStruct { .. } => 533,
			Error::AddressOfTemporaryAddress { .. } => 538,
			Error::InvalidOperandType { .. } => 550,
			Error::MismatchedOperandTypes { .. } => 551,
			Error::InvalidPrimitiveCast { .. } => 552,
			Error::AmbiguousType { .. } => 580,
			Error::AmbiguousTypeOfDeclaration { .. } => 581,
			Error::AmbiguousTypeOfNakedIntegerLiteral { .. } => 582,
			Error::AmbiguousTypeOfArrayLiteral { .. } => 583,
			Error::AmbiguousTypeOfStringLiteral { .. } => 584,
			Error::NonFinalLoopStatement { .. } => 800,
			Error::MisplacedLoopStatement { .. } => 801,
			Error::VariableDeclarationMayBeSkipped { .. } => 820,
			Error::MissingBraces { .. } => 840,
		}
	}

	#[cfg_attr(coverage, no_coverage)]
	#[cfg(not(tarpaulin_include))]
	pub fn report(&self) -> Report<(String, std::ops::Range<usize>)>
	{
		let a = ariadne::Color::Yellow;
		let b = ariadne::Color::Cyan;
		let c = ariadne::Color::Magenta;

		match self
		{
			Error::UnexpectedEndOfFile {
				expectation,
				last_location,
			} => Report::build(
				ReportKind::Error,
				&last_location.source_filename,
				last_location.span.end,
			)
			.with_code(format!("E{}", self.code()))
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
					.with_message("Expected more after this.".to_string())
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
			.with_code(format!("E{}", self.code()))
			.with_message("Unexpected character")
			.with_label(
				location.label().with_message(expectation).with_color(a),
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
			.with_code(format!("E{}", self.code()))
			.with_message("Invalid integer literal")
			.with_label(
				location.label().with_message(expectation).with_color(a),
			)
			.with_note(format!("{}", inner_error))
			.finish(),

			Error::Lexical {
				error: lexer::Error::InvalidBitIntegerLiteral,
				expectation,
				location,
			} => Report::build(
				ReportKind::Error,
				&location.source_filename,
				location.span.start,
			)
			.with_code(format!("E{}", self.code()))
			.with_message("Invalid bit integer literal")
			.with_label(
				location.label().with_message(expectation).with_color(a),
			)
			.with_note(format!(
				"Hexadecimal and binary integer literals have to fit {}.",
				"`u64`".fg(a)
			))
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
			.with_code(format!("E{}", self.code()))
			.with_message("Invalid untyped integer literal")
			.with_label(
				location.label().with_message(expectation).with_color(a),
			)
			.with_note(format!(
				"Consider adding a type suffix like {}.",
				"`i128`".fg(a)
			))
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
			.with_code(format!("E{}", self.code()))
			.with_message("Invalid integer literal type suffix")
			.with_label(
				location.label().with_message(expectation).with_color(a),
			)
			.finish(),

			Error::Lexical {
				error: lexer::Error::InvalidEscapeSequence,
				expectation: _,
				location,
			} => Report::build(
				ReportKind::Error,
				&location.source_filename,
				location.span.start,
			)
			.with_code(format!("E{}", self.code()))
			.with_message("Invalid escape sequence")
			.with_label(
				location
					.label()
					.with_message("Invalid character escape sequence")
					.with_color(a),
			)
			.finish(),

			Error::Lexical {
				error: lexer::Error::UnexpectedTrailingBackslash,
				expectation: _,
				location,
			} => Report::build(
				ReportKind::Error,
				&location.source_filename,
				location.span.start,
			)
			.with_code(format!("E{}", self.code()))
			.with_message("Unexpected trailing backslash")
			.with_label(
				location
					.label()
					.with_message("Unexpected trailing backslash")
					.with_color(a),
			)
			.with_note(
				"To continue a string across multiple lines, close it and \
				 then reopen it on the next line.",
			)
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
			.with_code(format!("E{}", self.code()))
			.with_message("Missing closing quote")
			.with_label(
				location.label().with_message(expectation).with_color(a),
			)
			.with_note(
				"To continue a string across multiple lines, close it and \
				 then reopen it on the next line.",
			)
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
			.with_code(format!("E{}", self.code()))
			.with_message("Invalid mixed string")
			.with_label(
				location.label().with_message(expectation).with_color(a),
			)
			.with_note(
				"String literals cannot contain both ASCII control characters \
				 and non-ASCII characters.",
			)
			.finish(),

			Error::UnexpectedToken {
				expectation,
				location,
			} => Report::build(
				ReportKind::Error,
				&location.source_filename,
				location.span.start,
			)
			.with_code(format!("E{}", self.code()))
			.with_message("Unexpected token")
			.with_label(
				location.label().with_message(expectation).with_color(a),
			)
			.finish(),

			Error::UnexpectedSemicolonAfterIdentifier { location, after } =>
			{
				Report::build(
					ReportKind::Error,
					&location.source_filename,
					location.span.start,
				)
				.with_code(format!("E{}", self.code()))
				.with_message("Unexpected token")
				.with_label(
					location
						.label()
						.with_message("Unexpected semicolon")
						.with_color(a),
				)
				.with_label(
					after
						.label()
						.with_order(2)
						.with_message("This is not a valid statement.")
						.with_color(b),
				)
				.finish()
			}

			Error::UnexpectedSemicolonAfterReturnValue { location, after } =>
			{
				Report::build(
					ReportKind::Error,
					&location.source_filename,
					location.span.start,
				)
				.with_code(format!("E{}", self.code()))
				.with_message("Unexpected semicolon after return value")
				.with_label(
					location
						.label()
						.with_message("Unexpected semicolon")
						.with_color(a),
				)
				.with_label(
					after
						.label()
						.with_order(2)
						.with_message(
							"This is the return value of this function.",
						)
						.with_color(b),
				)
				.finish()
			}

			Error::MaximumParseDepthExceeded { location } => Report::build(
				ReportKind::Error,
				&location.source_filename,
				location.span.start,
			)
			.with_code(format!("E{}", self.code()))
			.with_message("Maximum parse depth exceeded")
			.with_label(
				location
					.label()
					.with_message("This is too complex to parse.")
					.with_color(a),
			)
			.finish(),

			Error::MissingConstantType { location } => Report::build(
				ReportKind::Error,
				&location.source_filename,
				location.span.start,
			)
			.with_code(format!("E{}", self.code()))
			.with_message("Missing type")
			.with_label(
				location
					.label()
					.with_message("Constants need an explicit type.")
					.with_color(a),
			)
			.finish(),

			Error::MissingParameterType { location } => Report::build(
				ReportKind::Error,
				&location.source_filename,
				location.span.start,
			)
			.with_code(format!("E{}", self.code()))
			.with_message("Missing type")
			.with_label(
				location
					.label()
					.with_message("Function parameters need an explicit type.")
					.with_color(a),
			)
			.finish(),

			Error::MissingMemberType { location } => Report::build(
				ReportKind::Error,
				&location.source_filename,
				location.span.start,
			)
			.with_code(format!("E{}", self.code()))
			.with_message("Missing type")
			.with_label(
				location
					.label()
					.with_message("Structure members need an explicit type.")
					.with_color(a),
			)
			.finish(),

			Error::IllegalType {
				value_type,
				location,
			} => Report::build(
				ReportKind::Error,
				&location.source_filename,
				location.span.start,
			)
			.with_code(format!("E{}", self.code()))
			.with_message("Invalid type")
			.with_label(
				location
					.label()
					.with_message(format!(
						"The type {} is invalid.",
						show_type(value_type).fg(a)
					))
					.with_color(a),
			)
			.finish(),

			Error::IllegalVariableType {
				value_type,
				location,
			} => Report::build(
				ReportKind::Error,
				&location.source_filename,
				location.span.start,
			)
			.with_code(format!("E{}", self.code()))
			.with_message("Invalid variable type")
			.with_label(
				location
					.label()
					.with_message(format!(
						"A value of type {} cannot be assigned to a variable.",
						show_type(value_type).fg(a)
					))
					.with_color(a),
			)
			.finish(),

			Error::IllegalParameterType {
				value_type,
				location,
			} => Report::build(
				ReportKind::Error,
				&location.source_filename,
				location.span.start,
			)
			.with_code(format!("E{}", self.code()))
			.with_message("Invalid parameter type")
			.with_label(
				location
					.label()
					.with_message(format!(
						"The type {} is not allowed as a parameter.",
						show_type(value_type).fg(a)
					))
					.with_color(a),
			)
			.finish(),

			Error::IllegalReturnType {
				value_type,
				location,
			} => Report::build(
				ReportKind::Error,
				&location.source_filename,
				location.span.start,
			)
			.with_code(format!("E{}", self.code()))
			.with_message("Invalid return type")
			.with_label(
				location
					.label()
					.with_message(format!(
						"The type {} is not allowed as a return value.",
						show_type(value_type).fg(a)
					))
					.with_color(a),
			)
			.finish(),

			Error::IllegalMemberType {
				value_type,
				in_word: false,
				location,
			} => Report::build(
				ReportKind::Error,
				&location.source_filename,
				location.span.start,
			)
			.with_code(format!("E{}", self.code()))
			.with_message("Invalid member type")
			.with_label(
				location
					.label()
					.with_message(format!(
						"The type {} is not allowed as a member of a struct.",
						show_type(value_type).fg(a),
					))
					.with_color(a),
			)
			.finish(),

			Error::IllegalMemberType {
				value_type,
				in_word: true,
				location,
			} => Report::build(
				ReportKind::Error,
				&location.source_filename,
				location.span.start,
			)
			.with_code(format!("E{}", self.code()))
			.with_message("Invalid member type")
			.with_label(
				location
					.label()
					.with_message(format!(
						"The type {} is not allowed as a member of a word.",
						show_type(value_type).fg(a),
					))
					.with_color(a),
			)
			.finish(),

			Error::IllegalConstantType {
				value_type,
				location,
			} => Report::build(
				ReportKind::Error,
				&location.source_filename,
				location.span.start,
			)
			.with_code(format!("E{}", self.code()))
			.with_message("Invalid constant type")
			.with_label(
				location
					.label()
					.with_message(format!(
						"A value of type {} cannot be assigned to a constant.",
						show_type(value_type).fg(a)
					))
					.with_color(a),
			)
			.finish(),

			Error::TypeNotAllowedInExtern {
				value_type,
				location_of_type,
				location_of_declaration,
			} => Report::build(
				ReportKind::Error,
				&location_of_type.source_filename,
				location_of_type.span.start,
			)
			.with_code(format!("E{}", self.code()))
			.with_message("Invalid external type")
			.with_label(
				location_of_type
					.label()
					.with_message(format!(
						"The type {} is not allowed in external declarations.",
						show_type(value_type).fg(a)
					))
					.with_color(a),
			)
			.with_label(
				location_of_declaration
					.label()
					.with_message("Declaration marked external here.")
					.with_color(b),
			)
			.finish(),

			Error::WordSizeMismatch {
				inferred_size_in_bits,
				declared_size_in_bits,
				location_of_identifier,
				location_of_keyword,
			} => Report::build(
				ReportKind::Error,
				&location_of_keyword.source_filename,
				location_of_keyword.span.start,
			)
			.with_code(format!("E{}", self.code()))
			.with_message("Conflicting type sizes")
			.with_label(
				location_of_identifier
					.label()
					.with_message(format!(
						"The size of this structure is {} bits.",
						inferred_size_in_bits.to_string().fg(a)
					))
					.with_color(a),
			)
			.with_label(
				location_of_keyword
					.label()
					.with_message(format!(
						"The structure is declared with {} and therefore has \
						 a maximum size of {} bits.",
						format!("`word{}`", declared_size_in_bits).fg(b),
						declared_size_in_bits.to_string().fg(b),
					))
					.with_color(b),
			)
			.finish(),

			Error::MissingReturnType {
				inferred_type,
				location_of_return_value,
				location_of_declaration,
			} => Report::build(
				ReportKind::Error,
				&location_of_return_value.source_filename,
				location_of_return_value.span.start,
			)
			.with_code(format!("E{}", self.code()))
			.with_message("Missing return type")
			.with_label(
				location_of_return_value
					.label()
					.with_message(format!(
						"Value of type {} returned here.",
						show_type(inferred_type).fg(a)
					))
					.with_color(a),
			)
			.with_label(
				location_of_declaration
					.label()
					.with_message("Function declared here.")
					.with_color(b),
			)
			.finish(),

			Error::MissingAmbiguousReturnType {
				location_of_return_value,
				location_of_declaration,
			} => Report::build(
				ReportKind::Error,
				&location_of_return_value.source_filename,
				location_of_return_value.span.start,
			)
			.with_code(format!("E{}", self.code()))
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
					.with_message("Function declared here.")
					.with_color(b),
			)
			.finish(),

			Error::AmbiguousReturnValue {
				declared_type,
				location_of_return_value,
				location_of_declaration,
			} => Report::build(
				ReportKind::Error,
				&location_of_return_value.source_filename,
				location_of_return_value.span.start,
			)
			.with_code(format!("E{}", self.code()))
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
			.finish(),

			Error::ConflictingReturnValue {
				inferred_type,
				declared_type,
				location_of_return_value,
				location_of_declaration,
			} => Report::build(
				ReportKind::Error,
				&location_of_return_value.source_filename,
				location_of_return_value.span.start,
			)
			.with_code(format!("E{}", self.code()))
			.with_message("Conflicting return value")
			.with_label(
				location_of_return_value
					.label()
					.with_message(format!(
						"Value of type {} returned here.",
						show_type(inferred_type).fg(a)
					))
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
			.finish(),

			Error::MissingReturnValue {
				declared_type,
				location,
				location_of_declaration,
			} => Report::build(
				ReportKind::Error,
				&location.source_filename,
				location.span.start,
			)
			.with_code(format!("E{}", self.code()))
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
					after
						.label_after_end()
						.with_message("Expected return value...")
						.with_color(a),
				)
				.with_label(
					after
						.label()
						.with_message(format!(
							"...after this {} label.",
							"`return`".fg(b)
						))
						.with_order(2)
						.with_color(b),
				)
				.finish()
			}

			Error::DuplicateDeclarationVariable {
				name,
				location,
				previous,
			} => Report::build(
				ReportKind::Error,
				&location.source_filename,
				location.span.start,
			)
			.with_code(format!("E{}", self.code()))
			.with_message("Duplicate variable")
			.with_label(
				location
					.label()
					.with_message(format!(
						"A variable named '{}' is already defined in this \
						 scope.",
						name.fg(a)
					))
					.with_color(a),
			)
			.with_label(
				previous
					.label()
					.with_message("Previously defined here.")
					.with_color(b),
			)
			.finish(),

			Error::DuplicateDeclarationConstant {
				name,
				location,
				previous,
			} => Report::build(
				ReportKind::Error,
				&location.source_filename,
				location.span.start,
			)
			.with_code(format!("E{}", self.code()))
			.with_message("Duplicate constant")
			.with_label(
				location
					.label()
					.with_message(format!(
						"The constant '{}' is already defined.",
						name.fg(a)
					))
					.with_color(a),
			)
			.with_label(
				previous
					.label()
					.with_message("Previously defined here.")
					.with_color(b),
			)
			.finish(),

			Error::DuplicateDeclarationFunction {
				name,
				location,
				previous,
			} => Report::build(
				ReportKind::Error,
				&location.source_filename,
				location.span.start,
			)
			.with_code(format!("E{}", self.code()))
			.with_message("Duplicate function")
			.with_label(
				location
					.label()
					.with_message(format!(
						"A function named '{}' is already defined.",
						name.fg(a)
					))
					.with_color(a),
			)
			.with_label(
				previous
					.label()
					.with_message("Previously defined here.")
					.with_color(b),
			)
			.finish(),

			Error::DuplicateDeclarationParameter {
				name,
				location,
				previous,
			} => Report::build(
				ReportKind::Error,
				&location.source_filename,
				location.span.start,
			)
			.with_code(format!("E{}", self.code()))
			.with_message("Duplicate parameter")
			.with_label(
				location
					.label()
					.with_message(format!(
						"A parameter named '{}' is already defined for this \
						 function.",
						name.fg(a)
					))
					.with_color(a),
			)
			.with_label(
				previous
					.label()
					.with_message("Previously defined here.")
					.with_color(b),
			)
			.finish(),

			Error::DuplicateDeclarationStructure {
				name,
				location,
				previous,
			} => Report::build(
				ReportKind::Error,
				&location.source_filename,
				location.span.start,
			)
			.with_code(format!("E{}", self.code()))
			.with_message("Duplicate structure")
			.with_label(
				location
					.label()
					.with_message(format!(
						"A struct or word named '{}' is already defined.",
						name.fg(a)
					))
					.with_color(a),
			)
			.with_label(
				previous
					.label()
					.with_message("Previously defined here.")
					.with_color(b),
			)
			.finish(),

			Error::DuplicateDeclarationMember {
				name,
				location,
				previous,
			} => Report::build(
				ReportKind::Error,
				&location.source_filename,
				location.span.start,
			)
			.with_code(format!("E{}", self.code()))
			.with_message("Duplicate member")
			.with_label(
				location
					.label()
					.with_message(format!(
						"A member named '{}' is already defined in this \
						 structure.",
						name.fg(a)
					))
					.with_color(a),
			)
			.with_label(
				previous
					.label()
					.with_message("Previously defined here.")
					.with_color(b),
			)
			.finish(),

			Error::DuplicateDeclarationLabel {
				name,
				location,
				previous,
			} => Report::build(
				ReportKind::Error,
				&location.source_filename,
				location.span.start,
			)
			.with_code(format!("E{}", self.code()))
			.with_message("Duplicate label")
			.with_label(
				location
					.label()
					.with_message(format!(
						"The label '{}' is already defined in this scope.",
						name.fg(a)
					))
					.with_color(a),
			)
			.with_label(
				previous
					.label()
					.with_message("Previously defined here.")
					.with_color(b),
			)
			.finish(),

			Error::CyclicalConstant { name, location } => Report::build(
				ReportKind::Error,
				&location.source_filename,
				location.span.start,
			)
			.with_code(format!("E{}", self.code()))
			.with_message("Cyclical definition")
			.with_label(
				location
					.label()
					.with_message(format!(
						"The definition of constant '{}' depends on itself.",
						name.fg(a)
					))
					.with_color(a),
			)
			.finish(),

			Error::CyclicalStructure {
				name,
				location_of_member,
				location_of_declaration,
			} => Report::build(
				ReportKind::Error,
				&location_of_declaration.source_filename,
				location_of_declaration.span.start,
			)
			.with_code(format!("E{}", self.code()))
			.with_message("Cyclical definition")
			.with_label(
				location_of_declaration
					.label()
					.with_message(format!(
						"Cannot determine size of structure '{}' that \
						 contains itself.",
						name.fg(a)
					))
					.with_color(a),
			)
			.with_label(
				location_of_member
					.label()
					.with_message(format!(
						"This member contains '{}'.",
						name.fg(a)
					))
					.with_color(b),
			)
			.finish(),

			Error::UndefinedVariable { name, location } => Report::build(
				ReportKind::Error,
				&location.source_filename,
				location.span.start,
			)
			.with_code(format!("E{}", self.code()))
			.with_message("Undefined reference")
			.with_label(
				location
					.label()
					.with_message(format!(
						"Reference to undefined variable named '{}'.",
						name.fg(a)
					))
					.with_color(a),
			)
			.finish(),

			Error::UndefinedFunction { name, location } => Report::build(
				ReportKind::Error,
				&location.source_filename,
				location.span.start,
			)
			.with_code(format!("E{}", self.code()))
			.with_message("Undefined reference")
			.with_label(
				location
					.label()
					.with_message(format!(
						"Reference to undefined function named '{}'.",
						name.fg(a)
					))
					.with_color(a),
			)
			.finish(),

			Error::UndefinedStructure { name, location } => Report::build(
				ReportKind::Error,
				&location.source_filename,
				location.span.start,
			)
			.with_code(format!("E{}", self.code()))
			.with_message("Undefined reference")
			.with_label(
				location
					.label()
					.with_message(format!(
						"Reference to undefined struct or word named '{}'.",
						name.fg(a)
					))
					.with_color(a),
			)
			.finish(),

			Error::UndefinedMember {
				name_of_member,
				name_of_structure,
				location,
				location_of_declaration,
			} => Report::build(
				ReportKind::Error,
				&location.source_filename,
				location.span.start,
			)
			.with_code(format!("E{}", self.code()))
			.with_message("Undefined member")
			.with_label(
				location
					.label()
					.with_message(format!(
						"No member '{}' exists for structure '{}'.",
						name_of_member.fg(a),
						name_of_structure.fg(b),
					))
					.with_color(a),
			)
			.with_label(
				location_of_declaration
					.label()
					.with_message(format!(
						"The structure '{}' is defined here.",
						name_of_structure.fg(b),
					))
					.with_color(b),
			)
			.finish(),

			Error::UndefinedLabel { name, location } => Report::build(
				ReportKind::Error,
				&location.source_filename,
				location.span.start,
			)
			.with_code(format!("E{}", self.code()))
			.with_message("Undefined label")
			.with_label(
				location
					.label()
					.with_message(format!(
						"Reference to undefined label '{}'.",
						name.fg(a)
					))
					.with_color(a),
			)
			.finish(),

			Error::ConflictingTypes {
				name,
				current_type,
				previous_type,
				location,
				previous,
			} => Report::build(
				ReportKind::Error,
				&location.source_filename,
				location.span.start,
			)
			.with_code(format!("E{}", self.code()))
			.with_message("Conflicting types")
			.with_label(
				location
					.label()
					.with_message(format!(
						"'{}' has type {}.",
						name.fg(a),
						show_type(current_type).fg(a)
					))
					.with_color(a),
			)
			.with_label(
				previous
					.label()
					.with_message(format!(
						"Previously determined to be {}.",
						show_type(previous_type).fg(b)
					))
					.with_color(b),
			)
			.finish(),

			Error::ConflictingTypesInAssignment {
				name,
				current_type,
				previous_type,
				location,
				previous,
			} => Report::build(
				ReportKind::Error,
				&location.source_filename,
				location.span.start,
			)
			.with_code(format!("E{}", self.code()))
			.with_message("Conflicting types")
			.with_label(
				location
					.label()
					.with_message(format!(
						"Cannot assign it this expression of type {}.",
						show_type(current_type).fg(a)
					))
					.with_color(a),
			)
			.with_label(
				previous
					.label()
					.with_message(format!(
						"'{}' has type {}.",
						name.fg(b),
						show_type(previous_type).fg(b)
					))
					.with_color(b),
			)
			.finish(),

			Error::ArgumentTypeMismatch {
				parameter_name,
				argument_type,
				parameter_type,
				location,
				location_of_declaration,
			} => Report::build(
				ReportKind::Error,
				&location.source_filename,
				location.span.start,
			)
			.with_code(format!("E{}", self.code()))
			.with_message("Mismatched types")
			.with_label(
				location
					.label()
					.with_message(format!(
						"Argument has type {}.",
						show_type(argument_type).fg(a)
					))
					.with_color(a),
			)
			.with_label(
				location_of_declaration
					.label()
					.with_message(format!(
						"Parameter '{}' has type {}.",
						parameter_name.fg(b),
						show_type(parameter_type).fg(b)
					))
					.with_color(b),
			)
			.finish(),

			Error::ArgumentMissingAddress {
				parameter_name,
				argument_type,
				parameter_type,
				location,
				location_of_declaration,
			} => Report::build(
				ReportKind::Error,
				&location.source_filename,
				location.span.start,
			)
			.with_code(format!("E{}", self.code()))
			.with_message("Mismatched types")
			.with_label(
				location
					.label()
					.with_message(format!(
						"Argument has type {}.",
						show_type(argument_type).fg(a)
					))
					.with_color(a),
			)
			.with_label(
				location
					.label_before_start()
					.with_order(3)
					.with_message(format!(
						"Add {} here to pass by reference pointer.",
						"`&`".fg(c)
					))
					.with_color(c),
			)
			.with_label(
				location_of_declaration
					.label()
					.with_message(format!(
						"Parameter '{}' has type {}.",
						parameter_name.fg(b),
						show_type(parameter_type).fg(b)
					))
					.with_color(b),
			)
			.finish(),

			Error::IndexTypeMismatch {
				argument_type,
				index_type,
				location,
			} => Report::build(
				ReportKind::Error,
				&location.source_filename,
				location.span.start,
			)
			.with_code(format!("E{}", self.code()))
			.with_message("Mismatched types")
			.with_label(
				location
					.label()
					.with_message(format!(
						"Argument has type {}, expected {}.",
						show_type(argument_type).fg(a),
						show_type(index_type).fg(c)
					))
					.with_color(a),
			)
			.finish(),

			Error::NotAnArray {
				current_type,
				location,
				previous,
			} => Report::build(
				ReportKind::Error,
				&location.source_filename,
				location.span.start,
			)
			.with_code(format!("E{}", self.code()))
			.with_message("Conflicting types")
			.with_label(
				location
					.label()
					.with_message(format!(
						"Cannot index into value of non-array type {}.",
						show_type(current_type).fg(a)
					))
					.with_color(a),
			)
			.with_label(
				previous
					.label()
					.with_message("Type previously determined here.")
					.with_color(b),
			)
			.finish(),

			Error::NotAnArrayWithLength {
				current_type,
				location,
				previous,
			} => Report::build(
				ReportKind::Error,
				&location.source_filename,
				location.span.start,
			)
			.with_code(format!("E{}", self.code()))
			.with_message("Conflicting types")
			.with_label(
				location
					.label()
					.with_message(format!(
						"Cannot take length of value of non-array type {}.",
						show_type(current_type).fg(a)
					))
					.with_color(a),
			)
			.with_label(
				previous
					.label()
					.with_message("Type previously determined here.")
					.with_color(b),
			)
			.finish(),

			Error::NotAStructure {
				current_type,
				location,
				previous,
			} => Report::build(
				ReportKind::Error,
				&location.source_filename,
				location.span.start,
			)
			.with_code(format!("E{}", self.code()))
			.with_message("Conflicting types")
			.with_label(
				location
					.label()
					.with_message(format!(
						"Cannot access member of non-structure type {}.",
						show_type(current_type).fg(a)
					))
					.with_color(a),
			)
			.with_label(
				previous
					.label()
					.with_message("Type previously determined here.")
					.with_color(b),
			)
			.finish(),

			Error::TooFewArguments {
				location,
				location_of_declaration,
			} => Report::build(
				ReportKind::Error,
				&location.source_filename,
				location.span.start,
			)
			.with_code(format!("E{}", self.code()))
			.with_message("Too few arguments")
			.with_label(
				location
					.label()
					.with_message("Too few arguments in function call.")
					.with_color(a),
			)
			.with_label(
				location_of_declaration
					.label()
					.with_message("Function declared here.")
					.with_color(b),
			)
			.finish(),

			Error::TooManyArguments {
				location,
				location_of_declaration,
			} => Report::build(
				ReportKind::Error,
				&location.source_filename,
				location.span.start,
			)
			.with_code(format!("E{}", self.code()))
			.with_message("Too many arguments")
			.with_label(
				location
					.label()
					.with_message("Too many arguments in function call.")
					.with_color(a),
			)
			.with_label(
				location_of_declaration
					.label()
					.with_message("Function declared here.")
					.with_color(b),
			)
			.finish(),

			Error::UnsupportedInConstContext { location } => Report::build(
				ReportKind::Error,
				&location.source_filename,
				location.span.start,
			)
			.with_code(format!("E{}", self.code()))
			.with_message("Unsupported expression in constant expression")
			.with_label(
				location
					.label()
					.with_message(
						"This expression is not supported in a constant \
						 expression.",
					)
					.with_color(a),
			)
			.finish(),

			Error::FunctionInConstContext { location } => Report::build(
				ReportKind::Error,
				&location.source_filename,
				location.span.start,
			)
			.with_code(format!("E{}", self.code()))
			.with_message("Function in constant expression")
			.with_label(
				location
					.label()
					.with_message(
						"Function calls cannot occur in a constant expression.",
					)
					.with_color(a),
			)
			.finish(),

			Error::CannotCopyArray { location } => Report::build(
				ReportKind::Error,
				&location.source_filename,
				location.span.start,
			)
			.with_code(format!("E{}", self.code()))
			.with_message("Cannot copy array")
			.with_label(
				location
					.label()
					.with_message("Cannot copy this array.")
					.with_color(a),
			)
			.finish(),

			Error::CannotCopySlice { location } => Report::build(
				ReportKind::Error,
				&location.source_filename,
				location.span.start,
			)
			.with_code(format!("E{}", self.code()))
			.with_message("Cannot copy slice")
			.with_label(
				location
					.label()
					.with_message("Cannot copy this slice.")
					.with_color(a),
			)
			.finish(),

			Error::CannotCopyStruct { location } => Report::build(
				ReportKind::Error,
				&location.source_filename,
				location.span.start,
			)
			.with_code(format!("E{}", self.code()))
			.with_message("Cannot copy struct")
			.with_label(
				location
					.label()
					.with_message("Cannot copy this struct.")
					.with_color(a),
			)
			.finish(),

			Error::NotMutable {
				location,
				location_of_declaration,
			} => Report::build(
				ReportKind::Error,
				&location.source_filename,
				location.span.start,
			)
			.with_code(format!("E{}", self.code()))
			.with_message("Illegal mutation")
			.with_label(
				location
					.label()
					.with_message("Cannot mutate this value.")
					.with_color(a),
			)
			.with_label(
				location_of_declaration
					.label()
					.with_message("This value is not mutable.")
					.with_color(b),
			)
			.finish(),

			Error::AddressOfTemporaryAddress {
				location,
				location_of_declaration,
			} => Report::build(
				ReportKind::Error,
				&location.source_filename,
				location.span.start,
			)
			.with_code(format!("E{}", self.code()))
			.with_message("Address of temporary address")
			.with_label(
				location
					.label()
					.with_message("Cannot take address of temporary address.")
					.with_color(a),
			)
			.with_label(
				location_of_declaration
					.label()
					.with_message(format!(
						"This value is not of type {}.",
						"`&&_`".fg(b)
					))
					.with_color(b),
			)
			.finish(),

			Error::MissingBraces { location } => Report::build(
				ReportKind::Error,
				&location.source_filename,
				location.span.start,
			)
			.with_code(format!("E{}", self.code()))
			.with_message("Missing braces")
			.with_label(
				location
					.label()
					.with_message("Add braces around this statement.")
					.with_color(a),
			)
			.with_note(format!(
				"Braces around conditional branches can only be omitted for \
				 {} statements.",
				"`goto`".fg(a)
			))
			.finish(),

			Error::NonFinalLoopStatement {
				location,
				location_of_block,
			} => Report::build(
				ReportKind::Error,
				&location.source_filename,
				location.span.start,
			)
			.with_code(format!("E{}", self.code()))
			.with_message("Misplaced loop statement")
			.with_label(
				location
					.label()
					.with_message("This is not the final statement...")
					.with_color(a),
			)
			.with_label(
				location_of_block
					.label()
					.with_message("...of this block.")
					.with_color(b),
			)
			.with_note(format!(
				"The {} statement must always be the final statement of a \
				 block.",
				"`loop`".fg(a)
			))
			.finish(),

			Error::MisplacedLoopStatement { location } => Report::build(
				ReportKind::Error,
				&location.source_filename,
				location.span.start,
			)
			.with_code(format!("E{}", self.code()))
			.with_message("Misplaced loop statement")
			.with_label(
				location
					.label()
					.with_message("This is not allowed here.")
					.with_color(a),
			)
			.with_note(format!(
				"The {} statement must always be the final statement of a \
				 block.",
				"`loop`".fg(a)
			))
			.finish(),

			Error::VariableDeclarationMayBeSkipped {
				label,
				location,
				location_of_goto,
				location_of_label,
			} => Report::build(
				ReportKind::Error,
				&location.source_filename,
				location.span.start,
			)
			.with_code(format!("E{}", self.code()))
			.with_message("Variable declaration may be skipped")
			.with_label(
				location
					.label()
					.with_message("...may skip this variable declaration.")
					.with_color(a),
			)
			.with_label(
				location_of_goto
					.label()
					.with_message(format!(
						"A jump from this {} statement to '{}'...",
						"`goto`".fg(b),
						label.fg(c)
					))
					.with_color(b),
			)
			.with_label(
				location_of_label
					.label()
					.with_message(
						"After this label, the existence of the declared \
						 variable is dubious.",
					)
					.with_color(c),
			)
			.finish(),

			Error::AmbiguousType { location } => Report::build(
				ReportKind::Error,
				&location.source_filename,
				location.span.start,
			)
			.with_code(format!("E{}", self.code()))
			.with_message("Ambiguous type")
			.with_label(
				location
					.label()
					.with_message("Failed to infer type of expression.")
					.with_color(a),
			)
			.finish(),

			Error::AmbiguousTypeOfDeclaration { location } => Report::build(
				ReportKind::Error,
				&location.source_filename,
				location.span.start,
			)
			.with_code(format!("E{}", self.code()))
			.with_message("Ambiguous type")
			.with_label(
				location
					.label()
					.with_message("Failed to infer type of variable.")
					.with_color(a),
			)
			.with_note("Consider adding a type to this declaration.")
			.finish(),

			Error::AmbiguousTypeOfNakedIntegerLiteral {
				suggested_type,
				location,
			} => Report::build(
				ReportKind::Error,
				&location.source_filename,
				location.span.start,
			)
			.with_code(format!("E{}", self.code()))
			.with_message("Ambiguous type")
			.with_label(
				location
					.label()
					.with_message("Failed to infer type of integer literal.")
					.with_color(a),
			)
			.with_label(
				location
					.label_after_end()
					.with_message(format!(
						"Consider adding a type suffix like {}.",
						show_type(suggested_type).fg(b)
					))
					.with_color(b),
			)
			.finish(),

			Error::AmbiguousTypeOfArrayLiteral { location } => Report::build(
				ReportKind::Error,
				&location.source_filename,
				location.span.start,
			)
			.with_code(format!("E{}", self.code()))
			.with_message("Ambiguous type")
			.with_label(
				location
					.label()
					.with_message(
						"Failed to infer element type of array literal.",
					)
					.with_color(a),
			)
			.finish(),

			Error::AmbiguousTypeOfStringLiteral { location } => Report::build(
				ReportKind::Error,
				&location.source_filename,
				location.span.start,
			)
			.with_code(format!("E{}", self.code()))
			.with_message("Ambiguous type")
			.with_label(
				location
					.label()
					.with_message("Failed to infer type of string literal.")
					.with_color(a),
			)
			.finish(),

			Error::MismatchedOperandTypes {
				type_of_left,
				type_of_right,
				location_of_op,
				location_of_left,
				location_of_right,
			} => Report::build(
				ReportKind::Error,
				&location_of_op.source_filename,
				location_of_op.span.start,
			)
			.with_code(format!("E{}", self.code()))
			.with_message("Mismatched operand types")
			.with_label(
				location_of_left
					.label()
					.with_message(format!(
						"This has type {}.",
						show_type(type_of_left).fg(a)
					))
					.with_color(a),
			)
			.with_label(
				location_of_right
					.label()
					.with_message(format!(
						"This has type {}.",
						show_type(type_of_right).fg(b)
					))
					.with_color(b),
			)
			.with_label(
				location_of_op
					.label()
					.with_message(
						"This operator expects operands with equal types.",
					)
					.with_color(c),
			)
			.finish(),

			Error::InvalidOperandType {
				value_type,
				possible_types,
				location_of_op,
				location_of_operand,
			} => Report::build(
				ReportKind::Error,
				&location_of_op.source_filename,
				location_of_op.span.start,
			)
			.with_code(format!("E{}", self.code()))
			.with_message("Invalid operand type")
			.with_label(
				location_of_operand
					.label()
					.with_message(format!(
						"This has type {}.",
						show_type(value_type).fg(a)
					))
					.with_color(a),
			)
			.with_label(
				location_of_op
					.label()
					.with_order(2)
					.with_message(format!(
						"Expected {}.",
						show_possible_types(possible_types, b)
					))
					.with_color(b),
			)
			.finish(),

			Error::InvalidPrimitiveCast {
				value_type,
				coerced_type,
				possible_value_types,
				possible_coerced_types,
				location_of_operand,
				location_of_type,
			} => Report::build(
				ReportKind::Error,
				&location_of_operand.source_filename,
				location_of_operand.span.start,
			)
			.with_code(format!("E{}", self.code()))
			.with_message("Invalid primitive cast")
			.with_label(
				location_of_operand
					.label()
					.with_message(format!(
						"This has type {}.",
						show_type(value_type).fg(a)
					))
					.with_color(a),
			)
			.with_label(
				location_of_type
					.label()
					.with_message(format!(
						"Cannot cast {} into {}.",
						show_type(value_type).fg(a),
						show_type(coerced_type).fg(b),
					))
					.with_color(b),
			)
			.with_note(note_for_possible_casts(
				value_type,
				coerced_type,
				possible_value_types,
				possible_coerced_types,
				a,
				b,
			))
			.finish(),
		}
	}
}

#[cfg_attr(coverage, no_coverage)]
#[cfg(not(tarpaulin_include))]
fn note_for_possible_casts(
	value_type: &ValueType,
	coerced_type: &ValueType,
	possible_value_types: &[OperandValueType],
	possible_coerced_types: &[OperandValueType],
	a: ariadne::Color,
	b: ariadne::Color,
) -> String
{
	if possible_coerced_types.is_empty()
	{
		"Can only cast between primitive types.".to_string()
	}
	else if possible_value_types.is_empty()
	{
		format!(
			"Can cast {} into {}.",
			show_type(value_type).fg(a),
			show_possible_types(possible_coerced_types, b),
		)
	}
	else
	{
		format!(
			"Can cast {} into {}, or {} into {}.",
			show_type(value_type).fg(a),
			show_possible_types(possible_coerced_types, b),
			show_possible_types(possible_value_types, a),
			show_type(coerced_type).fg(b),
		)
	}
}

#[cfg_attr(coverage, no_coverage)]
#[cfg(not(tarpaulin_include))]
fn show_possible_types(
	possible_types: &[OperandValueType],
	color: ariadne::Color,
) -> String
{
	let list: Vec<String> = possible_types
		.iter()
		.map(|x| match x
		{
			OperandValueType::ValueType(value_type) =>
			{
				format!("{}", show_type(value_type).fg(color))
			}
			OperandValueType::Pointer => format!("{}", "`&_`".fg(color)),
		})
		.collect();
	match list.len()
	{
		0 => "something else".to_string(),
		1 => list.into_iter().next().unwrap(),
		_ => format!("one of {}", list.join(", ")),
	}
}

#[cfg_attr(coverage, no_coverage)]
#[cfg(not(tarpaulin_include))]
fn show_type(value_type: &ValueType) -> String
{
	format!("`{}`", show_type_inner(value_type))
}

#[cfg_attr(coverage, no_coverage)]
#[cfg(not(tarpaulin_include))]
fn show_type_inner(value_type: &ValueType) -> String
{
	match value_type
	{
		ValueType::Void => "void".to_string(),
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
		ValueType::Array {
			element_type,
			length,
		} => format!("[{}]{}", length, show_type_inner(element_type)),
		ValueType::Slice { element_type } =>
		{
			format!("[:]{}", show_type_inner(element_type))
		}
		ValueType::EndlessArray { element_type } =>
		{
			format!("[...]{}", show_type_inner(element_type))
		}
		ValueType::Arraylike { element_type } =>
		{
			format!("[]{}", show_type_inner(element_type))
		}
		ValueType::Struct { identifier } => identifier.name.to_string(),
		ValueType::Word {
			identifier,
			size_in_bytes: _,
		} => identifier.name.to_string(),
		ValueType::UnresolvedStructOrWord { identifier } => match identifier
		{
			Some(identifier) => identifier.name.to_string(),
			None => "struct".to_string(),
		},
		ValueType::Pointer { deref_type } =>
		{
			format!("&{}", show_type_inner(deref_type))
		}
		ValueType::View { deref_type } =>
		{
			format!("({})", show_type_inner(deref_type))
		}
	}
}
