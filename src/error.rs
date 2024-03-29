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

use ariadne::{Fmt, Label, Report, ReportKind};

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

	pub fn combined_with(self, mut other: Errors) -> Errors
	{
		let Errors { mut errors } = self;
		errors.append(&mut other.errors);
		Errors { errors }
	}

	pub fn sorted(self) -> Errors
	{
		let Errors { mut errors } = self;
		errors.sort_by(|a, b| {
			a.location()
				.comparison_key()
				.cmp(&b.location().comparison_key())
		});
		Errors { errors }
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
	TypeLacksKnownSize
	{
		queried_type: ValueType,
		location_of_query: Location,
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
		name_of_structure: String,
		location_of_declaration: Location,
		location_of_member: Location,
	},
	CyclicalStructureWithConstant
	{
		name_of_structure: String,
		name_of_constant: String,
		location_of_declaration: Location,
		location_of_member: Location,
		location_of_constant: Location,
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
	NotACompileTimeConstant
	{
		name: String,
		location: Location,
		location_of_declaration: Location,
	},
	UnresolvedImport
	{
		filename: String,
		location: Location,
	},
	UnresolvedImportWithHint
	{
		filename: String,
		location: Location,
		hinted_package_name: String,
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
	ExcessAddressInAssignment
	{
		name: String,
		previous_type: ValueType,
		location: Location,
		previous: Location,
	},
	MismatchedAddressInAssignment
	{
		name: String,
		assigned_type: ValueType,
		assignee_type: ValueType,
		declared_type: ValueType,
		location: Location,
		location_of_assignee: Location,
		location_of_declaration: Location,
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
		location_of_unaddressed: Location,
		type_of_unaddressed: ValueType,
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
		name: String,
		label: String,
		location: Location,
		location_of_declaration: Location,
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
	AmbiguousTypeOfPointerDeclaration
	{
		suggested_type: ValueType,
		location: Location,
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
	InvalidPrimitiveConversion
	{
		value_type: ValueType,
		coerced_type: ValueType,
		possible_value_types: Vec<OperandValueType>,
		possible_coerced_types: Vec<OperandValueType>,
		is_valid_bit_cast: bool,
		location_of_operand: Location,
		location_of_type: Location,
	},
	InvalidBitCast
	{
		value_type: ValueType,
		coerced_type: ValueType,
		location_of_operand: Location,
		location_of_type: Location,
	},

	// Lints:
	IntegerLiteralTruncation
	{
		value_type: ValueType,
		location_of_literal: Location,
	},
	LoopAsFirstStatement
	{
		location_of_loop: Location,
		location_of_condition: Location,
		location_of_block: Location,
	},
	UnreachableCode
	{
		location: Location
	},
}

impl Error
{
	pub fn code(&self) -> u16
	{
		match self
		{
			Error::UnexpectedEndOfFile { .. } => 100,
			Error::Lexical { error, .. } => match error
			{
				lexer::Error::UnexpectedZeroByteFile => 101,
				lexer::Error::UnexpectedCharacter => 110,
				lexer::Error::InvalidIntegerLength => 140,
				lexer::Error::InvalidIntegerTypeSuffix => 141,
				lexer::Error::MissingClosingQuote => 160,
				lexer::Error::UnexpectedTrailingBackslash => 161,
				lexer::Error::InvalidEscapeSequence => 162,
				lexer::Error::InvalidCharLiteral => 163,
			},
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
			Error::TypeLacksKnownSize { .. } => 359,
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
			Error::CyclicalStructureWithConstant { .. } => 416,
			Error::DuplicateDeclarationLabel { .. } => 420,
			Error::DuplicateDeclarationFunction { .. } => 421,
			Error::DuplicateDeclarationVariable { .. } => 422,
			Error::DuplicateDeclarationConstant { .. } => 423,
			Error::DuplicateDeclarationParameter { .. } => 424,
			Error::DuplicateDeclarationStructure { .. } => 425,
			Error::DuplicateDeclarationMember { .. } => 426,
			Error::NotACompileTimeConstant { .. } => 433,
			Error::UnresolvedImport { .. } => 470,
			Error::UnresolvedImportWithHint { .. } => 477,
			Error::VariableDeclarationMayBeSkipped { .. } => 482,
			Error::ConflictingTypes { .. } => 500,
			Error::NotAnArray { .. } => 501,
			Error::NotAnArrayWithLength { .. } => 502,
			Error::IndexTypeMismatch { .. } => 503,
			Error::ConflictingTypesInAssignment { .. } => 504,
			Error::NotAStructure { .. } => 505,
			Error::ExcessAddressInAssignment { .. } => 506,
			Error::MismatchedAddressInAssignment { .. } => 507,
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
			Error::InvalidPrimitiveConversion { .. } => 552,
			Error::InvalidBitCast { .. } => 553,
			Error::AmbiguousType { .. } => 580,
			Error::AmbiguousTypeOfDeclaration { .. } => 581,
			Error::AmbiguousTypeOfNakedIntegerLiteral { .. } => 582,
			Error::AmbiguousTypeOfArrayLiteral { .. } => 583,
			Error::AmbiguousTypeOfStringLiteral { .. } => 584,
			Error::AmbiguousTypeOfPointerDeclaration { .. } => 585,
			Error::NonFinalLoopStatement { .. } => 800,
			Error::MisplacedLoopStatement { .. } => 801,
			Error::MissingBraces { .. } => 840,

			// Lints:
			Error::IntegerLiteralTruncation { .. } => 1142,
			Error::LoopAsFirstStatement { .. } => 1800,
			Error::UnreachableCode { .. } => 1880,
		}
	}

	#[cfg_attr(coverage, no_coverage)]
	#[cfg(not(tarpaulin_include))]
	fn location(&self) -> &Location
	{
		match self
		{
			Error::UnexpectedEndOfFile { last_location, .. } => &last_location,
			Error::Lexical { location, .. } => &location,
			Error::UnexpectedToken { location, .. } => &location,
			Error::UnexpectedSemicolonAfterIdentifier { location, .. } =>
			{
				&location
			}
			Error::UnexpectedSemicolonAfterReturnValue { location, .. } =>
			{
				&location
			}
			Error::MissingReturnType {
				location_of_return_value,
				..
			} => &location_of_return_value,
			Error::MissingAmbiguousReturnType {
				location_of_return_value,
				..
			} => &location_of_return_value,
			Error::AmbiguousReturnValue {
				location_of_return_value,
				..
			} => &location_of_return_value,
			Error::ConflictingReturnValue {
				location_of_return_value,
				..
			} => &location_of_return_value,
			Error::MissingReturnValue { location, .. } => &location,
			Error::MissingReturnValueAfterStatement { location, .. } =>
			{
				&location
			}
			Error::MissingConstantType { location, .. } => &location,
			Error::MissingParameterType { location, .. } => &location,
			Error::MissingMemberType { location, .. } => &location,
			Error::IllegalType { location, .. } => &location,
			Error::IllegalReturnType { location, .. } => &location,
			Error::IllegalVariableType { location, .. } => &location,
			Error::IllegalConstantType { location, .. } => &location,
			Error::IllegalParameterType { location, .. } => &location,
			Error::IllegalMemberType { location, .. } => &location,
			Error::TypeNotAllowedInExtern {
				location_of_type, ..
			} => &location_of_type,
			Error::TypeLacksKnownSize {
				location_of_query, ..
			} => &location_of_query,
			Error::UnsupportedInConstContext { location, .. } => &location,
			Error::FunctionInConstContext { location, .. } => &location,
			Error::WordSizeMismatch {
				location_of_keyword,
				..
			} => &location_of_keyword,
			Error::MaximumParseDepthExceeded { location, .. } => &location,
			Error::UndefinedLabel { location, .. } => &location,
			Error::UndefinedFunction { location, .. } => &location,
			Error::UndefinedVariable { location, .. } => &location,
			Error::UndefinedStructure { location, .. } => &location,
			Error::UndefinedMember { location, .. } => &location,
			Error::CyclicalConstant { location, .. } => &location,
			Error::CyclicalStructure {
				location_of_declaration,
				..
			} => &location_of_declaration,
			Error::CyclicalStructureWithConstant {
				location_of_declaration,
				..
			} => &location_of_declaration,
			Error::DuplicateDeclarationLabel { location, .. } => &location,
			Error::DuplicateDeclarationFunction { location, .. } => &location,
			Error::DuplicateDeclarationVariable { location, .. } => &location,
			Error::DuplicateDeclarationConstant { location, .. } => &location,
			Error::DuplicateDeclarationParameter { location, .. } => &location,
			Error::DuplicateDeclarationStructure { location, .. } => &location,
			Error::DuplicateDeclarationMember { location, .. } => &location,
			Error::NotACompileTimeConstant { location, .. } => &location,
			Error::UnresolvedImport { location, .. } => &location,
			Error::UnresolvedImportWithHint { location, .. } => &location,
			Error::ConflictingTypes { location, .. } => &location,
			Error::NotAnArray { location, .. } => &location,
			Error::NotAnArrayWithLength { location, .. } => &location,
			Error::IndexTypeMismatch { location, .. } => &location,
			Error::ConflictingTypesInAssignment { location, .. } => &location,
			Error::ExcessAddressInAssignment { location, .. } => &location,
			Error::MismatchedAddressInAssignment { location, .. } => &location,
			Error::NotAStructure { location, .. } => &location,
			Error::TooFewArguments { location, .. } => &location,
			Error::TooManyArguments { location, .. } => &location,
			Error::ArgumentTypeMismatch { location, .. } => &location,
			Error::ArgumentMissingAddress { location, .. } => &location,
			Error::NotMutable { location, .. } => &location,
			Error::CannotCopyArray { location, .. } => &location,
			Error::CannotCopySlice { location, .. } => &location,
			Error::CannotCopyStruct { location, .. } => &location,
			Error::AddressOfTemporaryAddress { location, .. } => &location,
			Error::InvalidOperandType { location_of_op, .. } => &location_of_op,
			Error::MismatchedOperandTypes { location_of_op, .. } =>
			{
				&location_of_op
			}
			Error::InvalidPrimitiveConversion {
				location_of_operand,
				..
			} => &location_of_operand,
			Error::InvalidBitCast {
				location_of_operand,
				..
			} => &location_of_operand,
			Error::AmbiguousType { location, .. } => &location,
			Error::AmbiguousTypeOfDeclaration { location, .. } => &location,
			Error::AmbiguousTypeOfPointerDeclaration { location, .. } =>
			{
				&location
			}
			Error::AmbiguousTypeOfNakedIntegerLiteral { location, .. } =>
			{
				&location
			}
			Error::AmbiguousTypeOfArrayLiteral { location, .. } => &location,
			Error::AmbiguousTypeOfStringLiteral { location, .. } => &location,
			Error::NonFinalLoopStatement { location, .. } => &location,
			Error::MisplacedLoopStatement { location, .. } => &location,
			Error::VariableDeclarationMayBeSkipped { location, .. } =>
			{
				&location
			}
			Error::MissingBraces { location, .. } => &location,

			// Lints:
			Error::IntegerLiteralTruncation {
				location_of_literal,
				..
			} => &location_of_literal,
			Error::LoopAsFirstStatement {
				location_of_loop, ..
			} => &location_of_loop,
			Error::UnreachableCode { location } => &location,
		}
	}

	#[cfg_attr(coverage, no_coverage)]
	#[cfg(not(tarpaulin_include))]
	pub fn build_report(
		&self,
		config: Config,
	) -> Report<(String, std::ops::Range<usize>)>
	{
		let location = self.location();
		let code = self.code();
		let (kind, letter) = match code
		{
			100..=999 => (ReportKind::Error, 'E'),
			1000..=1999 => (ReportKind::Warning, 'L'),
			2000..=2999 => (ReportKind::Advice, 'L'),
			_ => (ReportKind::Error, 'E'),
		};
		let report =
			Report::build(kind, &location.source_filename, location.span.end)
				.with_config(config.into())
				.with_code(format!("{}{}", letter, code));
		let report = write(report, config.into(), self);
		report.finish()
	}
}

#[derive(Debug, Clone, Copy)]
pub struct Config
{
	inner: ariadne::Config,
	with_color: bool,
}

#[cfg_attr(coverage, no_coverage)]
#[cfg(not(tarpaulin_include))]
impl From<Config> for ariadne::Config
{
	fn from(config: Config) -> ariadne::Config
	{
		config.inner
	}
}

#[cfg_attr(coverage, no_coverage)]
#[cfg(not(tarpaulin_include))]
impl From<ariadne::Config> for Config
{
	fn from(inner: ariadne::Config) -> Config
	{
		Config {
			inner,
			with_color: false,
		}
	}
}

#[cfg_attr(coverage, no_coverage)]
#[cfg(not(tarpaulin_include))]
impl Config
{
	pub fn with_color(mut self, with_color: bool) -> Self
	{
		self.with_color = with_color;
		self
	}

	fn filter(&self, color: ariadne::Color) -> Option<ariadne::Color>
	{
		Some(color).filter(|_| self.with_color)
	}
}

const PRIMARY: ariadne::Color = ariadne::Color::Yellow;
const SECONDARY: ariadne::Color = ariadne::Color::Cyan;
const TERTIARY: ariadne::Color = ariadne::Color::Magenta;

struct Colors
{
	primary: Option<ariadne::Color>,
	secondary: Option<ariadne::Color>,
	tertiary: Option<ariadne::Color>,
}

impl From<Config> for Colors
{
	#[cfg_attr(coverage, no_coverage)]
	#[cfg(not(tarpaulin_include))]
	fn from(config: Config) -> Colors
	{
		Colors {
			primary: config.filter(PRIMARY),
			secondary: config.filter(SECONDARY),
			tertiary: config.filter(TERTIARY),
		}
	}
}

#[cfg_attr(coverage, no_coverage)]
#[cfg(not(tarpaulin_include))]
fn write<'a>(
	report: ariadne::ReportBuilder<'a, (String, std::ops::Range<usize>)>,
	colors: Colors,
	error: &Error,
) -> ariadne::ReportBuilder<'a, (String, std::ops::Range<usize>)>
{
	match error
	{
		Error::UnexpectedEndOfFile {
			last_location,
			expectation,
		} => report
			.with_message("Unexpected end of file")
			.with_label(
				last_location
					.label_after_end()
					.with_message(expectation)
					.with_order(1)
					.with_color(PRIMARY),
			)
			.with_label(
				last_location
					.label()
					.with_message("Expected more after this.")
					.with_order(2)
					.with_color(SECONDARY),
			),

		Error::Lexical {
			error: lexer::Error::UnexpectedZeroByteFile,
			expectation,
			location,
		} => report
			.with_message("Unexpected end of file")
			.with_label(
				location
					.label()
					.with_message(expectation)
					.with_color(PRIMARY),
			)
			.with_note("Provided file contains zero bytes of data."),

		Error::Lexical {
			error: lexer::Error::UnexpectedCharacter,
			expectation,
			location,
		} => report.with_message("Unexpected character").with_label(
			location
				.label()
				.with_message(expectation)
				.with_color(PRIMARY),
		),

		Error::Lexical {
			error: lexer::Error::InvalidIntegerLength,
			expectation,
			location,
		} => report
			.with_message("Invalid integer literal")
			.with_label(
				location
					.label()
					.with_message(expectation)
					.with_color(PRIMARY),
			)
			.with_note("Integer literals can be at most 128 bits."),

		Error::Lexical {
			error: lexer::Error::InvalidIntegerTypeSuffix,
			expectation,
			location,
		} => report.with_message("Invalid integer literal").with_label(
			location
				.label()
				.with_message(expectation)
				.with_color(PRIMARY),
		),

		Error::Lexical {
			error: lexer::Error::InvalidEscapeSequence,
			expectation: _,
			location,
		} => report.with_message("Invalid escape sequence").with_label(
			location
				.label()
				.with_message("Invalid character escape sequence")
				.with_color(PRIMARY),
		),

		Error::Lexical {
			error: lexer::Error::UnexpectedTrailingBackslash,
			expectation: _,
			location,
		} => report
			.with_message("Unexpected trailing backslash")
			.with_label(
				location
					.label()
					.with_message("Unexpected trailing backslash")
					.with_color(PRIMARY),
			)
			.with_note(
				"To continue a string across multiple lines, close it and \
				 then reopen it on the next line.",
			),

		Error::Lexical {
			error: lexer::Error::MissingClosingQuote,
			expectation,
			location,
		} => report
			.with_message("Missing closing quote")
			.with_label(
				location
					.label()
					.with_message(expectation)
					.with_color(PRIMARY),
			)
			.with_note(
				"To continue a string across multiple lines, close it and \
				 then reopen it on the next line.",
			),

		Error::Lexical {
			error: lexer::Error::InvalidCharLiteral,
			expectation: _,
			location,
		} => report
			.with_message("Invalid character literal")
			.with_label(
				location
					.label()
					.with_message("Invalid character literal")
					.with_color(PRIMARY),
			)
			.with_note(
				"Character literals must contain exactly one ASCII character.",
			),

		Error::UnexpectedToken {
			expectation,
			location,
		} => report.with_message("Unexpected token").with_label(
			location
				.label()
				.with_message(expectation)
				.with_color(PRIMARY),
		),

		Error::UnexpectedSemicolonAfterIdentifier { location, after } => report
			.with_message("Unexpected token")
			.with_label(
				location
					.label()
					.with_message("Unexpected semicolon")
					.with_color(PRIMARY),
			)
			.with_label(
				after
					.label()
					.with_order(2)
					.with_message("This is not a valid statement.")
					.with_color(SECONDARY),
			),

		Error::UnexpectedSemicolonAfterReturnValue { location, after } =>
		{
			report
				.with_message("Unexpected semicolon after return value")
				.with_label(
					location
						.label()
						.with_message("Unexpected semicolon")
						.with_color(PRIMARY),
				)
				.with_label(
					after
						.label()
						.with_order(2)
						.with_message(
							"This is the return value of this function.",
						)
						.with_color(SECONDARY),
				)
		}

		Error::MaximumParseDepthExceeded { location } => report
			.with_message("Maximum parse depth exceeded")
			.with_label(
				location
					.label()
					.with_message("This is too complex to parse.")
					.with_color(PRIMARY),
			),

		Error::MissingConstantType { location } =>
		{
			report.with_message("Missing type").with_label(
				location
					.label()
					.with_message("Constants need an explicit type.")
					.with_color(PRIMARY),
			)
		}

		Error::MissingParameterType { location } =>
		{
			report.with_message("Missing type").with_label(
				location
					.label()
					.with_message("Function parameters need an explicit type.")
					.with_color(PRIMARY),
			)
		}

		Error::MissingMemberType { location } =>
		{
			report.with_message("Missing type").with_label(
				location
					.label()
					.with_message("Structure members need an explicit type.")
					.with_color(PRIMARY),
			)
		}

		Error::IllegalType {
			value_type,
			location,
		} => report.with_message("Invalid type").with_label(
			location
				.label()
				.with_message(format!(
					"The type {} is invalid.",
					show_type(value_type, colors.primary)
				))
				.with_color(PRIMARY),
		),

		Error::IllegalVariableType {
			value_type,
			location,
		} => report.with_message("Invalid variable type").with_label(
			location
				.label()
				.with_message(format!(
					"A value of type {} cannot be assigned to a variable.",
					show_type(value_type, colors.primary)
				))
				.with_color(PRIMARY),
		),

		Error::IllegalParameterType {
			value_type,
			location,
		} => report.with_message("Invalid parameter type").with_label(
			location
				.label()
				.with_message(format!(
					"The type {} is not allowed as a parameter.",
					show_type(value_type, colors.primary)
				))
				.with_color(PRIMARY),
		),

		Error::IllegalReturnType {
			value_type,
			location,
		} => report.with_message("Invalid return type").with_label(
			location
				.label()
				.with_message(format!(
					"The type {} is not allowed as a return value.",
					show_type(value_type, colors.primary)
				))
				.with_color(PRIMARY),
		),

		Error::IllegalMemberType {
			value_type,
			in_word: false,
			location,
		} => report.with_message("Invalid member type").with_label(
			location
				.label()
				.with_message(format!(
					"The type {} is not allowed as a member of a struct.",
					show_type(value_type, colors.primary),
				))
				.with_color(PRIMARY),
		),

		Error::IllegalMemberType {
			value_type,
			in_word: true,
			location,
		} => report.with_message("Invalid member type").with_label(
			location
				.label()
				.with_message(format!(
					"The type {} is not allowed as a member of a word.",
					show_type(value_type, colors.primary),
				))
				.with_color(PRIMARY),
		),

		Error::IllegalConstantType {
			value_type,
			location,
		} => report.with_message("Invalid constant type").with_label(
			location
				.label()
				.with_message(format!(
					"A value of type {} cannot be assigned to a constant.",
					show_type(value_type, colors.primary)
				))
				.with_color(PRIMARY),
		),

		Error::TypeNotAllowedInExtern {
			value_type,
			location_of_type,
			location_of_declaration,
		} => report
			.with_message("Invalid external type")
			.with_label(
				location_of_type
					.label()
					.with_message(format!(
						"The type {} is not allowed in external declarations.",
						show_type(value_type, colors.primary)
					))
					.with_color(PRIMARY),
			)
			.with_label(
				location_of_declaration
					.label()
					.with_message("Declaration marked external here.")
					.with_order(2)
					.with_color(SECONDARY),
			),

		Error::TypeLacksKnownSize {
			queried_type,
			location_of_query,
		} => report
			.with_message("Invalid type size operation")
			.with_label(
				location_of_query
					.label()
					.with_message(format!(
						"The type {} does not have a known size.",
						show_type(queried_type, colors.primary)
					))
					.with_color(PRIMARY),
			),

		Error::WordSizeMismatch {
			inferred_size_in_bits,
			declared_size_in_bits,
			location_of_identifier,
			location_of_keyword,
		} => report
			.with_message("Conflicting type sizes")
			.with_label(
				location_of_identifier
					.label()
					.with_message(format!(
						"The size of this structure is {} bits.",
						inferred_size_in_bits.fg(colors.primary)
					))
					.with_color(PRIMARY),
			)
			.with_label(
				location_of_keyword
					.label()
					.with_message(format!(
						"The structure is declared with `{}{}` and therefore \
						 has a maximum size of {} bits.",
						"word".fg(colors.secondary),
						declared_size_in_bits.fg(colors.secondary),
						declared_size_in_bits.fg(colors.secondary),
					))
					.with_color(SECONDARY),
			),

		Error::MissingReturnType {
			inferred_type,
			location_of_return_value,
			location_of_declaration,
		} => report
			.with_message("Missing return type")
			.with_label(
				location_of_return_value
					.label()
					.with_message(format!(
						"Value of type {} returned here.",
						show_type(inferred_type, colors.primary)
					))
					.with_color(PRIMARY),
			)
			.with_label(
				location_of_declaration
					.label()
					.with_message("Function declared here.")
					.with_color(SECONDARY),
			),

		Error::MissingAmbiguousReturnType {
			location_of_return_value,
			location_of_declaration,
		} => report
			.with_message("Missing return type")
			.with_label(
				location_of_return_value
					.label()
					.with_message("Value of indiscernible type returned here.")
					.with_color(PRIMARY),
			)
			.with_label(
				location_of_declaration
					.label()
					.with_message("Function declared here.")
					.with_color(SECONDARY),
			),

		Error::AmbiguousReturnValue {
			declared_type,
			location_of_return_value,
			location_of_declaration,
		} => report
			.with_message("Ambiguous return value")
			.with_label(
				location_of_return_value
					.label()
					.with_message("Failed to infer the type of this value.")
					.with_color(PRIMARY),
			)
			.with_label(
				location_of_declaration
					.label()
					.with_message(format!(
						"Expected {} based on this declaration.",
						show_type(declared_type, colors.secondary)
					))
					.with_color(SECONDARY),
			),

		Error::ConflictingReturnValue {
			inferred_type,
			declared_type,
			location_of_return_value,
			location_of_declaration,
		} => report
			.with_message("Conflicting return value")
			.with_label(
				location_of_return_value
					.label()
					.with_message(format!(
						"Value of type {} returned here.",
						show_type(inferred_type, colors.primary)
					))
					.with_color(PRIMARY),
			)
			.with_label(
				location_of_declaration
					.label()
					.with_message(format!(
						"Expected {} based on this declaration.",
						show_type(declared_type, colors.secondary)
					))
					.with_color(SECONDARY),
			),

		Error::MissingReturnValue {
			declared_type,
			location,
			location_of_declaration,
		} => report
			.with_message("Missing return value")
			.with_label(
				location
					.label()
					.with_message("No return value.")
					.with_color(PRIMARY),
			)
			.with_label(
				location_of_declaration
					.label()
					.with_message(format!(
						"Expected {} based on this declaration.",
						show_type(declared_type, colors.secondary)
					))
					.with_color(SECONDARY),
			),

		Error::MissingReturnValueAfterStatement { after, .. } => report
			.with_message("Missing return value")
			.with_label(
				after
					.label_after_end()
					.with_message("Expected return value...")
					.with_color(PRIMARY),
			)
			.with_label(
				after
					.label()
					.with_message(format!(
						"...after this `{}` label.",
						"return".fg(colors.secondary)
					))
					.with_order(2)
					.with_color(SECONDARY),
			),

		Error::DuplicateDeclarationVariable {
			name,
			location,
			previous,
		} => report
			.with_message("Duplicate variable")
			.with_label(
				location
					.label()
					.with_message(format!(
						"A variable named '{}' is already defined in this \
						 scope.",
						name.fg(colors.primary)
					))
					.with_color(PRIMARY),
			)
			.with_label(
				previous
					.label()
					.with_message("Previously defined here.")
					.with_color(SECONDARY),
			),

		Error::DuplicateDeclarationConstant {
			name,
			location,
			previous,
		} => report
			.with_message("Duplicate constant")
			.with_label(
				location
					.label()
					.with_message(format!(
						"The constant '{}' is already defined.",
						name.fg(colors.primary)
					))
					.with_color(PRIMARY),
			)
			.with_label(
				previous
					.label()
					.with_message("Previously defined here.")
					.with_color(SECONDARY),
			),

		Error::DuplicateDeclarationFunction {
			name,
			location,
			previous,
		} => report
			.with_message("Duplicate function")
			.with_label(
				location
					.label()
					.with_message(format!(
						"A function named '{}' is already defined.",
						name.fg(colors.primary)
					))
					.with_color(PRIMARY),
			)
			.with_label(
				previous
					.label()
					.with_message("Previously defined here.")
					.with_color(SECONDARY),
			),

		Error::DuplicateDeclarationParameter {
			name,
			location,
			previous,
		} => report
			.with_message("Duplicate parameter")
			.with_label(
				location
					.label()
					.with_message(format!(
						"A parameter named '{}' is already defined for this \
						 function.",
						name.fg(colors.primary)
					))
					.with_color(PRIMARY),
			)
			.with_label(
				previous
					.label()
					.with_message("Previously defined here.")
					.with_color(SECONDARY),
			),

		Error::DuplicateDeclarationStructure {
			name,
			location,
			previous,
		} => report
			.with_message("Duplicate structure")
			.with_label(
				location
					.label()
					.with_message(format!(
						"A struct or word named '{}' is already defined.",
						name.fg(colors.primary)
					))
					.with_color(PRIMARY),
			)
			.with_label(
				previous
					.label()
					.with_message("Previously defined here.")
					.with_color(SECONDARY),
			),

		Error::DuplicateDeclarationMember {
			name,
			location,
			previous,
		} => report
			.with_message("Duplicate member")
			.with_label(
				location
					.label()
					.with_message(format!(
						"A member named '{}' is already defined in this \
						 structure.",
						name.fg(colors.primary)
					))
					.with_color(PRIMARY),
			)
			.with_label(
				previous
					.label()
					.with_message("Previously defined here.")
					.with_color(SECONDARY),
			),

		Error::DuplicateDeclarationLabel {
			name,
			location,
			previous,
		} => report
			.with_message("Duplicate label")
			.with_label(
				location
					.label()
					.with_message(format!(
						"The label '{}' is already defined in this scope.",
						name.fg(colors.primary)
					))
					.with_color(PRIMARY),
			)
			.with_label(
				previous
					.label()
					.with_message("Previously defined here.")
					.with_color(SECONDARY),
			),

		Error::CyclicalConstant { name, location } =>
		{
			report.with_message("Cyclical definition").with_label(
				location
					.label()
					.with_message(format!(
						"The definition of constant '{}' depends on itself.",
						name.fg(colors.primary)
					))
					.with_color(PRIMARY),
			)
		}

		Error::CyclicalStructure {
			name_of_structure,
			location_of_declaration,
			location_of_member,
		} => report
			.with_message("Cyclical definition")
			.with_label(
				location_of_declaration
					.label()
					.with_message(format!(
						"Cannot determine size of structure '{}' that \
						 contains itself.",
						name_of_structure.fg(colors.primary)
					))
					.with_color(PRIMARY),
			)
			.with_label(
				location_of_member
					.label()
					.with_message(format!(
						"This member contains '{}'.",
						name_of_structure.fg(colors.primary)
					))
					.with_color(SECONDARY),
			),

		Error::CyclicalStructureWithConstant {
			name_of_structure,
			name_of_constant,
			location_of_declaration,
			location_of_member,
			location_of_constant,
		} => report
			.with_message("Cyclical definition")
			.with_label(
				location_of_constant
					.label()
					.with_message(format!(
						"This constant depends on the size of '{}'.",
						name_of_structure.fg(colors.primary)
					))
					.with_color(SECONDARY),
			)
			.with_label(
				location_of_declaration
					.label()
					.with_message(format!(
						"Cannot determine size of structure '{}'...",
						name_of_structure.fg(colors.primary)
					))
					.with_color(PRIMARY),
			)
			.with_label(
				location_of_member
					.label()
					.with_message(format!(
						"...because this member depends on '{}'.",
						name_of_constant.fg(colors.secondary)
					))
					.with_color(TERTIARY),
			),

		Error::UndefinedVariable { name, location } =>
		{
			report.with_message("Undefined reference").with_label(
				location
					.label()
					.with_message(format!(
						"There is no variable, parameter or constant named \
						 '{}' in this scope.",
						name.fg(colors.primary)
					))
					.with_color(PRIMARY),
			)
		}

		Error::UndefinedFunction { name, location } =>
		{
			report.with_message("Undefined reference").with_label(
				location
					.label()
					.with_message(format!(
						"Reference to undefined function named '{}'.",
						name.fg(colors.primary)
					))
					.with_color(PRIMARY),
			)
		}

		Error::UndefinedStructure { name, location } =>
		{
			report.with_message("Undefined reference").with_label(
				location
					.label()
					.with_message(format!(
						"Reference to undefined struct or word named '{}'.",
						name.fg(colors.primary)
					))
					.with_color(PRIMARY),
			)
		}

		Error::UndefinedMember {
			name_of_member,
			name_of_structure,
			location,
			location_of_declaration,
		} => report
			.with_message("Undefined member")
			.with_label(
				location
					.label()
					.with_message(format!(
						"No member '{}' exists for structure '{}'.",
						name_of_member.fg(colors.primary),
						name_of_structure.fg(colors.secondary),
					))
					.with_color(PRIMARY),
			)
			.with_label(
				location_of_declaration
					.label()
					.with_message(format!(
						"The structure '{}' is defined here.",
						name_of_structure.fg(colors.secondary),
					))
					.with_color(SECONDARY),
			),

		Error::UndefinedLabel { name, location } =>
		{
			report.with_message("Undefined label").with_label(
				location
					.label()
					.with_message(format!(
						"Reference to undefined label '{}'.",
						name.fg(colors.primary)
					))
					.with_color(PRIMARY),
			)
		}

		Error::NotACompileTimeConstant {
			name,
			location,
			location_of_declaration,
		} => report
			.with_message("Value not known at compile-time")
			.with_label(
				location
					.label()
					.with_message(
						"Only compile-time constants are allowed here.",
					)
					.with_color(PRIMARY),
			)
			.with_label(
				location_of_declaration
					.label()
					.with_message(format!(
						"The value of the variable '{}' declared here is not \
						 known at compile-time.",
						name.fg(colors.secondary),
					))
					.with_color(SECONDARY),
			),

		Error::UnresolvedImport { filename, location } => report
			.with_message("Unresolved import")
			.with_label(
				location
					.label()
					.with_message(format!(
						"Reference to unknown source '{}'.",
						filename.fg(colors.primary)
					))
					.with_color(PRIMARY),
			)
			.with_note(
				"All source files need to be added as compiler arguments.",
			),

		Error::UnresolvedImportWithHint {
			filename,
			location,
			hinted_package_name,
		} => report
			.with_message("Unresolved import")
			.with_label(
				location
					.label()
					.with_message(format!(
						"Reference to unknown source '{}'.",
						filename.fg(colors.primary)
					))
					.with_color(PRIMARY),
			)
			.with_note(format!(
				"Add '{}' as a compiler argument to include it.",
				hinted_package_name.fg(colors.primary)
			)),

		Error::ConflictingTypes {
			name,
			current_type,
			previous_type,
			location,
			previous,
		} => report
			.with_message("Conflicting types")
			.with_label(
				location
					.label()
					.with_message(format!(
						"'{}' has type {}.",
						name.fg(colors.primary),
						show_type(current_type, colors.primary)
					))
					.with_color(PRIMARY),
			)
			.with_label(
				previous
					.label()
					.with_message(format!(
						"Previously determined to be {}.",
						show_type(previous_type, colors.secondary)
					))
					.with_color(SECONDARY),
			),

		Error::ConflictingTypesInAssignment {
			name,
			current_type,
			previous_type,
			location,
			previous,
		} => report
			.with_message("Conflicting types")
			.with_label(
				location
					.label()
					.with_message(format!(
						"Cannot assign this expression of type {}.",
						show_type(current_type, colors.primary)
					))
					.with_color(PRIMARY),
			)
			.with_label(
				previous
					.label()
					.with_message(format!(
						"'{}' has type {}.",
						name.fg(colors.secondary),
						show_type(previous_type, colors.secondary)
					))
					.with_color(SECONDARY),
			),

		Error::ExcessAddressInAssignment {
			name,
			previous_type,
			location,
			previous,
		} => report
			.with_message("Conflicting types")
			.with_label(
				location
					.label()
					.with_message("Cannot assign to immutable address.")
					.with_color(PRIMARY),
			)
			.with_label(
				previous
					.label()
					.with_message(format!(
						"'{}' has type {}.",
						name.fg(colors.secondary),
						show_type(previous_type, colors.secondary)
					))
					.with_color(SECONDARY),
			),

		Error::MismatchedAddressInAssignment {
			name,
			assigned_type,
			assignee_type,
			declared_type,
			location,
			location_of_assignee,
			location_of_declaration,
		} => report
			.with_message("Conflicting types")
			.with_label(
				location
					.label()
					.with_order(1)
					.with_message(format!(
						"Cannot assign this expression of type {}.",
						show_type(assigned_type, colors.primary)
					))
					.with_color(PRIMARY),
			)
			.with_label(
				location_of_assignee
					.label()
					.with_order(2)
					.with_message(format!(
						"The assignee has type {}.",
						show_type(assignee_type, colors.secondary)
					))
					.with_color(SECONDARY),
			)
			.with_label(
				location_of_declaration
					.label()
					.with_message(format!(
						"'{}' has type {}.",
						name.fg(colors.tertiary),
						show_type(declared_type, colors.tertiary)
					))
					.with_color(TERTIARY),
			),

		Error::ArgumentTypeMismatch {
			parameter_name,
			argument_type,
			parameter_type,
			location,
			location_of_declaration,
		} => report
			.with_message("Mismatched types")
			.with_label(
				location
					.label()
					.with_message(format!(
						"Argument has type {}.",
						show_type(argument_type, colors.primary)
					))
					.with_color(PRIMARY),
			)
			.with_label(
				location_of_declaration
					.label()
					.with_message(format!(
						"Parameter '{}' has type {}.",
						parameter_name.fg(colors.secondary),
						show_type(parameter_type, colors.secondary)
					))
					.with_color(SECONDARY),
			),

		Error::ArgumentMissingAddress {
			parameter_name,
			argument_type,
			parameter_type,
			location,
			location_of_declaration,
		} => report
			.with_message("Mismatched types")
			.with_label(
				location
					.label()
					.with_message(format!(
						"Argument has type {}.",
						show_type(argument_type, colors.primary)
					))
					.with_color(PRIMARY),
			)
			.with_label(
				location
					.label_before_start()
					.with_order(3)
					.with_message(format!(
						"Add `{}` here to pass by reference pointer.",
						"&".fg(colors.tertiary)
					))
					.with_color(TERTIARY),
			)
			.with_label(
				location_of_declaration
					.label()
					.with_message(format!(
						"Parameter '{}' has type {}.",
						parameter_name.fg(colors.secondary),
						show_type(parameter_type, colors.secondary)
					))
					.with_color(SECONDARY),
			),

		Error::IndexTypeMismatch {
			argument_type,
			index_type,
			location,
		} => report.with_message("Mismatched types").with_label(
			location
				.label()
				.with_message(format!(
					"Argument has type {}, expected {}.",
					show_type(argument_type, colors.primary),
					show_type(index_type, colors.tertiary)
				))
				.with_color(PRIMARY),
		),

		Error::NotAnArray {
			current_type,
			location,
			previous,
		} => report
			.with_message("Conflicting types")
			.with_label(
				location
					.label()
					.with_message(format!(
						"Cannot index into value of non-array type {}.",
						show_type(current_type, colors.primary)
					))
					.with_color(PRIMARY),
			)
			.with_label(
				previous
					.label()
					.with_message("Type previously determined here.")
					.with_color(SECONDARY),
			),

		Error::NotAnArrayWithLength {
			current_type,
			location,
			previous,
		} => report
			.with_message("Conflicting types")
			.with_label(
				location
					.label()
					.with_message(format!(
						"Cannot take length of value of non-array type {}.",
						show_type(current_type, colors.primary)
					))
					.with_color(PRIMARY),
			)
			.with_label(
				previous
					.label()
					.with_message("Type previously determined here.")
					.with_color(SECONDARY),
			),

		Error::NotAStructure {
			current_type,
			location,
			previous,
		} => report
			.with_message("Conflicting types")
			.with_label(
				location
					.label()
					.with_message(format!(
						"Cannot access member of non-structure type {}.",
						show_type(current_type, colors.primary)
					))
					.with_color(PRIMARY),
			)
			.with_label(
				previous
					.label()
					.with_message("Type previously determined here.")
					.with_color(SECONDARY),
			),

		Error::TooFewArguments {
			location,
			location_of_declaration,
		} => report
			.with_message("Too few arguments")
			.with_label(
				location
					.label()
					.with_message("Too few arguments in function call.")
					.with_color(PRIMARY),
			)
			.with_label(
				location_of_declaration
					.label()
					.with_message("Function declared here.")
					.with_color(SECONDARY),
			),

		Error::TooManyArguments {
			location,
			location_of_declaration,
		} => report
			.with_message("Too many arguments")
			.with_label(
				location
					.label()
					.with_message("Too many arguments in function call.")
					.with_color(PRIMARY),
			)
			.with_label(
				location_of_declaration
					.label()
					.with_message("Function declared here.")
					.with_color(SECONDARY),
			),

		Error::UnsupportedInConstContext { location } => report
			.with_message("Unsupported expression in constant expression")
			.with_label(
				location
					.label()
					.with_message(
						"This expression is not supported in a constant \
						 expression.",
					)
					.with_color(PRIMARY),
			),

		Error::FunctionInConstContext { location } => report
			.with_message("Function in constant expression")
			.with_label(
				location
					.label()
					.with_message(
						"Function calls cannot occur in a constant expression.",
					)
					.with_color(PRIMARY),
			),

		Error::CannotCopyArray { location } =>
		{
			report.with_message("Cannot copy array").with_label(
				location
					.label()
					.with_message("Cannot copy this array.")
					.with_color(PRIMARY),
			)
		}

		Error::CannotCopySlice { location } =>
		{
			report.with_message("Cannot copy array view").with_label(
				location
					.label()
					.with_message("Cannot copy this array view.")
					.with_color(PRIMARY),
			)
		}

		Error::CannotCopyStruct { location } =>
		{
			report.with_message("Cannot copy struct").with_label(
				location
					.label()
					.with_message("Cannot copy this struct.")
					.with_color(PRIMARY),
			)
		}

		Error::NotMutable {
			location,
			location_of_declaration,
		} => report
			.with_message("Illegal mutation")
			.with_label(
				location
					.label()
					.with_message("Cannot mutate this value.")
					.with_color(PRIMARY),
			)
			.with_label(
				location_of_declaration
					.label()
					.with_message("This value is not mutable.")
					.with_color(SECONDARY),
			),

		Error::AddressOfTemporaryAddress {
			location,
			location_of_unaddressed,
			type_of_unaddressed,
		} => report
			.with_message("Address of temporary address")
			.with_label(
				location
					.label()
					.with_message("Cannot take address of temporary address.")
					.with_color(PRIMARY),
			)
			.with_label(
				location_of_unaddressed
					.label()
					.with_message(format!(
						"This reference has type {}.",
						show_type(type_of_unaddressed, colors.secondary),
					))
					.with_color(SECONDARY),
			),

		Error::MissingBraces { location } => report
			.with_message("Missing braces")
			.with_label(
				location
					.label()
					.with_message("Add braces around this statement.")
					.with_color(PRIMARY),
			)
			.with_note(format!(
				"Braces around conditional branches can only be omitted for \
				 `{}` statements.",
				"goto".fg(colors.primary)
			)),

		Error::NonFinalLoopStatement {
			location,
			location_of_block,
		} => report
			.with_message("Misplaced loop statement")
			.with_label(
				location
					.label()
					.with_message("This is not the final statement...")
					.with_color(PRIMARY),
			)
			.with_label(
				location_of_block
					.label()
					.with_message("...of this block.")
					.with_color(SECONDARY),
			)
			.with_note(format!(
				"The `{}` statement must always be the final statement of a \
				 block.",
				"loop".fg(colors.primary)
			)),

		Error::MisplacedLoopStatement { location } => report
			.with_message("Misplaced loop statement")
			.with_label(
				location
					.label()
					.with_message("This is not allowed here.")
					.with_color(PRIMARY),
			)
			.with_note(format!(
				"The `{}` statement must always be the final statement of a \
				 block.",
				"loop".fg(colors.primary)
			)),

		Error::VariableDeclarationMayBeSkipped {
			name,
			label,
			location,
			location_of_declaration,
			location_of_goto,
			location_of_label,
		} => report
			.with_message("Variable declaration may be skipped")
			.with_label(
				location
					.label()
					.with_message("The variable is referenced here.")
					.with_color(PRIMARY),
			)
			.with_label(
				location_of_declaration
					.label()
					.with_message(format!(
						"...may skip the declaration of the variable '{}'.",
						name.fg(colors.primary)
					))
					.with_color(PRIMARY),
			)
			.with_label(
				location_of_goto
					.label()
					.with_message(format!(
						"A jump from this `{}` statement to '{}'...",
						"goto".fg(colors.secondary),
						label.fg(colors.tertiary)
					))
					.with_color(SECONDARY),
			)
			.with_label(
				location_of_label
					.label()
					.with_message(format!(
						"After this label, the existence of '{}' is dubious.",
						name.fg(colors.primary)
					))
					.with_color(TERTIARY),
			),

		Error::AmbiguousType { location } =>
		{
			report.with_message("Ambiguous type").with_label(
				location
					.label()
					.with_message("Failed to infer type of expression.")
					.with_color(PRIMARY),
			)
		}

		Error::AmbiguousTypeOfDeclaration { location } => report
			.with_message("Ambiguous type")
			.with_label(
				location
					.label()
					.with_message("Failed to infer type of variable.")
					.with_color(PRIMARY),
			)
			.with_note("Consider adding a type to this declaration."),

		Error::AmbiguousTypeOfPointerDeclaration {
			location,
			suggested_type,
		} => report
			.with_message("Ambiguous type")
			.with_label(
				location
					.label()
					.with_message("Failed to infer type of variable.")
					.with_color(PRIMARY),
			)
			.with_label(
				location
					.label_after_end()
					.with_message(format!(
						"Consider adding {} here.",
						show_colon_and_type(suggested_type, colors.secondary)
					))
					.with_color(SECONDARY),
			)
			.with_note("Pointer variables require an explicit type."),

		Error::AmbiguousTypeOfNakedIntegerLiteral {
			suggested_type,
			location,
		} => report
			.with_message("Ambiguous type")
			.with_label(
				location
					.label()
					.with_message("Failed to infer type of integer literal.")
					.with_color(PRIMARY),
			)
			.with_label(
				location
					.label_after_end()
					.with_message(format!(
						"Consider adding a type suffix like {}.",
						show_type(suggested_type, colors.secondary)
					))
					.with_color(SECONDARY),
			),

		Error::AmbiguousTypeOfArrayLiteral { location } =>
		{
			report.with_message("Ambiguous type").with_label(
				location
					.label()
					.with_message(
						"Failed to infer element type of array literal.",
					)
					.with_color(PRIMARY),
			)
		}

		Error::AmbiguousTypeOfStringLiteral { location } =>
		{
			report.with_message("Ambiguous type").with_label(
				location
					.label()
					.with_message("Failed to infer type of string literal.")
					.with_color(PRIMARY),
			)
		}

		Error::MismatchedOperandTypes {
			type_of_left,
			type_of_right,
			location_of_op,
			location_of_left,
			location_of_right,
		} => report
			.with_message("Mismatched operand types")
			.with_label(
				location_of_left
					.label()
					.with_message(format!(
						"This has type {}.",
						show_type(type_of_left, colors.primary)
					))
					.with_color(PRIMARY),
			)
			.with_label(
				location_of_right
					.label()
					.with_message(format!(
						"This has type {}.",
						show_type(type_of_right, colors.secondary)
					))
					.with_color(SECONDARY),
			)
			.with_label(
				location_of_op
					.label()
					.with_message(
						"This operator expects operands with equal types.",
					)
					.with_color(TERTIARY),
			),

		Error::InvalidOperandType {
			value_type,
			possible_types,
			location_of_op,
			location_of_operand,
		} => report
			.with_message("Invalid operand type")
			.with_label(
				location_of_operand
					.label()
					.with_message(format!(
						"This has type {}.",
						show_type(value_type, colors.primary)
					))
					.with_color(PRIMARY),
			)
			.with_label(
				location_of_op
					.label()
					.with_order(2)
					.with_message(format!(
						"Expected {}.",
						show_possible_types(possible_types, colors.secondary)
					))
					.with_color(SECONDARY),
			),

		Error::InvalidPrimitiveConversion {
			value_type,
			coerced_type,
			possible_value_types,
			possible_coerced_types,
			is_valid_bit_cast,
			location_of_operand,
			location_of_type,
		} => report
			.with_message("Invalid primitive conversion")
			.with_label(
				location_of_operand
					.label()
					.with_message(format!(
						"This has type {}.",
						show_type(value_type, colors.primary)
					))
					.with_color(PRIMARY),
			)
			.with_label(
				location_of_type
					.label()
					.with_message(format!(
						"Cannot convert {} into {}.",
						show_type(value_type, colors.primary),
						show_type(coerced_type, colors.secondary),
					))
					.with_color(SECONDARY),
			)
			.with_note(note_for_possible_conversions(
				value_type,
				coerced_type,
				possible_value_types,
				possible_coerced_types,
				*is_valid_bit_cast,
				colors,
			)),

		Error::InvalidBitCast {
			value_type,
			coerced_type,
			location_of_operand,
			location_of_type,
		} => report
			.with_message("Invalid bitcast")
			.with_label(
				location_of_operand
					.label()
					.with_message(format!(
						"This has type {}.",
						show_type(value_type, colors.primary)
					))
					.with_color(PRIMARY),
			)
			.with_label(
				location_of_type
					.label()
					.with_message(format!(
						"Cannot bitcast {} as {}.",
						show_type(value_type, colors.primary),
						show_type(coerced_type, colors.secondary),
					))
					.with_color(SECONDARY),
			),

		// Lints:
		Error::LoopAsFirstStatement {
			location_of_loop,
			location_of_condition,
			location_of_block,
		} => report
			.with_message("Conditional infinite loop")
			.with_label(
				location_of_loop
					.label()
					.with_message(
						"...this loop statement will cause an infinite loop...",
					)
					.with_color(PRIMARY),
			)
			.with_label(
				location_of_block
					.label()
					.with_message("...because it belongs to this block.")
					.with_color(TERTIARY),
			)
			.with_label(
				location_of_condition
					.label()
					.with_message("If this condition is met...")
					.with_color(SECONDARY),
			)
			.with_note(format!(
				"Perhaps use `{}` instead to jump to a `{}` elsewhere. To \
				 surpress this warning, add a label.",
				"goto".fg(colors.primary),
				"loop".fg(colors.primary)
			)),

		Error::IntegerLiteralTruncation {
			location_of_literal,
			value_type,
		} if is_address_value_type(value_type) => report
			.with_message("Integer literal truncation")
			.with_label(
				location_of_literal
					.label()
					.with_message(format!(
						"This integer literal does not fit {} and will be \
						 truncated.",
						show_type(value_type, colors.primary),
					))
					.with_color(PRIMARY),
			)
			.with_note("Address values can be at most 64 bits."),

		Error::IntegerLiteralTruncation {
			location_of_literal,
			value_type,
		} => report
			.with_message("Integer literal truncation")
			.with_label(
				location_of_literal
					.label()
					.with_message(format!(
						"This integer literal does not fit {} and will be \
						 truncated.",
						show_type(value_type, colors.primary),
					))
					.with_color(PRIMARY),
			),

		Error::UnreachableCode { location } =>
		{
			report.with_message("Unreachable code").with_label(
				location
					.label()
					.with_message("starting here")
					.with_color(PRIMARY),
			)
		}
	}
}

#[cfg_attr(coverage, no_coverage)]
#[cfg(not(tarpaulin_include))]
fn note_for_possible_conversions(
	value_type: &ValueType,
	coerced_type: &ValueType,
	possible_value_types: &[OperandValueType],
	possible_coerced_types: &[OperandValueType],
	is_valid_bit_cast: bool,
	colors: Colors,
) -> String
{
	if is_valid_bit_cast
	{
		format!(
			"Prepend `{}` to bitcast between these types.",
			"cast".fg(colors.primary)
		)
	}
	else if possible_coerced_types.is_empty()
	{
		"Can only convert between primitive types.".to_string()
	}
	else if possible_value_types.is_empty()
	{
		format!(
			"Can convert {} into {}.",
			show_type(value_type, colors.primary),
			show_possible_types(possible_coerced_types, colors.secondary),
		)
	}
	else
	{
		format!(
			"Can convert {} into {}, or {} into {}.",
			show_type(value_type, colors.primary),
			show_possible_types(possible_coerced_types, colors.secondary),
			show_possible_types(possible_value_types, colors.primary),
			show_type(coerced_type, colors.secondary),
		)
	}
}

#[cfg_attr(coverage, no_coverage)]
#[cfg(not(tarpaulin_include))]
fn show_possible_types(
	possible_types: &[OperandValueType],
	color: Option<ariadne::Color>,
) -> String
{
	let list: Vec<String> = possible_types
		.iter()
		.map(|x| match x
		{
			OperandValueType::ValueType(value_type) =>
			{
				format!("{}", show_type(value_type, color))
			}
			OperandValueType::Pointer => format!("`{}`", "&_".fg(color)),
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
fn show_colon_and_type(
	value_type: &ValueType,
	color: Option<ariadne::Color>,
) -> String
{
	format!(
		"`{}{}`",
		": ".fg(color),
		show_type_inner(value_type).fg(color)
	)
}

#[cfg_attr(coverage, no_coverage)]
#[cfg(not(tarpaulin_include))]
fn show_type(value_type: &ValueType, color: Option<ariadne::Color>) -> String
{
	format!("`{}`", show_type_inner(value_type).fg(color))
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
		ValueType::Char8 => "char8".to_string(),
		ValueType::Bool => "bool".to_string(),
		ValueType::Array {
			element_type,
			length,
		} => format!("[{}]{}", length, show_type_inner(element_type)),
		ValueType::ArrayWithNamedLength {
			element_type,
			named_length,
		} => format!(
			"[{}]{}",
			named_length.name.to_string(),
			show_type_inner(element_type)
		),
		ValueType::Slice { element_type } =>
		{
			format!("[:]{}", show_type_inner(element_type))
		}
		ValueType::SlicePointer { element_type } =>
		{
			format!("&[:]{}", show_type_inner(element_type))
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

#[cfg_attr(coverage, no_coverage)]
#[cfg(not(tarpaulin_include))]
impl Location
{
	fn label(&self) -> Label<(String, std::ops::Range<usize>)>
	{
		Label::new((self.source_filename.to_string(), self.span.clone()))
	}

	fn label_before_start(&self) -> Label<(String, std::ops::Range<usize>)>
	{
		let location = Location {
			span: self.span.start..self.span.start,
			..self.clone()
		};
		location.label()
	}

	fn label_after_end(&self) -> Label<(String, std::ops::Range<usize>)>
	{
		let location = Location {
			span: self.span.end..self.span.end,
			..self.clone()
		};
		location.label()
	}
}

#[cfg_attr(coverage, no_coverage)]
#[cfg(not(tarpaulin_include))]
fn is_address_value_type(value_type: &ValueType) -> bool
{
	match value_type
	{
		ValueType::Usize { .. } => true,
		ValueType::Pointer { .. } => true,
		ValueType::View { .. } => true,
		_ => false,
	}
}
