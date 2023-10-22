//
// Part of penne
// Copyright (c) 2023 Sander in 't Veld
// License: MIT
//

//! The Penne compiler supports various built-in functions that cannot be
//! expressed in normal Penne source code, unlike those in the core library).

use crate::common::*;
use crate::resolved;

#[derive(Debug, Clone, Copy)]
pub enum Fd
{
	Stdout,
	Stderr,
}

pub fn resolve(
	builtin: Builtin,
	location: &Location,
	arguments: Vec<resolved::Expression>,
	return_type: resolved::ValueType,
) -> resolved::Expression
{
	match builtin
	{
		Builtin::Abort =>
		{
			resolved::Expression::Builtin(resolved::GeneratorBuiltin::Abort)
		}
		Builtin::Format =>
		{
			resolved::Expression::Builtin(resolved::GeneratorBuiltin::Format {
				arguments,
			})
		}
		Builtin::File => resolved::Expression::StringLiteral {
			bytes: location.source_filename.as_bytes().to_vec(),
		},
		Builtin::Line => resolved::Expression::BitIntegerLiteral {
			value: u128::try_from(location.line_number).unwrap(),
			value_type: resolved::ValueType::Usize,
		},
		Builtin::Print => write(Fd::Stdout, location, arguments),
		Builtin::Eprint => write(Fd::Stderr, location, arguments),
		Builtin::Dbg if arguments.len() == 0 =>
		{
			let arguments = vec![
				string_literal("["),
				file(location),
				string_literal(":"),
				line(location),
				string_literal("]\n"),
			];
			resolve(Builtin::Eprint, location, arguments, return_type)
		}
		Builtin::Dbg =>
		{
			assert_eq!(arguments.len(), 2);
			let mut arguments = arguments.into_iter();
			let value = arguments.next().unwrap();
			let value_source_code_string_literal = arguments.next().unwrap();
			let arguments = vec![
				string_literal("["),
				file(location),
				string_literal(":"),
				line(location),
				string_literal("] "),
				value_source_code_string_literal,
				string_literal(" = "),
				value,
				string_literal("\n"),
			];
			todo!();
			let eprint = resolve(
				Builtin::Eprint,
				location,
				arguments,
				resolved::ValueType::Void,
			);
			let statements =
				vec![resolved::Statement::EvaluateAndDiscard { value: eprint }];
			let value = Box::new(value);
			resolved::Expression::InlineBlock { statements, value }
		}
		Builtin::Panic =>
		{
			let eprint = resolve(
				Builtin::Eprint,
				location,
				arguments,
				resolved::ValueType::Void,
			);
			let statements =
				vec![resolved::Statement::EvaluateAndDiscard { value: eprint }];
			let value = Box::new(resolved::Expression::Builtin(
				resolved::GeneratorBuiltin::Abort,
			));
			resolved::Expression::InlineBlock { statements, value }
		}
		Builtin::IncludeBytes => unreachable!(),
	}
}

fn file(location: &Location) -> resolved::Expression
{
	let str_type = resolved::ValueType::for_string_slice();
	resolve(Builtin::File, location, Vec::new(), str_type)
}

fn line(location: &Location) -> resolved::Expression
{
	resolve(
		Builtin::Line,
		location,
		Vec::new(),
		resolved::ValueType::Usize,
	)
}

fn write(
	fd: Fd,
	location: &Location,
	arguments: Vec<resolved::Expression>,
) -> resolved::Expression
{
	let str_type = resolved::ValueType::for_string_slice();
	let buffer = resolve(Builtin::Format, location, arguments, str_type);
	let write = resolved::GeneratorBuiltin::Write {
		fd,
		buffer: Box::new(buffer),
	};
	resolved::Expression::Builtin(write)
}

fn string_literal(literal: &str) -> resolved::Expression
{
	resolved::Expression::StringLiteral {
		bytes: literal.as_bytes().to_vec(),
	}
}
