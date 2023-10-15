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
				resolved::Expression::StringLiteral {
					bytes: "[{}:{}]\n".as_bytes().to_vec(),
				},
				resolve(Builtin::File, location, Vec::new()),
				resolve(Builtin::Line, location, Vec::new()),
			];
			resolve(Builtin::Eprint, location, arguments)
		}
		Builtin::Dbg =>
		{
			assert_eq!(arguments.len(), 2);
			let value = arguments[0];
			let value_source_code_string_literal = arguments[1];
			let arguments = vec![
				resolved::Expression::StringLiteral {
					bytes: "[{}:{}] {} = {}\n".as_bytes().to_vec(),
				},
				resolve(Builtin::File, location, Vec::new()),
				resolve(Builtin::Line, location, Vec::new()),
				value_source_code_string_literal,
				value,
			];
			let eprint = resolve(Builtin::Eprint, location, arguments);
			let statements =
				vec![resolved::Statement::EvaluateAndDiscard { value: eprint }];
			let value = Box::new(value);
			resolved::Expression::InlineBlock { statements, value }
		}
		Builtin::Panic =>
		{
			let eprint = resolve(Builtin::Eprint, location, arguments);
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

fn write(
	fd: Fd,
	location: &Location,
	arguments: Vec<resolved::Expression>,
) -> resolved::Expression
{
	let buffer = resolve(Builtin::Format, location, arguments);
	let write = resolved::GeneratorBuiltin::Write {
		fd,
		buffer: Box::new(buffer),
	};
	resolved::Expression::Builtin(write)
}
