//
// Part of penne
// Copyright (c) 2023 Sander in 't Veld
// License: MIT
//

//! The Penne compiler supports various built-in functions that cannot be
//! expressed in normal Penne source code, unlike those in the core library).

use crate::common::Builtin;
use crate::common::Location;
use crate::resolved::GeneratorBuiltin;
use crate::resolved::ValueType;
use crate::resolved::{Expression, Statement};

#[derive(Debug, Clone, Copy)]
pub enum Fd
{
	Stdout,
	Stderr,
}

pub fn resolve(
	builtin: Builtin,
	location: &Location,
	arguments: Vec<Expression>,
	_return_type: ValueType,
) -> Expression
{
	match builtin
	{
		Builtin::Abort => Expression::Builtin(GeneratorBuiltin::Abort),
		Builtin::Format =>
		{
			Expression::Builtin(GeneratorBuiltin::Format { arguments })
		}
		Builtin::File => file(location),
		Builtin::Line => line(location),
		Builtin::Print => write(Fd::Stdout, arguments),
		Builtin::Eprint => write(Fd::Stderr, arguments),
		Builtin::Dbg if arguments.len() == 0 =>
		{
			let arguments = vec![
				string_literal("["),
				file(location),
				string_literal(":"),
				line(location),
				string_literal("]\n"),
			];
			let eprint = write(Fd::Stderr, arguments);
			eprint
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
				// TODO value,
				string_literal("\n"),
			];
			let eprint = write(Fd::Stderr, arguments);
			let statements =
				vec![Statement::EvaluateAndDiscard { value: eprint }];
			let value = Box::new(value);
			Expression::InlineBlock { statements, value }
		}
		Builtin::Panic =>
		{
			let eprint = write(Fd::Stderr, arguments);
			let statements =
				vec![Statement::EvaluateAndDiscard { value: eprint }];
			let value = Box::new(Expression::Builtin(GeneratorBuiltin::Abort));
			Expression::InlineBlock { statements, value }
		}
		Builtin::IncludeBytes => unreachable!(),
	}
}

fn file(location: &Location) -> Expression
{
	Expression::StringLiteral {
		bytes: location.source_filename.as_bytes().to_vec(),
	}
}

fn line(location: &Location) -> Expression
{
	Expression::BitIntegerLiteral {
		value: u128::try_from(location.line_number).unwrap(),
		value_type: ValueType::Usize,
	}
}

fn write(fd: Fd, arguments: Vec<Expression>) -> Expression
{
	let buffer = Expression::Builtin(GeneratorBuiltin::Format { arguments });
	let write = GeneratorBuiltin::Write {
		fd,
		buffer: Box::new(buffer),
	};
	Expression::Builtin(write)
}

fn string_literal(literal: &str) -> Expression
{
	Expression::StringLiteral {
		bytes: literal.as_bytes().to_vec(),
	}
}
