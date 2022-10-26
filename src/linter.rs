//
// Part of penne
// Copyright (c) 2020 Sander in 't Veld
// License: MIT
//

//! The optional linting stage analyzes the common AST and generates warnings
//! for error-free code that may have unintended behavior at runtime.

use crate::common::*;

use ariadne::{Fmt, Report, ReportKind};

pub fn lint(program: &Vec<Declaration>) -> Vec<Lint>
{
	let mut linter = Linter {
		lints: Vec::new(),
		is_naked_branch: None,
		is_first_statement_of_branch: None,
	};
	for declaration in program
	{
		declaration.lint(&mut linter);
	}
	linter.lints
}

#[derive(Debug, Clone)]
pub enum Lint
{
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

impl Lint
{
	pub fn code(&self) -> u16
	{
		match self
		{
			Lint::LoopAsFirstStatement { .. } => 1800,
			Lint::UnreachableCode { .. } => 1880,
		}
	}

	pub fn report(&self) -> Report<(String, std::ops::Range<usize>)>
	{
		let a = ariadne::Color::Yellow;
		let b = ariadne::Color::Cyan;
		let c = ariadne::Color::Magenta;

		match self
		{
			Lint::LoopAsFirstStatement {
				location_of_loop,
				location_of_condition,
				location_of_block,
			} => Report::build(
				ReportKind::Warning,
				&location_of_loop.source_filename,
				location_of_loop.span.start,
			)
			.with_code(format!("L{}", self.code()))
			.with_message("Conditional infinite loop")
			.with_label(
				location_of_loop
					.label()
					.with_message(format!(
						"...this loop statement will cause an infinite loop..."
					))
					.with_color(a),
			)
			.with_label(
				location_of_block
					.label()
					.with_message(format!(
						"...because it belongs to this block."
					))
					.with_color(c),
			)
			.with_label(
				location_of_condition
					.label()
					.with_message(format!("If this condition is met..."))
					.with_color(b),
			)
			.with_note(format!(
				"Perhaps use {} instead. To surpress this warning, add a \
				 label.",
				"`goto`".fg(a)
			))
			.finish(),

			Lint::UnreachableCode { location } => Report::build(
				ReportKind::Advice,
				&location.source_filename,
				location.span.start,
			)
			.with_code(format!("L{}", self.code()))
			.with_message("Unreachable code")
			.with_label(
				location
					.label()
					.with_message(format!("starting here"))
					.with_color(a),
			)
			.finish(),
		}
	}
}

struct Linter
{
	lints: Vec<Lint>,
	is_naked_branch: Option<NakedBranch>,
	is_first_statement_of_branch: Option<Branch>,
}

struct NakedBranch
{
	location_of_condition: Location,
}

struct Branch
{
	location_of_condition: Location,
	location_of_block: Location,
}

trait Lintable
{
	fn lint(&self, linter: &mut Linter);
}

impl Lintable for Declaration
{
	fn lint(&self, linter: &mut Linter)
	{
		match self
		{
			Declaration::Constant {
				name: _,
				value: _,
				value_type: _,
				flags: _,
			} => (),
			Declaration::Function {
				name: _,
				parameters: _,
				body: Ok(body),
				return_type: _,
				flags: _,
			} => body.lint(linter),
			Declaration::Function {
				name: _,
				parameters: _,
				body: Err(_poison),
				return_type: _,
				flags: _,
			} => (),
			Declaration::FunctionHead {
				name: _,
				parameters: _,
				return_type: _,
				flags: _,
			} => (),
			Declaration::PreprocessorDirective { .. } => unreachable!(),
			Declaration::Poison(_) => (),
		}
	}
}

impl Lintable for FunctionBody
{
	fn lint(&self, linter: &mut Linter)
	{
		for statement in &self.statements
		{
			statement.lint(linter);
		}
	}
}

impl Lintable for Block
{
	fn lint(&self, linter: &mut Linter)
	{
		match self.statements.split_first()
		{
			Some((first, others)) =>
			{
				linter.is_first_statement_of_branch =
					match linter.is_naked_branch.take()
					{
						Some(NakedBranch {
							location_of_condition,
						}) => Some(Branch {
							location_of_condition,
							location_of_block: self.location.clone(),
						}),
						None => None,
					};
				first.lint(linter);
				linter.is_first_statement_of_branch = None;
				for statement in others
				{
					statement.lint(linter);
				}
			}
			None => (),
		}
	}
}

impl Lintable for Statement
{
	fn lint(&self, linter: &mut Linter)
	{
		match self
		{
			Statement::Declaration { .. } => (),
			Statement::Assignment { .. } => (),
			Statement::MethodCall { .. } => (),
			Statement::Loop { location } =>
			{
				if let Some(branch) = linter.is_first_statement_of_branch.take()
				{
					let lint = Lint::LoopAsFirstStatement {
						location_of_loop: location.clone(),
						location_of_condition: branch.location_of_condition,
						location_of_block: branch.location_of_block,
					};
					linter.lints.push(lint);
				}
			}
			Statement::Goto { .. } => (),
			Statement::Label { .. } => (),
			Statement::If {
				condition,
				then_branch,
				else_branch,
				location: _,
			} =>
			{
				linter.is_first_statement_of_branch = None;

				linter.is_naked_branch = Some(NakedBranch {
					location_of_condition: condition.location.clone(),
				});
				then_branch.lint(linter);

				if let Some(else_branch) = else_branch
				{
					linter.is_naked_branch = Some(NakedBranch {
						location_of_condition: else_branch
							.location_of_else
							.clone(),
					});
					else_branch.branch.lint(linter);
				}

				linter.is_naked_branch = None;
			}
			Statement::Block(block) => block.lint(linter),
			Statement::Poison(_) => (),
		}
	}
}
