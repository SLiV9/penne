//
// Part of penne
// Copyright (c) 2020 Sander in 't Veld
// License: MIT
//

//! The optional linting stage analyzes the common AST and generates warnings
//! for error-free code that may have unintended behavior at runtime.

use crate::common::*;

pub use crate::error::Error as Lint;

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
				depth: _,
				location_of_declaration: _,
				location_of_type: _,
			} => (),
			Declaration::Function {
				name: _,
				parameters: _,
				body: Ok(body),
				return_type: _,
				flags: _,
				location_of_declaration: _,
				location_of_return_type: _,
			} => body.lint(linter),
			Declaration::Function {
				name: _,
				parameters: _,
				body: Err(_poison),
				return_type: _,
				flags: _,
				location_of_declaration: _,
				location_of_return_type: _,
			} => (),
			Declaration::FunctionHead {
				name: _,
				parameters: _,
				return_type: _,
				flags: _,
				location_of_declaration: _,
				location_of_return_type: _,
			} => (),
			Declaration::Structure {
				name: _,
				members: _,
				structural_type: _,
				flags: _,
				depth: _,
				location_of_declaration: _,
			} => (),
			Declaration::Import { .. } => (),
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
		if let Some((first, others)) = self.statements.split_first()
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
