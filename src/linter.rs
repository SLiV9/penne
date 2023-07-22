//
// Part of penne
// Copyright (c) 2020 Sander in 't Veld
// License: MIT
//

//! The optional linting stage analyzes the common AST and generates warnings
//! for error-free code that may have unintended behavior at runtime.

use crate::common::*;

pub use crate::error::Error as Lint;

/// The Linter keeps track of context during the linting stage.
#[derive(Default)]
pub struct Linter
{
	lints: Vec<Lint>,
	is_naked_branch: Option<NakedBranch>,
	is_first_statement_of_branch: Option<Branch>,
}

impl Linter
{
	/// Generates lints for a declaration.
	/// The lints are stored in the Linter
	/// and can be retrieved by turning the Linter into `Vec<Lint>`.
	pub fn lint(&mut self, declaration: &Declaration)
	{
		declaration.lint(self)
	}
}

impl From<Linter> for Vec<Lint>
{
	fn from(linter: Linter) -> Vec<Lint>
	{
		linter.lints
	}
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

impl<T: Lintable> Lintable for Option<T>
{
	fn lint(&self, linter: &mut Linter)
	{
		if let Some(thing) = self
		{
			thing.lint(linter);
		}
	}
}

impl Lintable for Declaration
{
	fn lint(&self, linter: &mut Linter)
	{
		match self
		{
			Declaration::Constant {
				name: _,
				value,
				value_type: _,
				flags: _,
				depth: _,
				location_of_declaration: _,
				location_of_type: _,
			} => value.lint(linter),
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
			Statement::Declaration {
				name: _,
				value,
				value_type: _,
				location: _,
			} =>
			{
				value.lint(linter);
			}
			Statement::Assignment {
				reference,
				value,
				location: _,
			} =>
			{
				reference.lint(linter);
				value.lint(linter);
			}
			Statement::MethodCall { name: _, arguments } =>
			{
				for argument in arguments
				{
					argument.lint(linter);
				}
			}
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

impl Lintable for Expression
{
	fn lint(&self, linter: &mut Linter)
	{
		match self
		{
			Expression::Binary {
				op: _,
				left,
				right,
				location: _,
				location_of_op: _,
			} =>
			{
				left.lint(linter);
				right.lint(linter);
			}
			Expression::Unary {
				op: _,
				expression,
				location: _,
				location_of_op: _,
			} =>
			{
				expression.lint(linter);
			}
			Expression::BooleanLiteral {
				value: _,
				location: _,
			} => (),
			Expression::SignedIntegerLiteral {
				value,
				value_type: Some(Ok(value_type)),
				location,
			} =>
			{
				let value: i128 = *value;
				let is_truncated = if value < 0
				{
					value < value_type.min_i128()
				}
				else
				{
					value as u128 > value_type.max_u128()
				};
				if is_truncated
				{
					let lint = Lint::IntegerLiteralTruncation {
						value_type: value_type.clone(),
						location_of_literal: location.clone(),
					};
					linter.lints.push(lint);
				}
			}
			Expression::SignedIntegerLiteral {
				value: _,
				value_type: _,
				location: _,
			} => (),
			Expression::BitIntegerLiteral {
				value,
				value_type: Some(Ok(value_type)),
				location,
			} =>
			{
				let value: u128 = *value;
				if value > value_type.max_u128()
				{
					let lint = Lint::IntegerLiteralTruncation {
						value_type: value_type.clone(),
						location_of_literal: location.clone(),
					};
					linter.lints.push(lint);
				}
			}
			Expression::BitIntegerLiteral {
				value: _,
				value_type: _,
				location: _,
			} => (),
			Expression::ArrayLiteral {
				array,
				element_type: _,
			} =>
			{
				for element in &array.elements
				{
					element.lint(linter);
				}
			}
			Expression::StringLiteral {
				bytes: _,
				location: _,
			} => (),
			Expression::Structural {
				members,
				structural_type: _,
				location: _,
			} =>
			{
				for member in members
				{
					member.expression.lint(linter);
				}
			}
			Expression::Parenthesized { inner, location: _ } =>
			{
				inner.lint(linter);
			}
			Expression::Autocoerce {
				expression,
				coerced_type: _,
			} =>
			{
				expression.lint(linter);
			}
			Expression::PrimitiveCast {
				expression,
				coerced_type: _,
				location: _,
				location_of_type: _,
			} =>
			{
				expression.lint(linter);
			}
			Expression::Deref {
				reference,
				deref_type: _,
			} =>
			{
				reference.lint(linter);
			}
			Expression::LengthOfArray {
				reference,
				location: _,
			} =>
			{
				reference.lint(linter);
			}
			Expression::SizeOf { .. } => (),
			Expression::FunctionCall {
				name: _,
				arguments,
				return_type: _,
			} =>
			{
				for argument in arguments
				{
					argument.lint(linter);
				}
			}
			Expression::Poison(_) => (),
		}
	}
}

impl Lintable for Reference
{
	fn lint(&self, linter: &mut Linter)
	{
		for step in &self.steps
		{
			step.lint(linter);
		}
	}
}

impl Lintable for ReferenceStep
{
	fn lint(&self, linter: &mut Linter)
	{
		match self
		{
			ReferenceStep::Element {
				argument,
				is_endless: _,
			} =>
			{
				argument.lint(linter);
			}
			ReferenceStep::Member {
				member: _,
				offset: _,
			} => (),
			ReferenceStep::Autodeslice { offset: _ } => (),
			ReferenceStep::Autoderef => (),
			ReferenceStep::Autoview => (),
		}
	}
}
