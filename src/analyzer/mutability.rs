//
// Part of penne
// Copyright (c) 2020 Sander in 't Veld
// License: MIT
//

use crate::common::*;
use crate::error::Error;

pub fn analyze(declaration: Declaration, analyzer: &mut Analyzer)
	-> Declaration
{
	declaration.analyze(analyzer)
}

#[derive(Default)]
pub struct Analyzer
{
	variables: std::collections::HashMap<u32, (Identifier, bool)>,
}

impl Analyzer
{
	fn declare_variable(&mut self, identifier: &Identifier, is_mutable: bool)
	{
		self.variables
			.insert(identifier.resolution_id, (identifier.clone(), is_mutable));
	}

	fn use_variable(
		&self,
		identifier: &Poisonable<Identifier>,
		is_mutated: bool,
	) -> Result<(), Poison>
	{
		if let Ok(identifier) = identifier
		{
			if let Some((previous_identifier, is_mutable)) =
				self.variables.get(&identifier.resolution_id)
			{
				if is_mutated && !is_mutable
				{
					let error = Error::NotMutable {
						location: identifier.location.clone(),
						location_of_declaration: previous_identifier
							.location
							.clone(),
					};
					Err(Poison::Error(error))
				}
				else
				{
					Ok(())
				}
			}
			else
			{
				Err(Poison::Poisoned)
			}
		}
		else
		{
			Ok(())
		}
	}
}

trait Analyzable
{
	fn analyze(self, analyzer: &mut Analyzer) -> Self;
}

impl Analyzable for Declaration
{
	fn analyze(self, analyzer: &mut Analyzer) -> Self
	{
		match self
		{
			Declaration::Constant {
				ref name,
				ref value_type,
				..
			} =>
			{
				// If the constant type contains an error, autoderef may have
				// been skipped, which means we do not know if we have interior
				// mutability. Avoid cascading errors by faking mutability.
				let is_error = value_type.is_err();
				analyzer.declare_variable(&name, false || is_error);

				// We do not need to analyze constants for mutability because
				// they are already analyzed for constness, and mutations and
				// addresses are never allowed in a constant expression.
				self
			}
			Declaration::Function {
				name,
				parameters,
				body,
				return_type,
				flags,
				location_of_declaration,
				location_of_return_type,
			} =>
			{
				let parameters = parameters
					.into_iter()
					.map(|x| x.analyze(analyzer))
					.collect();
				let body = match body
				{
					Ok(body) => Ok(body.analyze(analyzer)),
					Err(poison) => Err(poison),
				};
				Declaration::Function {
					name,
					parameters,
					body,
					return_type,
					flags,
					location_of_declaration,
					location_of_return_type,
				}
			}
			Declaration::FunctionHead {
				name,
				parameters,
				return_type,
				flags,
				location_of_declaration,
				location_of_return_type,
			} =>
			{
				let parameters = parameters
					.into_iter()
					.map(|x| x.analyze(analyzer))
					.collect();
				Declaration::FunctionHead {
					name,
					parameters,
					return_type,
					flags,
					location_of_declaration,
					location_of_return_type,
				}
			}
			Declaration::Structure {
				name,
				members,
				structural_type,
				flags,
				depth,
				location_of_declaration,
			} =>
			{
				let members =
					members.into_iter().map(|x| x.analyze(analyzer)).collect();
				Declaration::Structure {
					name,
					members,
					structural_type,
					flags,
					depth,
					location_of_declaration,
				}
			}
			Declaration::Import { .. } => self,
			Declaration::Poison(_) => self,
		}
	}
}

impl Analyzable for Member
{
	fn analyze(self, analyzer: &mut Analyzer) -> Self
	{
		match &self.name
		{
			Ok(name) =>
			{
				analyzer.declare_variable(name, true);
			}
			Err(_poison) => (),
		}
		self
	}
}

impl Analyzable for Parameter
{
	fn analyze(self, analyzer: &mut Analyzer) -> Self
	{
		match &self.name
		{
			Ok(name) =>
			{
				// If the parameter type contains an error, autoderef may have
				// been skipped, which means we do not know if we have interior
				// mutability. Avoid cascading errors by faking mutability.
				let is_error = self.value_type.is_err();
				analyzer.declare_variable(name, false || is_error);
			}
			Err(_poison) => (),
		}
		self
	}
}

impl Analyzable for FunctionBody
{
	fn analyze(self, analyzer: &mut Analyzer) -> Self
	{
		let statements = self
			.statements
			.into_iter()
			.map(|x| x.analyze(analyzer))
			.collect();
		let return_value = self.return_value.map(|x| x.analyze(analyzer));
		FunctionBody {
			statements,
			return_value,
			return_value_identifier: self.return_value_identifier,
		}
	}
}

impl Analyzable for Block
{
	fn analyze(self, analyzer: &mut Analyzer) -> Self
	{
		let statements = self
			.statements
			.into_iter()
			.map(|x| x.analyze(analyzer))
			.collect();
		Block {
			statements,
			location: self.location,
		}
	}
}

impl Analyzable for Statement
{
	fn analyze(self, analyzer: &mut Analyzer) -> Self
	{
		match self
		{
			Statement::Declaration {
				name,
				value,
				value_type,
				location,
			} =>
			{
				let value = value.map(|x| x.analyze(analyzer));
				let is_mutable = match value_type
				{
					Some(Ok(ValueType::Slice { .. })) => false,
					Some(Ok(ValueType::SlicePointer { .. })) => false,
					Some(Ok(ValueType::View { .. })) => false,
					Some(Ok(_)) => true,
					Some(Err(_)) => true,
					None => true,
				};
				analyzer.declare_variable(&name, is_mutable);
				Statement::Declaration {
					name,
					value,
					value_type,
					location,
				}
			}
			Statement::Assignment {
				reference,
				value,
				location,
			} =>
			{
				let value = value.analyze(analyzer);
				let reference = reference.analyze(analyzer);

				match analyzer.use_variable(
					&reference.base,
					needs_outer_mutability(&reference),
				)
				{
					Ok(()) => Statement::Assignment {
						reference,
						value,
						location,
					},
					Err(poison) => Statement::Poison(poison),
				}
			}
			Statement::MethodCall {
				name,
				builtin,
				arguments,
			} =>
			{
				let arguments = arguments
					.into_iter()
					.map(|x| x.analyze(analyzer))
					.collect();
				Statement::MethodCall {
					name,
					builtin,
					arguments,
				}
			}
			Statement::Loop { .. } => self,
			Statement::Goto { .. } => self,
			Statement::Label { .. } => self,
			Statement::If {
				condition,
				then_branch,
				else_branch,
				location,
			} =>
			{
				let condition = condition.analyze(analyzer);
				let then_branch = Box::new(then_branch.analyze(analyzer));
				let else_branch = else_branch.map(|x| {
					let branch = x.branch.analyze(analyzer);
					Else {
						branch: Box::new(branch),
						location_of_else: x.location_of_else,
					}
				});
				Statement::If {
					condition,
					then_branch,
					else_branch,
					location,
				}
			}
			Statement::Block(block) =>
			{
				let block = block.analyze(analyzer);
				Statement::Block(block)
			}
			Statement::Poison(_) => self,
		}
	}
}

impl Analyzable for Comparison
{
	fn analyze(self, analyzer: &mut Analyzer) -> Self
	{
		let left = self.left.analyze(analyzer);
		let right = self.right.analyze(analyzer);
		Comparison {
			op: self.op,
			left,
			right,
			location: self.location,
			location_of_op: self.location_of_op,
		}
	}
}

impl Analyzable for Array
{
	fn analyze(self, analyzer: &mut Analyzer) -> Self
	{
		let elements = self
			.elements
			.into_iter()
			.map(|x| x.analyze(analyzer))
			.collect();
		Array {
			elements,
			location: self.location,
			resolution_id: self.resolution_id,
		}
	}
}

impl Analyzable for Expression
{
	fn analyze(self, analyzer: &mut Analyzer) -> Self
	{
		match self
		{
			Expression::Binary {
				op,
				left,
				right,
				location,
				location_of_op,
			} =>
			{
				let left = left.analyze(analyzer);
				let right = right.analyze(analyzer);
				Expression::Binary {
					op,
					left: Box::new(left),
					right: Box::new(right),
					location,
					location_of_op,
				}
			}
			Expression::Unary {
				op,
				expression,
				location,
				location_of_op,
			} =>
			{
				let expression = expression.analyze(analyzer);
				Expression::Unary {
					op,
					expression: Box::new(expression),
					location,
					location_of_op,
				}
			}
			Expression::BooleanLiteral { .. } => self,
			Expression::SignedIntegerLiteral { .. } => self,
			Expression::BitIntegerLiteral { .. } => self,
			Expression::ArrayLiteral {
				array,
				element_type,
			} =>
			{
				let array = array.analyze(analyzer);
				Expression::ArrayLiteral {
					array,
					element_type,
				}
			}
			Expression::StringLiteral { .. } => self,
			Expression::Structural {
				members,
				structural_type,
				location,
			} =>
			{
				let members = members
					.into_iter()
					.map(|member| {
						let expression = member.expression.analyze(analyzer);
						MemberExpression {
							expression,
							..member
						}
					})
					.collect();
				Expression::Structural {
					members,
					structural_type,
					location,
				}
			}
			Expression::Parenthesized { inner, location } =>
			{
				let inner = inner.analyze(analyzer);
				Expression::Parenthesized {
					inner: Box::new(inner),
					location,
				}
			}
			Expression::Autocoerce {
				expression,
				coerced_type,
			} =>
			{
				let expression = expression.analyze(analyzer);
				Expression::Autocoerce {
					expression: Box::new(expression),
					coerced_type,
				}
			}
			Expression::BitCast {
				expression,
				coerced_type,
				location,
				location_of_keyword,
			} =>
			{
				let expression = expression.analyze(analyzer);
				Expression::BitCast {
					expression: Box::new(expression),
					coerced_type,
					location,
					location_of_keyword,
				}
			}
			Expression::TypeCast {
				expression,
				coerced_type,
				location,
				location_of_type,
			} =>
			{
				let expression = expression.analyze(analyzer);
				Expression::TypeCast {
					expression: Box::new(expression),
					coerced_type,
					location,
					location_of_type,
				}
			}
			Expression::Deref {
				reference,
				deref_type,
			} =>
			{
				let is_addressed = reference.address_depth > 0
					&& needs_outer_mutability(&reference);
				let reference = reference.analyze(analyzer);
				match analyzer.use_variable(&reference.base, is_addressed)
				{
					Ok(()) => Expression::Deref {
						reference,
						deref_type,
					},
					Err(poison) => Expression::Poison(poison),
				}
			}
			Expression::LengthOfArray {
				reference,
				location,
			} =>
			{
				let reference = reference.analyze(analyzer);
				match analyzer.use_variable(&reference.base, false)
				{
					Ok(()) => Expression::LengthOfArray {
						reference,
						location,
					},
					Err(poison) => Expression::Poison(poison),
				}
			}
			Expression::SizeOf { .. } => self,
			Expression::FunctionCall {
				name,
				builtin,
				arguments,
				return_type,
			} =>
			{
				let arguments = arguments
					.into_iter()
					.map(|argument| argument.analyze(analyzer))
					.collect();
				Expression::FunctionCall {
					name,
					builtin,
					arguments,
					return_type,
				}
			}
			Expression::Poison(_) => self,
		}
	}
}

impl Analyzable for ReferenceStep
{
	fn analyze(self, analyzer: &mut Analyzer) -> Self
	{
		match self
		{
			ReferenceStep::Element {
				argument,
				is_endless,
			} =>
			{
				let argument = argument.analyze(analyzer);
				ReferenceStep::Element {
					argument: Box::new(argument),
					is_endless,
				}
			}
			ReferenceStep::Member { .. } => self,
			ReferenceStep::Autodeslice { .. } => self,
			ReferenceStep::Autoderef => self,
			ReferenceStep::Autoview => self,
		}
	}
}

impl Analyzable for Reference
{
	fn analyze(self, analyzer: &mut Analyzer) -> Self
	{
		let steps = self
			.steps
			.into_iter()
			.map(|x| x.analyze(analyzer))
			.collect();
		Reference {
			base: self.base,
			steps,
			address_depth: self.address_depth,
			location: self.location,
			location_of_unaddressed: self.location_of_unaddressed,
		}
	}
}

fn needs_outer_mutability(reference: &Reference) -> bool
{
	for step in reference.steps.iter()
	{
		match step
		{
			ReferenceStep::Element { .. } => (),
			ReferenceStep::Member { .. } => (),
			ReferenceStep::Autodeslice { offset } => match offset
			{
				DesliceOffset::ArrayByView => (),
				DesliceOffset::ArrayByPointer =>
				{
					return false;
				}
				DesliceOffset::Length => (),
			},
			ReferenceStep::Autoderef =>
			{
				return false;
			}
			ReferenceStep::Autoview => (),
		}
	}
	true
}
