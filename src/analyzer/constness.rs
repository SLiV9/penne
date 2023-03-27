//
// Part of penne
// Copyright (c) 2020 Sander in 't Veld
// License: MIT
//

use crate::common::*;
use crate::error::Error;
use crate::typer::Typed;

pub fn analyze(declaration: Declaration) -> Declaration
{
	declaration.analyze()
}

trait Analyzable
{
	fn analyze(self) -> Self;
}

impl Analyzable for Declaration
{
	fn analyze(self) -> Self
	{
		match self
		{
			Declaration::Constant {
				name,
				value,
				value_type,
				flags,
				depth,
				location_of_declaration,
				location_of_type,
			} =>
			{
				let value = value.analyze();
				Declaration::Constant {
					name,
					value,
					value_type,
					flags,
					depth,
					location_of_declaration,
					location_of_type,
				}
			}
			Declaration::Function { .. } => self,
			Declaration::FunctionHead { .. } => self,
			Declaration::Structure { .. } => self,
			Declaration::Import { .. } => self,
			Declaration::Poison(_) => self,
		}
	}
}

impl Analyzable for Array
{
	fn analyze(self) -> Self
	{
		let elements = self.elements.into_iter().map(|x| x.analyze()).collect();
		Array {
			elements,
			location: self.location,
			resolution_id: self.resolution_id,
		}
	}
}

impl Analyzable for Expression
{
	fn analyze(self) -> Self
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
				let left = left.analyze();
				let right = right.analyze();
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
				let expression = expression.analyze();
				Expression::Unary {
					op,
					expression: Box::new(expression),
					location,
					location_of_op,
				}
			}
			Expression::PrimitiveLiteral { .. } => self,
			Expression::NakedIntegerLiteral { .. } => self,
			Expression::BitIntegerLiteral { .. } => self,
			Expression::ArrayLiteral {
				array,
				element_type,
			} =>
			{
				let array = array.analyze();
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
						let expression = member.expression.analyze();
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
				let inner = inner.analyze();
				Expression::Parenthesized {
					inner: Box::new(inner),
					location,
				}
			}
			Expression::Deref {
				reference,
				deref_type,
			} =>
			{
				if !reference.is_trivial()
				{
					let error = Error::UnsupportedInConstContext {
						location: reference.location,
					};
					return Expression::Poison(Poison::Error(error));
				}

				let deref_type = match deref_type
				{
					Some(Ok(ValueType::Array { .. }))
					| Some(Ok(ValueType::EndlessArray { .. })) =>
					{
						Some(Err(Poison::Error(Error::CannotCopyArray {
							location: reference.location.clone(),
						})))
					}
					Some(Ok(ValueType::Slice { .. }))
					| Some(Ok(ValueType::SlicePointer { .. }))
					| Some(Ok(ValueType::Arraylike { .. })) =>
					{
						Some(Err(Poison::Error(Error::CannotCopySlice {
							location: reference.location.clone(),
						})))
					}
					Some(Ok(ValueType::Struct { .. })) =>
					{
						Some(Err(Poison::Error(Error::CannotCopyStruct {
							location: reference.location.clone(),
						})))
					}
					_ => deref_type,
				};
				let reference = reference.analyze();
				Expression::Deref {
					reference,
					deref_type,
				}
			}
			Expression::Autocoerce {
				expression,
				coerced_type,
			} =>
			{
				let expression = expression.analyze();
				Expression::Autocoerce {
					expression: Box::new(expression),
					coerced_type,
				}
			}
			Expression::PrimitiveCast {
				expression,
				coerced_type,
				location,
				location_of_type,
			} =>
			{
				let expression = expression.analyze();
				Expression::PrimitiveCast {
					expression: Box::new(expression),
					coerced_type,
					location,
					location_of_type,
				}
			}
			Expression::LengthOfArray {
				reference: _,
				location,
			} =>
			{
				let error = Error::UnsupportedInConstContext { location };
				Expression::Poison(Poison::Error(error))
			}
			Expression::SizeOfStructure { .. } => self,
			Expression::FunctionCall {
				name,
				arguments: _,
				return_type: _,
			} =>
			{
				let error = Error::FunctionInConstContext {
					location: name.location,
				};
				Expression::Poison(Poison::Error(error))
			}
			Expression::Poison(_) => self,
		}
	}
}

impl Analyzable for ReferenceStep
{
	fn analyze(self) -> Self
	{
		match self
		{
			ReferenceStep::Element {
				argument,
				is_endless,
			} =>
			{
				let argument = argument.analyze();
				let argument = match argument.value_type()
				{
					Some(Ok(ValueType::Usize)) => argument,
					Some(Ok(other_type)) => Expression::Poison(Poison::Error(
						Error::IndexTypeMismatch {
							argument_type: other_type,
							index_type: ValueType::Usize,
							location: argument.location().clone(),
						},
					)),
					Some(Err(_)) => argument,
					None => argument,
				};
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
	fn analyze(self) -> Self
	{
		let steps = self.steps.into_iter().map(|x| x.analyze()).collect();
		Reference {
			base: self.base,
			steps,
			address_depth: self.address_depth,
			location: self.location,
			location_of_unaddressed: self.location_of_unaddressed,
		}
	}
}
