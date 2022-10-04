//
// Part of penne
// Copyright (c) 2020 Sander in 't Veld
// License: MIT
//

use crate::common;
use crate::common::*;
use crate::resolved;
use crate::typer::Typed;

use anyhow::anyhow;
use anyhow::Context;

pub fn resolve(
	program: Vec<common::Declaration>,
) -> Result<Vec<resolved::Declaration>, anyhow::Error>
{
	program.resolve()
}

trait Resolvable
{
	type Item;

	fn resolve(self) -> Result<Self::Item, anyhow::Error>;
}

impl<T> Resolvable for Vec<T>
where
	T: Resolvable,
{
	type Item = Vec<T::Item>;

	fn resolve(self) -> Result<Self::Item, anyhow::Error>
	{
		self.into_iter().map(|x| x.resolve()).collect()
	}
}

impl Resolvable for Declaration
{
	type Item = resolved::Declaration;

	fn resolve(self) -> Result<Self::Item, anyhow::Error>
	{
		match self
		{
			Declaration::Constant {
				name,
				value,
				value_type,
				flags,
			} => Ok(resolved::Declaration::Constant {
				name: name.resolve()?,
				value: value.resolve()?,
				value_type,
				flags,
			}),
			Declaration::Function {
				name,
				parameters,
				body,
				return_type,
				flags,
			} => Ok(resolved::Declaration::Function {
				name: name.resolve()?,
				parameters: parameters.resolve()?,
				body: body.resolve()?,
				return_type,
				flags,
			}),
			Declaration::FunctionHead {
				name,
				parameters,
				return_type,
				flags,
			} => Ok(resolved::Declaration::FunctionHead {
				name: name.resolve()?,
				parameters: parameters.resolve()?,
				return_type,
				flags,
			}),
			Declaration::PreprocessorDirective { .. } => unreachable!(),
		}
	}
}

impl Resolvable for Parameter
{
	type Item = resolved::Parameter;

	fn resolve(self) -> Result<Self::Item, anyhow::Error>
	{
		if let Some(value_type) = self.value_type
		{
			Ok(resolved::Parameter {
				name: self.name.resolve()?,
				value_type,
			})
		}
		else
		{
			Err(anyhow!("failed to infer type")
				.context(self.name.location.format())
				.context(format!(
					"failed to infer type for '{}'",
					self.name.name
				)))
		}
	}
}

impl Resolvable for FunctionBody
{
	type Item = resolved::FunctionBody;

	fn resolve(self) -> Result<Self::Item, anyhow::Error>
	{
		Ok(resolved::FunctionBody {
			statements: self.statements.resolve()?,
			return_value: self.return_value.map(|v| v.resolve()).transpose()?,
		})
	}
}

impl Resolvable for Statement
{
	type Item = resolved::Statement;

	fn resolve(self) -> Result<Self::Item, anyhow::Error>
	{
		match self
		{
			Statement::Declaration {
				name,
				value: Some(value),
				value_type: Some(vt),
				location,
			} =>
			{
				let value =
					value.resolve().with_context(|| location.format())?;
				Ok(resolved::Statement::Declaration {
					name: name.resolve()?,
					value: Some(value),
					value_type: vt,
				})
			}
			Statement::Declaration {
				name,
				value: None,
				value_type: Some(vt),
				location: _,
			} => Ok(resolved::Statement::Declaration {
				name: name.resolve()?,
				value: None,
				value_type: vt,
			}),
			Statement::Declaration {
				name,
				value: _,
				value_type: None,
				location,
			} => Err(anyhow!("failed to infer type")
				.context(location.format())
				.context(format!("failed to infer type for '{}'", name.name))),
			Statement::Assignment {
				reference,
				value,
				location,
			} if value.value_type().is_some() =>
			{
				let value =
					value.resolve().with_context(|| location.format())?;
				let reference =
					reference.resolve().with_context(|| location.format())?;
				Ok(resolved::Statement::Assignment { reference, value })
			}
			Statement::Assignment {
				reference,
				value: _,
				location: _,
			} => Err(anyhow!("failed to infer type")
				.context(reference.location.format())
				.context(format!(
					"failed to infer type for '{}'",
					reference.base.name
				))),
			Statement::MethodCall { name, arguments } =>
			{
				Ok(resolved::Statement::MethodCall {
					name: name.resolve()?,
					arguments: arguments.resolve()?,
				})
			}
			Statement::Loop { .. } => Ok(resolved::Statement::Loop),
			Statement::Goto { label, location: _ } =>
			{
				Ok(resolved::Statement::Goto {
					label: label.resolve()?,
				})
			}
			Statement::Label { label, location: _ } =>
			{
				Ok(resolved::Statement::Label {
					label: label.resolve()?,
				})
			}
			Statement::If {
				condition,
				then_branch,
				else_branch,
				location,
			} =>
			{
				let condition =
					condition.resolve().with_context(|| location.format())?;
				let then_branch = {
					let branch = then_branch.resolve()?;
					Box::new(branch)
				};
				let else_branch = match else_branch
				{
					Some(else_branch) =>
					{
						let branch = else_branch.resolve()?;
						Some(Box::new(branch))
					}
					None => None,
				};
				Ok(resolved::Statement::If {
					condition,
					then_branch,
					else_branch,
				})
			}
			Statement::Block(Block {
				statements,
				location: _,
			}) => Ok(resolved::Statement::Block(resolved::Block {
				statements: statements.resolve()?,
			})),
		}
	}
}

impl Resolvable for Comparison
{
	type Item = resolved::Comparison;

	fn resolve(self) -> Result<Self::Item, anyhow::Error>
	{
		let compared_type =
			resolve_compared_type(self.op, &self.left, &self.right)
				.with_context(|| self.location.format())
				.with_context(|| {
					"failed to infer valid operand types for comparison operator"
				})?;
		let left = self
			.left
			.resolve()
			.with_context(|| self.location.format())?;
		let right = self
			.right
			.resolve()
			.with_context(|| self.location.format())?;
		Ok(resolved::Comparison {
			op: self.op,
			left,
			right,
			compared_type,
		})
	}
}

impl Resolvable for Expression
{
	type Item = resolved::Expression;

	fn resolve(self) -> Result<Self::Item, anyhow::Error>
	{
		match self
		{
			Expression::Binary {
				op,
				left,
				right,
				location,
			} =>
			{
				let value_type = resolve_binary_op_type(op, &left, &right)
					.with_context(|| location.format())
					.with_context(|| {
						"failed to infer valid types for binary operator"
					})?;
				let left = left.resolve().with_context(|| location.format())?;
				let right =
					right.resolve().with_context(|| location.format())?;
				Ok(resolved::Expression::Binary {
					op,
					left: Box::new(left),
					right: Box::new(right),
					value_type,
				})
			}
			Expression::Unary {
				op,
				expression,
				location,
			} =>
			{
				let _value_type = resolve_unary_op_type(op, &expression)
					.with_context(|| location.format())
					.with_context(|| {
						"failed to infer valid operand type for unary operator"
					})?;
				let expr =
					expression.resolve().with_context(|| location.format())?;
				Ok(resolved::Expression::Unary {
					op,
					expression: Box::new(expr),
				})
			}
			Expression::PrimitiveLiteral(lit) =>
			{
				Ok(resolved::Expression::PrimitiveLiteral(lit))
			}
			Expression::NakedIntegerLiteral {
				value,
				value_type: Some(value_type),
				location: _,
			} => Ok(resolved::Expression::NakedIntegerLiteral {
				value,
				value_type,
			}),
			Expression::NakedIntegerLiteral {
				value,
				value_type: None,
				location,
			} => match i32::try_from(value)
			{
				// Naked integer literals that can fit in an int should not
				// cause type resolution errors; if they are assigned to a
				// variable that that variable will error, if they are unused
				// (e.g. excess function argument) then that is more relevant.
				// Disallow i32::MIN because ValueType::Int is symmetric.
				Ok(i32::MIN) | Err(_) => Err(anyhow!("failed to infer type")
					.context(location.format())
					.context(format!("failed to infer integer literal type"))),
				Ok(_) => Ok(resolved::Expression::NakedIntegerLiteral {
					value,
					value_type: ValueType::Int32,
				}),
			},
			Expression::BitIntegerLiteral {
				value,
				value_type: Some(value_type),
				location: _,
			} => Ok(resolved::Expression::BitIntegerLiteral {
				value,
				value_type,
			}),
			Expression::BitIntegerLiteral {
				value: _,
				value_type: None,
				location,
			} => Err(anyhow!("failed to infer type")
				.context(location.format())
				.context(format!("failed to infer integer literal type"))),
			Expression::ArrayLiteral {
				array:
					Array {
						elements,
						location,
						resolution_id: _,
					},
				element_type,
			} =>
			{
				if let Some(element_type) = element_type
				{
					Ok(resolved::Expression::ArrayLiteral {
						elements: elements.resolve()?,
						element_type,
					})
				}
				else
				{
					Err(anyhow!("failed to infer type")
						.context(location.format())
						.context(format!(
							"failed to infer array literal element type"
						)))
				}
			}
			Expression::StringLiteral {
				bytes,
				value_type: Some(value_type),
				location,
			} => match value_type
			{
				ValueType::String =>
				{
					Ok(resolved::Expression::StringLiteral { bytes })
				}
				ValueType::Slice { .. } =>
				{
					Ok(resolved::Expression::ByteStringLiteral { bytes })
				}
				_ => Err(anyhow!("unexpected type")
					.context(location.format())
					.context(format!("failed to infer string literal type"))),
			},
			Expression::StringLiteral {
				bytes: _,
				value_type: _,
				location,
			} => Err(anyhow!("failed to infer type")
				.context(location.format())
				.context(format!("failed to infer string literal type"))),
			Expression::Deref {
				reference,
				deref_type,
			} =>
			{
				if let Some(deref_type) = deref_type
				{
					Ok(resolved::Expression::Deref {
						reference: reference.resolve()?,
						deref_type,
					})
				}
				else
				{
					Err(anyhow!("failed to infer type")
						.context(reference.location.format())
						.context(format!("failed to infer type of reference")))
				}
			}
			Expression::Autocoerce {
				expression,
				coerced_type,
			} =>
			{
				if expression.value_type().is_some()
				{
					let expression = expression.resolve()?;
					Ok(resolved::Expression::Autocoerce {
						expression: Box::new(expression),
						coerced_type: coerced_type.clone(),
					})
				}
				else
				{
					Err(anyhow!("failed to infer type in coercion")
						.context(format!("failed to infer type in coercion")))
				}
			}
			Expression::PrimitiveCast {
				expression,
				coerced_type,
				location,
			} =>
			{
				let expression_type =
					analyze_primitive_cast(&expression, coerced_type.clone())
						.with_context(|| location.format())
						.with_context(|| {
							"failed to infer valid expression type for cast"
						})?;
				let expression = expression.resolve()?;
				Ok(resolved::Expression::PrimitiveCast {
					expression: Box::new(expression),
					expression_type,
					coerced_type: coerced_type,
				})
			}
			Expression::LengthOfArray { reference } =>
			{
				Ok(resolved::Expression::LengthOfArray {
					reference: reference.resolve()?,
				})
			}
			Expression::FunctionCall {
				name,
				arguments,
				return_type,
			} =>
			{
				if let Some(return_type) = return_type
				{
					Ok(resolved::Expression::FunctionCall {
						name: name.resolve()?,
						arguments: arguments.resolve()?,
						return_type,
					})
				}
				else
				{
					Err(anyhow!("failed to infer return type")
						.context(name.location.format())
						.context(format!("failed to infer return type")))
				}
			}
		}
	}
}

impl Resolvable for Reference
{
	type Item = resolved::Reference;

	fn resolve(self) -> Result<Self::Item, anyhow::Error>
	{
		Ok(resolved::Reference {
			base: self.base.resolve()?,
			steps: self.steps.resolve()?,
			take_address: self.address_depth > 0,
		})
	}
}

impl Resolvable for ReferenceStep
{
	type Item = resolved::ReferenceStep;

	fn resolve(self) -> Result<Self::Item, anyhow::Error>
	{
		match self
		{
			ReferenceStep::Element { argument } =>
			{
				let argument = argument.resolve()?;
				Ok(resolved::ReferenceStep::Element {
					argument: Box::new(argument),
				})
			}
			ReferenceStep::Member { member } =>
			{
				Ok(resolved::ReferenceStep::Member {
					member: member.resolve()?,
				})
			}
			ReferenceStep::Autodeslice { offset } =>
			{
				Ok(resolved::ReferenceStep::Autodeslice { offset })
			}
			ReferenceStep::Autoderef => Ok(resolved::ReferenceStep::Autoderef),
			ReferenceStep::Autoview => Ok(resolved::ReferenceStep::Autoview),
		}
	}
}

impl Resolvable for Identifier
{
	type Item = resolved::Identifier;

	fn resolve(self) -> Result<Self::Item, anyhow::Error>
	{
		if self.resolution_id > 0
		{
			Ok(resolved::Identifier {
				name: self.name,
				resolution_id: self.resolution_id,
			})
		}
		else
		{
			Err(anyhow!("failed to resolve identifier")
				.context(self.location.format())
				.context(format!("failed to resolve '{}'", self.name)))
		}
	}
}

fn resolve_compared_type(
	op: ComparisonOp,
	left: &Expression,
	right: &Expression,
) -> Result<ValueType, anyhow::Error>
{
	let vt = if let Some(vt) = left.value_type()
	{
		vt
	}
	else
	{
		return Err(anyhow!("failed to infer type"));
	};
	match right.value_type()
	{
		Some(rvt) if rvt == vt => (),
		Some(rvt) =>
		{
			return Err(anyhow!("type mismatch")
				.context(format!("got {:?} and {:?}", vt, rvt)))
		}
		None => return Err(anyhow!("failed to infer type")),
	}
	match op
	{
		ComparisonOp::Equals | ComparisonOp::DoesNotEqual => match vt
		{
			ValueType::Int8 => Ok(vt),
			ValueType::Int16 => Ok(vt),
			ValueType::Int32 => Ok(vt),
			ValueType::Int64 => Ok(vt),
			ValueType::Int128 => Ok(vt),
			ValueType::Uint8 => Ok(vt),
			ValueType::Uint16 => Ok(vt),
			ValueType::Uint32 => Ok(vt),
			ValueType::Uint64 => Ok(vt),
			ValueType::Uint128 => Ok(vt),
			ValueType::Usize => Ok(vt),
			ValueType::Bool => Ok(vt),
			ValueType::Char => Ok(vt),
			ValueType::Pointer { deref_type: _ } => Ok(vt),
			vt => Err(anyhow!("invalid operand types").context(format!(
				"expected number, bool, char, pointer, got {:?}",
				vt
			))),
		},
		ComparisonOp::IsGreater
		| ComparisonOp::IsLess
		| ComparisonOp::IsGE
		| ComparisonOp::IsLE => match vt
		{
			ValueType::Int8 => Ok(vt),
			ValueType::Int16 => Ok(vt),
			ValueType::Int32 => Ok(vt),
			ValueType::Int64 => Ok(vt),
			ValueType::Int128 => Ok(vt),
			ValueType::Uint8 => Ok(vt),
			ValueType::Uint16 => Ok(vt),
			ValueType::Uint32 => Ok(vt),
			ValueType::Uint64 => Ok(vt),
			ValueType::Uint128 => Ok(vt),
			ValueType::Usize => Ok(vt),
			ValueType::Bool => Ok(vt),
			ValueType::Char => Ok(vt),
			vt => Err(anyhow!("invalid operand types")
				.context(format!("expected number, bool, char, got {:?}", vt))),
		},
	}
}

fn resolve_binary_op_type(
	op: BinaryOp,
	left: &Expression,
	right: &Expression,
) -> Result<ValueType, anyhow::Error>
{
	let vt = if let Some(vt) = left.value_type()
	{
		vt
	}
	else
	{
		return Err(anyhow!("failed to infer type"));
	};
	match right.value_type()
	{
		Some(rvt) if rvt == vt => (),
		Some(rvt) =>
		{
			return Err(anyhow!("type mismatch")
				.context(format!("got {:?} and {:?}", vt, rvt)))
		}
		None => return Err(anyhow!("failed to infer type")),
	}
	match op
	{
		BinaryOp::Add
		| BinaryOp::Subtract
		| BinaryOp::Multiply
		| BinaryOp::Divide
		| BinaryOp::Modulo => match vt
		{
			ValueType::Int8 => Ok(vt),
			ValueType::Int16 => Ok(vt),
			ValueType::Int32 => Ok(vt),
			ValueType::Int64 => Ok(vt),
			ValueType::Int128 => Ok(vt),
			ValueType::Uint8 => Ok(vt),
			ValueType::Uint16 => Ok(vt),
			ValueType::Uint32 => Ok(vt),
			ValueType::Uint64 => Ok(vt),
			ValueType::Uint128 => Ok(vt),
			ValueType::Usize => Ok(vt),
			vt => Err(anyhow!("invalid operand types").context(format!(
				"expected i8, i16, ..., u8, u16, ..., usize, got {:?}",
				vt
			))),
		},
		BinaryOp::BitwiseAnd | BinaryOp::BitwiseOr | BinaryOp::BitwiseXor =>
		{
			match vt
			{
				ValueType::Uint8 => Ok(vt),
				ValueType::Uint16 => Ok(vt),
				ValueType::Uint32 => Ok(vt),
				ValueType::Uint64 => Ok(vt),
				ValueType::Uint128 => Ok(vt),
				vt => Err(anyhow!("invalid operand types")
					.context(format!("expected u8, u16, ..., got {:?}", vt))),
			}
		}
		BinaryOp::ShiftLeft | BinaryOp::ShiftRight => match vt
		{
			ValueType::Uint8 => Ok(vt),
			ValueType::Uint16 => Ok(vt),
			ValueType::Uint32 => Ok(vt),
			ValueType::Uint64 => Ok(vt),
			ValueType::Uint128 => Ok(vt),
			vt => Err(anyhow!("invalid operand types")
				.context(format!("expected u8, u16, ..., got {:?}", vt))),
		},
	}
}

fn resolve_unary_op_type(
	op: UnaryOp,
	operand: &Expression,
) -> Result<ValueType, anyhow::Error>
{
	let vt = if let Some(vt) = operand.value_type()
	{
		vt
	}
	else
	{
		return Err(anyhow!("failed to infer type"));
	};
	match op
	{
		UnaryOp::Negative => match vt
		{
			ValueType::Int8 => Ok(vt),
			ValueType::Int16 => Ok(vt),
			ValueType::Int32 => Ok(vt),
			ValueType::Int64 => Ok(vt),
			ValueType::Int128 => Ok(vt),
			vt => Err(anyhow!("invalid operand type")
				.context(format!("expected i8, i16, ..., got {:?}", vt))),
		},
		UnaryOp::BitwiseComplement => match vt
		{
			ValueType::Bool => Ok(vt),
			ValueType::Uint8 => Ok(vt),
			ValueType::Uint16 => Ok(vt),
			ValueType::Uint32 => Ok(vt),
			ValueType::Uint64 => Ok(vt),
			ValueType::Uint128 => Ok(vt),
			vt => Err(anyhow!("invalid operand type")
				.context(format!("expected bool, u8, u16, ..., got {:?}", vt))),
		},
	}
}

fn analyze_primitive_cast(
	expression: &Expression,
	coerced_type: ValueType,
) -> Result<ValueType, anyhow::Error>
{
	let value_type = if let Some(vt) = expression.value_type()
	{
		vt
	}
	else
	{
		return Err(anyhow!("failed to infer type"));
	};
	match (&value_type, &coerced_type)
	{
		(x, y) if x == y => Ok(value_type),
		(vt, ct) if vt.is_integral() && ct.is_integral() => Ok(value_type),
		(ValueType::Bool, ct) if ct.is_integral() => Ok(value_type),
		(vt, ValueType::Bool) if vt.is_integral() => Ok(value_type),
		(ValueType::Char, ValueType::Uint8) => Ok(value_type),
		(ValueType::Char, ValueType::Uint32) => Ok(value_type),
		(ValueType::Uint8, ValueType::Char) => Ok(value_type),
		(ValueType::Uint32, ValueType::Char) => Ok(value_type),
		(vt, _) => Err(anyhow!("invalid expression type")
			.context(format!("expected primitive, got {:?}", vt))),
	}
}
