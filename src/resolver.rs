//
// Part of penne
// Copyright (c) 2020 Sander in 't Veld
// License: MIT
//

use crate::common;
use crate::common::*;
use crate::error::Error;
use crate::error::Errors;
use crate::error::{Poison, Poisonable};
use crate::resolved;
use crate::typer::Typed;
use crate::value_type::OperandValueType;

pub fn resolve(
	program: Vec<common::Declaration>,
) -> Result<Vec<resolved::Declaration>, Errors>
{
	program.resolve()
}

trait Resolvable
{
	type Item;

	fn resolve(self) -> Result<Self::Item, Errors>;
}

impl<T> Resolvable for Vec<T>
where
	T: Resolvable,
{
	type Item = Vec<T::Item>;

	fn resolve(self) -> Result<Self::Item, Errors>
	{
		self.into_iter().map(|x| x.resolve()).fold(
			Ok(Vec::new()),
			|accum, result| match (accum, result)
			{
				(Ok(mut items), Ok(item)) =>
				{
					items.push(item);
					Ok(items)
				}
				(Ok(_), Err(errors)) => Err(errors),
				(Err(errors), Ok(_)) => Err(errors),
				(
					Err(Errors { mut errors }),
					Err(Errors { errors: mut more }),
				) =>
				{
					errors.append(&mut more);
					Err(Errors { errors })
				}
			},
		)
	}
}

impl<T1, T2> Resolvable for (T1, T2)
where
	T1: Resolvable,
	T2: Resolvable,
{
	type Item = (T1::Item, T2::Item);

	fn resolve(self) -> Result<Self::Item, Errors>
	{
		let (a, b) = self;
		match (a.resolve(), b.resolve())
		{
			(Ok(x), Ok(y)) => Ok((x, y)),
			(Ok(_), Err(errors)) => Err(errors),
			(Err(errors), Ok(_)) => Err(errors),
			(Err(Errors { mut errors }), Err(Errors { errors: mut more })) =>
			{
				errors.append(&mut more);
				Err(Errors { errors })
			}
		}
	}
}

impl<T1, T2, T3> Resolvable for (T1, T2, T3)
where
	T1: Resolvable,
	T2: Resolvable,
	T3: Resolvable,
{
	type Item = (T1::Item, T2::Item, T3::Item);

	fn resolve(self) -> Result<Self::Item, Errors>
	{
		let (a, b, c) = self;
		let ((a, b), c) = ((a, b), c).resolve()?;
		Ok((a, b, c))
	}
}

impl<T> Resolvable for Option<T>
where
	T: Resolvable,
{
	type Item = Option<T::Item>;

	fn resolve(self) -> Result<Self::Item, Errors>
	{
		self.map(|x| x.resolve()).transpose()
	}
}

impl<T> Resolvable for Box<T>
where
	T: Resolvable,
{
	type Item = Box<T::Item>;

	fn resolve(self) -> Result<Self::Item, Errors>
	{
		let x = (*self).resolve()?;
		Ok(Box::new(x))
	}
}

impl<T> Resolvable for Poisonable<T>
where
	T: Resolvable,
{
	type Item = T::Item;

	fn resolve(self) -> Result<Self::Item, Errors>
	{
		match self
		{
			Ok(x) => x.resolve(),
			Err(y) => y.resolve(),
		}
	}
}

impl<T> Resolvable for Poison<T>
where
	T: Resolvable,
{
	type Item = T::Item;

	fn resolve(self) -> Result<Self::Item, Errors>
	{
		match self
		{
			Poison::Error { error, partial: _ } => Err(error.into()),
			Poison::Poisoned =>
			{
				// Do not show any errors because this thing was poisoned by
				// a different error, and cascading errors are unreliable.
				Err(Errors { errors: Vec::new() })
			}
		}
	}
}

impl Resolvable for Declaration
{
	type Item = resolved::Declaration;

	fn resolve(self) -> Result<Self::Item, Errors>
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
				value_type: value_type.resolve()?,
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
			Declaration::Poison(poison) => match poison.resolve()
			{
				Ok(_) => unreachable!(),
				Err(e) => Err(e),
			},
		}
	}
}

impl Resolvable for Parameter
{
	type Item = resolved::Parameter;

	fn resolve(self) -> Result<Self::Item, Errors>
	{
		Ok(resolved::Parameter {
			name: self.name.resolve()?,
			value_type: self.value_type.resolve()?,
		})
	}
}

impl Resolvable for FunctionBody
{
	type Item = resolved::FunctionBody;

	fn resolve(self) -> Result<Self::Item, Errors>
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

	fn resolve(self) -> Result<Self::Item, Errors>
	{
		match self
		{
			Statement::Declaration {
				name,
				value: Some(value),
				value_type: Some(vt),
				location: _,
			} =>
			{
				let (name, value, vt) = (name, value, vt).resolve()?;
				Ok(resolved::Statement::Declaration {
					name,
					value: Some(value),
					value_type: vt,
				})
			}
			Statement::Declaration {
				name,
				value: None,
				value_type: Some(vt),
				location: _,
			} =>
			{
				let (name, vt) = (name, vt).resolve()?;
				Ok(resolved::Statement::Declaration {
					name,
					value: None,
					value_type: vt,
				})
			}
			Statement::Declaration {
				name: _,
				value: _,
				value_type: None,
				location,
			} => Err(Error::AmbiguousTypeOfDeclaration { location })?,
			Statement::Assignment {
				reference,
				value,
				location: _,
			} if value.value_type().is_some() =>
			{
				let (reference, value) = (reference, value).resolve()?;
				Ok(resolved::Statement::Assignment { reference, value })
			}
			Statement::Assignment {
				reference: _,
				value,
				location: _,
			} => Err(Error::AmbiguousType {
				location: value.location().clone(),
			})?,
			Statement::MethodCall { name, arguments } =>
			{
				let (name, arguments) = (name, arguments).resolve()?;
				Ok(resolved::Statement::MethodCall { name, arguments })
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
				location: _,
			} =>
			{
				let (condition, then_branch, else_branch) =
					(condition, then_branch, else_branch).resolve()?;
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
			Statement::Poison(poison) => match poison.resolve()
			{
				Ok(_) => unreachable!(),
				Err(e) => Err(e),
			},
		}
	}
}

impl Resolvable for Else
{
	type Item = Box<resolved::Statement>;

	fn resolve(self) -> Result<Self::Item, Errors>
	{
		self.branch.resolve()
	}
}

impl Resolvable for Comparison
{
	type Item = resolved::Comparison;

	fn resolve(self) -> Result<Self::Item, Errors>
	{
		let compared_type = resolve_compared_type(
			self.op,
			&self.left,
			&self.right,
			&self.location,
		);
		// If the left hand size contains errors, there is not much point in
		// reporting errors about the right side because type inference failed.
		let left = self.left.resolve()?;
		let right = self.right.resolve()?;
		// If either side contains errors, type inference will probably fail,
		// but there is not much point in reporting that.
		let compared_type = compared_type?;
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

	fn resolve(self) -> Result<Self::Item, Errors>
	{
		match self
		{
			Expression::Binary {
				op,
				left,
				right,
				location: _,
				location_of_op,
			} =>
			{
				let value_type =
					resolve_binary_op_type(op, &left, &right, &location_of_op);
				// If the left hand size contains errors, there is not much
				// point in reporting errors about the right side because
				// type inference failed.
				let left = left.resolve()?;
				let right = right.resolve()?;
				// If either side contains errors, type inference will probably
				// fail, but there is not much point in reporting that.
				let value_type = value_type?;
				Ok(resolved::Expression::Binary {
					op,
					left,
					right,
					value_type,
				})
			}
			Expression::Unary {
				op,
				expression,
				location: _,
				location_of_op,
			} =>
			{
				let value_type =
					resolve_unary_op_type(op, &expression, &location_of_op);
				let expression = expression.resolve()?;
				// If the expression contains errors, type inference will
				// fail, but there is not much point in reporting that.
				let _ = value_type?;
				Ok(resolved::Expression::Unary { op, expression })
			}
			Expression::PrimitiveLiteral {
				literal,
				location: _,
			} => Ok(resolved::Expression::PrimitiveLiteral(literal)),
			Expression::NakedIntegerLiteral {
				value,
				value_type: Some(value_type),
				location: _,
			} => Ok(resolved::Expression::NakedIntegerLiteral {
				value,
				value_type: value_type.resolve()?,
			}),
			Expression::NakedIntegerLiteral {
				value: _,
				value_type: None,
				location,
			} =>
			{
				// Naked integer literals that can fit in an int should not
				// cause type resolution errors; if they are assigned to a
				// variable that that variable will error, if they are unused
				// (e.g. excess function argument) then that is more relevant.
				// But if we are here, no other error was raised, and we cannot
				// continue to generation with a value of unknown (bit)size.
				Err(Error::AmbiguousTypeOfNakedIntegerLiteral {
					suggested_type: ValueType::Int32,
					location,
				})?
			}
			Expression::BitIntegerLiteral {
				value,
				value_type: Some(value_type),
				location: _,
			} => Ok(resolved::Expression::BitIntegerLiteral {
				value,
				value_type: value_type.resolve()?,
			}),
			Expression::BitIntegerLiteral {
				value: _,
				value_type: None,
				location,
			} => Err(Error::AmbiguousTypeOfNakedIntegerLiteral {
				suggested_type: ValueType::Uint64,
				location,
			})?,
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
					let (elements, element_type) =
						(elements, element_type).resolve()?;
					Ok(resolved::Expression::ArrayLiteral {
						elements,
						element_type,
					})
				}
				else
				{
					Err(Error::AmbiguousTypeOfArrayLiteral { location })?
				}
			}
			Expression::StringLiteral {
				bytes,
				value_type: Some(value_type),
				location: _,
			} => match value_type.resolve()?
			{
				ValueType::String =>
				{
					Ok(resolved::Expression::StringLiteral { bytes })
				}
				ValueType::Slice { .. } =>
				{
					Ok(resolved::Expression::ByteStringLiteral { bytes })
				}
				_ => unreachable!(),
			},
			Expression::StringLiteral {
				bytes: _,
				value_type: _,
				location,
			} => Err(Error::AmbiguousTypeOfStringLiteral { location })?,
			Expression::Deref {
				reference,
				deref_type,
			} =>
			{
				// If the reference fails to resolve, there is no point in
				// reporting about type inference.
				let reference_location = reference.location.clone();
				let reference = reference.resolve()?;
				if let Some(deref_type) = deref_type
				{
					Ok(resolved::Expression::Deref {
						reference,
						deref_type: deref_type.resolve()?,
					})
				}
				else
				{
					Err(Error::AmbiguousType {
						location: reference_location,
					})?
				}
			}
			Expression::Autocoerce {
				expression,
				coerced_type,
			} =>
			{
				let from = expression.value_type();
				let expression = expression.resolve()?;
				assert!(from.is_some());
				Ok(resolved::Expression::Autocoerce {
					expression,
					coerced_type,
				})
			}
			Expression::PrimitiveCast {
				expression,
				coerced_type,
				location: _,
				location_of_type,
			} =>
			{
				let expression_type = analyze_primitive_cast(
					&expression,
					coerced_type.clone(),
					&location_of_type,
				);
				let expression = expression.resolve()?;
				// If the expression contains errors, type inference will
				// fail, but there is not much point in reporting that.
				let expression_type = expression_type?;
				Ok(resolved::Expression::PrimitiveCast {
					expression,
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
				// If the reference fails to resolve, there is no point in
				// reporting about type inference.
				let location = name.location.clone();
				let (name, arguments) = (name, arguments).resolve()?;
				if let Some(return_type) = return_type
				{
					Ok(resolved::Expression::FunctionCall {
						name,
						arguments,
						return_type: return_type.resolve()?,
					})
				}
				else
				{
					Err(Error::AmbiguousType { location })?
				}
			}
			Expression::Poison(poison) => match poison.resolve()
			{
				Ok(_) => unreachable!(),
				Err(e) => Err(e),
			},
		}
	}
}

impl Resolvable for Reference
{
	type Item = resolved::Reference;

	fn resolve(self) -> Result<Self::Item, Errors>
	{
		// If the base reference is poisoned, there is no point in showing
		// errors related to the reference steps.
		let base = self.base.resolve()?;
		let steps = self.steps.resolve()?;
		Ok(resolved::Reference {
			base,
			steps,
			take_address: self.address_depth > 0,
		})
	}
}

impl Resolvable for ReferenceStep
{
	type Item = resolved::ReferenceStep;

	fn resolve(self) -> Result<Self::Item, Errors>
	{
		match self
		{
			ReferenceStep::Element {
				argument,
				is_endless,
			} =>
			{
				let argument = argument.resolve()?;
				Ok(resolved::ReferenceStep::Element {
					argument,
					is_endless: is_endless.unwrap_or(false),
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

	fn resolve(self) -> Result<Self::Item, Errors>
	{
		assert!(self.resolution_id > 0);
		Ok(resolved::Identifier {
			name: self.name,
			resolution_id: self.resolution_id,
		})
	}
}

impl Resolvable for ValueType
{
	type Item = Self;

	fn resolve(self) -> Result<Self::Item, Errors>
	{
		Ok(self)
	}
}

fn resolve_compared_type(
	op: ComparisonOp,
	left: &Expression,
	right: &Expression,
	location_of_op: &Location,
) -> Result<ValueType, Errors>
{
	let vt = match left.value_type()
	{
		Some(Ok(vt)) => vt,
		Some(Err(poison)) => return poison.resolve(),
		None => Err(Error::AmbiguousType {
			location: left.location().clone(),
		})?,
	};
	match right.value_type()
	{
		Some(Ok(rvt)) if rvt == vt => (),
		Some(Ok(rvt)) =>
		{
			return Err(Error::MismatchedOperandTypes {
				type_of_left: vt,
				type_of_right: rvt,
				location_of_op: location_of_op.clone(),
				location_of_left: left.location().clone(),
				location_of_right: right.location().clone(),
			}
			.into())
		}
		Some(Err(poison)) => return poison.resolve(),
		None => Err(Error::AmbiguousType {
			location: right.location().clone(),
		})?,
	}

	let value_type = analyze_operand_type(
		vt,
		op.valid_types(),
		location_of_op,
		left.location(),
	)?;
	Ok(value_type)
}

const VALID_TYPES_FOR_EQUALITY: [OperandValueType; 14] = [
	OperandValueType::ValueType(ValueType::Int8),
	OperandValueType::ValueType(ValueType::Int16),
	OperandValueType::ValueType(ValueType::Int32),
	OperandValueType::ValueType(ValueType::Int64),
	OperandValueType::ValueType(ValueType::Int128),
	OperandValueType::ValueType(ValueType::Uint8),
	OperandValueType::ValueType(ValueType::Uint16),
	OperandValueType::ValueType(ValueType::Uint32),
	OperandValueType::ValueType(ValueType::Uint64),
	OperandValueType::ValueType(ValueType::Uint128),
	OperandValueType::ValueType(ValueType::Usize),
	OperandValueType::ValueType(ValueType::Bool),
	OperandValueType::ValueType(ValueType::Char),
	OperandValueType::Pointer,
];

const VALID_TYPES_FOR_IS_GREATER: [OperandValueType; 13] = [
	OperandValueType::ValueType(ValueType::Int8),
	OperandValueType::ValueType(ValueType::Int16),
	OperandValueType::ValueType(ValueType::Int32),
	OperandValueType::ValueType(ValueType::Int64),
	OperandValueType::ValueType(ValueType::Int128),
	OperandValueType::ValueType(ValueType::Uint8),
	OperandValueType::ValueType(ValueType::Uint16),
	OperandValueType::ValueType(ValueType::Uint32),
	OperandValueType::ValueType(ValueType::Uint64),
	OperandValueType::ValueType(ValueType::Uint128),
	OperandValueType::ValueType(ValueType::Usize),
	OperandValueType::ValueType(ValueType::Bool),
	OperandValueType::ValueType(ValueType::Char),
];

impl ComparisonOp
{
	fn valid_types(&self) -> &'static [OperandValueType]
	{
		match self
		{
			ComparisonOp::Equals => &VALID_TYPES_FOR_EQUALITY,
			ComparisonOp::DoesNotEqual => &VALID_TYPES_FOR_EQUALITY,
			ComparisonOp::IsGreater => &VALID_TYPES_FOR_IS_GREATER,
			ComparisonOp::IsLess => &VALID_TYPES_FOR_IS_GREATER,
			ComparisonOp::IsGE => &VALID_TYPES_FOR_IS_GREATER,
			ComparisonOp::IsLE => &VALID_TYPES_FOR_IS_GREATER,
		}
	}
}

fn resolve_binary_op_type(
	op: BinaryOp,
	left: &Expression,
	right: &Expression,
	location_of_op: &Location,
) -> Result<ValueType, Errors>
{
	let vt = match left.value_type()
	{
		Some(Ok(vt)) => vt,
		Some(Err(poison)) => return poison.resolve(),
		None => Err(Error::AmbiguousType {
			location: left.location().clone(),
		})?,
	};
	match right.value_type()
	{
		Some(Ok(rvt)) if rvt == vt => (),
		Some(Ok(rvt)) =>
		{
			return Err(Error::MismatchedOperandTypes {
				type_of_left: vt,
				type_of_right: rvt,
				location_of_op: location_of_op.clone(),
				location_of_left: left.location().clone(),
				location_of_right: right.location().clone(),
			}
			.into())
		}
		Some(Err(poison)) => return poison.resolve(),
		None => Err(Error::AmbiguousType {
			location: right.location().clone(),
		})?,
	}

	let value_type = analyze_operand_type(
		vt,
		op.valid_types(),
		location_of_op,
		left.location(),
	)?;
	Ok(value_type)
}

const VALID_TYPES_FOR_ARITHMETIC: [OperandValueType; 11] = [
	OperandValueType::ValueType(ValueType::Int8),
	OperandValueType::ValueType(ValueType::Int16),
	OperandValueType::ValueType(ValueType::Int32),
	OperandValueType::ValueType(ValueType::Int64),
	OperandValueType::ValueType(ValueType::Int128),
	OperandValueType::ValueType(ValueType::Uint8),
	OperandValueType::ValueType(ValueType::Uint16),
	OperandValueType::ValueType(ValueType::Uint32),
	OperandValueType::ValueType(ValueType::Uint64),
	OperandValueType::ValueType(ValueType::Uint128),
	OperandValueType::ValueType(ValueType::Usize),
];
const VALID_TYPES_FOR_BITWISE: [OperandValueType; 5] = [
	OperandValueType::ValueType(ValueType::Uint8),
	OperandValueType::ValueType(ValueType::Uint16),
	OperandValueType::ValueType(ValueType::Uint32),
	OperandValueType::ValueType(ValueType::Uint64),
	OperandValueType::ValueType(ValueType::Uint128),
];
const VALID_TYPES_FOR_BITSHIFT: [OperandValueType; 5] = [
	OperandValueType::ValueType(ValueType::Uint8),
	OperandValueType::ValueType(ValueType::Uint16),
	OperandValueType::ValueType(ValueType::Uint32),
	OperandValueType::ValueType(ValueType::Uint64),
	OperandValueType::ValueType(ValueType::Uint128),
];

impl BinaryOp
{
	fn valid_types(&self) -> &'static [OperandValueType]
	{
		match self
		{
			BinaryOp::Add => &VALID_TYPES_FOR_ARITHMETIC,
			BinaryOp::Subtract => &VALID_TYPES_FOR_ARITHMETIC,
			BinaryOp::Multiply => &VALID_TYPES_FOR_ARITHMETIC,
			BinaryOp::Divide => &VALID_TYPES_FOR_ARITHMETIC,
			BinaryOp::Modulo => &VALID_TYPES_FOR_ARITHMETIC,
			BinaryOp::BitwiseAnd => &VALID_TYPES_FOR_BITWISE,
			BinaryOp::BitwiseOr => &VALID_TYPES_FOR_BITWISE,
			BinaryOp::BitwiseXor => &VALID_TYPES_FOR_BITWISE,
			BinaryOp::ShiftLeft => &VALID_TYPES_FOR_BITSHIFT,
			BinaryOp::ShiftRight => &VALID_TYPES_FOR_BITSHIFT,
		}
	}
}

fn resolve_unary_op_type(
	op: UnaryOp,
	operand: &Expression,
	location_of_op: &Location,
) -> Result<ValueType, Errors>
{
	let vt = match operand.value_type()
	{
		Some(Ok(vt)) => vt,
		Some(Err(poison)) => return poison.resolve(),
		None => Err(Error::AmbiguousType {
			location: operand.location().clone(),
		})?,
	};

	let value_type = analyze_operand_type(
		vt,
		op.valid_types(),
		location_of_op,
		operand.location(),
	)?;
	Ok(value_type)
}

const VALID_TYPES_FOR_NEGATIVE: [OperandValueType; 5] = [
	OperandValueType::ValueType(ValueType::Int8),
	OperandValueType::ValueType(ValueType::Int16),
	OperandValueType::ValueType(ValueType::Int32),
	OperandValueType::ValueType(ValueType::Int64),
	OperandValueType::ValueType(ValueType::Int128),
];
const VALID_TYPES_FOR_COMPLEMENT: [OperandValueType; 6] = [
	OperandValueType::ValueType(ValueType::Bool),
	OperandValueType::ValueType(ValueType::Uint8),
	OperandValueType::ValueType(ValueType::Uint16),
	OperandValueType::ValueType(ValueType::Uint32),
	OperandValueType::ValueType(ValueType::Uint64),
	OperandValueType::ValueType(ValueType::Uint128),
];

impl UnaryOp
{
	fn valid_types(&self) -> &'static [OperandValueType]
	{
		match self
		{
			UnaryOp::Negative => &VALID_TYPES_FOR_NEGATIVE,
			UnaryOp::BitwiseComplement => &VALID_TYPES_FOR_COMPLEMENT,
		}
	}
}

fn analyze_operand_type(
	value_type: ValueType,
	valid_types: &[OperandValueType],
	location_of_op: &Location,
	location_of_operand: &Location,
) -> Result<ValueType, Error>
{
	let is_valid = valid_types.iter().any(|valid_type| match valid_type
	{
		OperandValueType::ValueType(vt) if vt == &value_type => true,
		OperandValueType::ValueType(_) => false,
		OperandValueType::Pointer => match &value_type
		{
			ValueType::Pointer { .. } => true,
			_ => false,
		},
	});
	if is_valid
	{
		Ok(value_type)
	}
	else
	{
		Err(Error::InvalidOperandType {
			value_type,
			possible_types: valid_types.to_vec(),
			location_of_op: location_of_op.clone(),
			location_of_operand: location_of_operand.clone(),
		})?
	}
}

fn analyze_primitive_cast(
	expression: &Expression,
	coerced_type: ValueType,
	location_of_type: &Location,
) -> Result<ValueType, Errors>
{
	let value_type = match expression.value_type()
	{
		Some(Ok(vt)) => vt,
		Some(Err(poison)) => return poison.resolve(),
		None => Err(Error::AmbiguousType {
			location: expression.location().clone(),
		})?,
	};
	if is_valid_primitive_cast(&value_type, &coerced_type)
	{
		Ok(value_type)
	}
	else
	{
		let possible_value_types = VALID_PRIMITIVE_TYPES
			.iter()
			.filter(|vt| vt != &&coerced_type)
			.filter(|vt| is_valid_primitive_cast(vt, &coerced_type))
			.map(|x| OperandValueType::ValueType(x.clone()))
			.collect();
		let possible_coerced_types = VALID_PRIMITIVE_TYPES
			.iter()
			.filter(|ct| ct != &&value_type)
			.filter(|ct| is_valid_primitive_cast(&value_type, ct))
			.map(|x| OperandValueType::ValueType(x.clone()))
			.collect();
		Err(Error::InvalidPrimitiveCast {
			value_type,
			coerced_type,
			possible_value_types,
			possible_coerced_types,
			location_of_operand: expression.location().clone(),
			location_of_type: location_of_type.clone(),
		})?
	}
}

const VALID_PRIMITIVE_TYPES: [ValueType; 13] = [
	ValueType::Int8,
	ValueType::Int16,
	ValueType::Int32,
	ValueType::Int64,
	ValueType::Int128,
	ValueType::Uint8,
	ValueType::Uint16,
	ValueType::Uint32,
	ValueType::Uint64,
	ValueType::Uint128,
	ValueType::Usize,
	ValueType::Bool,
	ValueType::Char,
];

fn is_valid_primitive_cast(
	value_type: &ValueType,
	coerced_type: &ValueType,
) -> bool
{
	match (value_type, coerced_type)
	{
		(x, y) if x == y => true,
		(vt, ct) if vt.is_integral() && ct.is_integral() => true,
		(ValueType::Bool, ct) if ct.is_integral() => true,
		(vt, ValueType::Bool) if vt.is_integral() => true,
		(ValueType::Char, ValueType::Uint8) => true,
		(ValueType::Char, ValueType::Uint32) => true,
		(ValueType::Uint8, ValueType::Char) => true,
		(ValueType::Uint32, ValueType::Char) => true,
		(_, _) => false,
	}
}
