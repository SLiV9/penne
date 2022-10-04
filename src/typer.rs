//
// Part of penne
// Copyright (c) 2020 Sander in 't Veld
// License: MIT
//

use crate::common::*;
use crate::parser::{MAX_ADDRESS_DEPTH, MAX_REFERENCE_DEPTH};

use anyhow::anyhow;

pub const MAX_AUTODEREF_DEPTH: usize =
	MAX_REFERENCE_DEPTH + MAX_ADDRESS_DEPTH as usize;

pub fn analyze(
	program: Vec<Declaration>,
) -> Result<Vec<Declaration>, anyhow::Error>
{
	let mut typer = Typer {
		symbols: std::collections::HashMap::new(),
		functions: std::collections::HashMap::new(),
		contextual_type: None,
	};
	for declaration in &program
	{
		declare(declaration, &mut typer)?;
	}
	program.iter().map(|x| x.analyze(&mut typer)).collect()
}

struct Typer
{
	symbols: std::collections::HashMap<u32, (Identifier, ValueType)>,
	functions: std::collections::HashMap<u32, Vec<Option<ValueType>>>,
	contextual_type: Option<ValueType>,
}

impl Typer
{
	fn put_symbol(
		&mut self,
		identifier: &Identifier,
		value_type: Option<ValueType>,
	) -> Result<(), anyhow::Error>
	{
		if let Some(vt) = &value_type
		{
			match self.symbols.get_mut(&identifier.resolution_id)
			{
				Some((_, ot)) if ot == vt => Ok(()),
				Some((_, ot)) if ot.can_autoderef_into(vt) => Ok(()),
				Some((storedid, storedtype))
					if vt.can_autoderef_into(&storedtype) =>
				{
					*storedid = identifier.clone();
					*storedtype = vt.clone();
					Ok(())
				}
				Some((old_identifier, old_type)) => Err(anyhow!(
					"first occurrence {}",
					old_identifier.location.format()
				)
				.context(identifier.location.format())
				.context(format!(
					"conflicting types for '{}', first {:?} and now {:?}",
					identifier.name, old_type, vt
				))),
				None =>
				{
					self.symbols.insert(
						identifier.resolution_id,
						(identifier.clone(), vt.clone()),
					);
					Ok(())
				}
			}
		}
		else
		{
			Ok(())
		}
	}

	fn get_symbol(&self, name: &Identifier) -> Option<ValueType>
	{
		match self.symbols.get(&name.resolution_id)
		{
			Some((_, vt)) => Some(vt.clone()),
			None => None,
		}
	}

	fn get_type_of_reference(
		&self,
		reference: &Reference,
	) -> Result<Option<ValueType>, anyhow::Error>
	{
		if let Some((old_identifier, base_type)) =
			self.symbols.get(&reference.base.resolution_id)
		{
			let mut x = base_type.fully_dereferenced();
			for step in reference.steps.iter()
			{
				match step
				{
					ReferenceStep::Element { argument: _ } =>
					{
						match x.get_element_type()
						{
							Some(element_type) =>
							{
								x = element_type.fully_dereferenced();
							}
							None =>
							{
								return Err(anyhow!(
									"first occurrence {}",
									old_identifier.location.format()
								)
								.context(reference.location.format())
								.context(format!(
									"conflicting types for '{}'",
									reference.base.name,
								)));
							}
						}
					}
					ReferenceStep::Member { .. } => (),
					ReferenceStep::Autodeslice { .. } => (),
					ReferenceStep::Autoderef => (),
					ReferenceStep::Autoview => (),
				}
			}
			for _i in 0..reference.address_depth
			{
				x = ValueType::Pointer {
					deref_type: Box::new(x),
				};
			}
			Ok(Some(x))
		}
		else
		{
			Ok(None)
		}
	}

	fn declare_function_parameters(
		&mut self,
		identifier: &Identifier,
		parameters: &[Parameter],
	)
	{
		let parameter_types: Vec<Option<ValueType>> =
			parameters.iter().map(|p| p.value_type.clone()).collect();
		self.functions
			.insert(identifier.resolution_id, parameter_types);
	}

	fn analyze_function_arguments(
		&mut self,
		identifier: &Identifier,
		arguments: &[Expression],
	) -> Result<Vec<Expression>, anyhow::Error>
	{
		let parameter_types: Vec<Option<ValueType>> =
			match self.functions.get(&identifier.resolution_id)
			{
				Some(parameter_types) => parameter_types.clone(),
				None => Vec::new(),
			};
		let parameter_hints =
			parameter_types.into_iter().chain(std::iter::repeat(None));
		let arguments: Result<Vec<Expression>, anyhow::Error> = arguments
			.iter()
			.zip(parameter_hints)
			.map(|(a, p)| {
				self.contextual_type = p.clone();
				a.analyze(self)
			})
			.collect();
		arguments
	}
}

pub trait Typed
{
	fn value_type(&self) -> Option<ValueType>;
}

pub trait StaticallyTyped
{
	fn static_value_type(&self) -> ValueType;
}

impl Typed for PrimitiveLiteral
{
	fn value_type(&self) -> Option<ValueType>
	{
		Some(self.static_value_type())
	}
}

impl StaticallyTyped for PrimitiveLiteral
{
	fn static_value_type(&self) -> ValueType
	{
		match self
		{
			PrimitiveLiteral::Int8(_) => ValueType::Int8,
			PrimitiveLiteral::Int16(_) => ValueType::Int16,
			PrimitiveLiteral::Int32(_) => ValueType::Int32,
			PrimitiveLiteral::Int64(_) => ValueType::Int64,
			PrimitiveLiteral::Int128(_) => ValueType::Int128,
			PrimitiveLiteral::Uint8(_) => ValueType::Uint8,
			PrimitiveLiteral::Uint16(_) => ValueType::Uint16,
			PrimitiveLiteral::Uint32(_) => ValueType::Uint32,
			PrimitiveLiteral::Uint64(_) => ValueType::Uint64,
			PrimitiveLiteral::Uint128(_) => ValueType::Uint128,
			PrimitiveLiteral::Usize(_) => ValueType::Usize,
			PrimitiveLiteral::Bool(_) => ValueType::Bool,
		}
	}
}

fn declare(
	declaration: &Declaration,
	typer: &mut Typer,
) -> Result<(), anyhow::Error>
{
	match declaration
	{
		Declaration::Constant {
			name,
			value: _,
			value_type,
			flags: _,
		} =>
		{
			typer.put_symbol(name, Some(value_type.clone()))?;
			Ok(())
		}
		Declaration::Function {
			name,
			parameters,
			body: _,
			return_type,
			flags: _,
		}
		| Declaration::FunctionHead {
			name,
			parameters,
			return_type,
			flags: _,
		} =>
		{
			let rt_identifier = name.clone().return_value();
			typer.put_symbol(&rt_identifier, return_type.clone())?;

			let parameters: Result<Vec<Parameter>, anyhow::Error> =
				parameters.iter().map(|x| x.analyze(typer)).collect();
			let parameters = parameters?;

			typer.declare_function_parameters(name, &parameters);
			Ok(())
		}
		Declaration::PreprocessorDirective { .. } => unreachable!(),
	}
}

trait Analyzable
{
	type Item;

	fn analyze(&self, typer: &mut Typer) -> Result<Self::Item, anyhow::Error>;
}

impl Typed for Declaration
{
	fn value_type(&self) -> Option<ValueType>
	{
		match self
		{
			Declaration::Constant { value_type, .. } =>
			{
				Some(value_type.clone())
			}
			Declaration::Function { return_type, .. } => return_type.clone(),
			Declaration::FunctionHead { return_type, .. } =>
			{
				return_type.clone()
			}
			Declaration::PreprocessorDirective { .. } => unreachable!(),
		}
	}
}

impl Analyzable for Declaration
{
	type Item = Declaration;

	fn analyze(&self, typer: &mut Typer) -> Result<Self::Item, anyhow::Error>
	{
		match self
		{
			Declaration::Constant {
				name,
				value,
				value_type,
				flags,
			} =>
			{
				typer.contextual_type = Some(value_type.clone());
				let value = value.analyze(typer)?;
				let declaration = Declaration::Constant {
					name: name.clone(),
					value,
					value_type: value_type.clone(),
					flags: *flags,
				};
				Ok(declaration)
			}
			Declaration::Function {
				name,
				parameters,
				body,
				return_type,
				flags,
			} =>
			{
				if body.return_value.is_none() && return_type.is_some()
				{
					return Err(anyhow!("missing return value")
						.context(name.location.format())
						.context(format!(
							"missing return value for '{}'",
							name.name
						)));
				}

				let parameters: Result<Vec<Parameter>, anyhow::Error> =
					parameters.iter().map(|x| x.analyze(typer)).collect();
				let parameters = parameters?;

				// Pre-analyze the function body because it might contain
				// untyped declarations, e.g. "var x;", whose types won't be
				// determined in the first pass.
				typer.contextual_type = return_type.clone();
				let prebody: FunctionBody = body.analyze(typer)?;
				// Pre-analyze the statements in reverse, because there might
				// be chains of untyped declarations, e.g.
				//   var x = 1;
				//   var y = x;
				// whose types won't be determined in the forward pass.
				typer.contextual_type = return_type.clone();
				for statement in prebody.statements.iter().rev()
				{
					let _unused: Statement = statement.analyze(typer)?;
				}
				let _unused = prebody;

				typer.contextual_type = return_type.clone();
				let body = body.analyze(typer)?;
				let return_type = body.value_type();

				if body.return_value.is_some() && return_type.is_none()
				{
					return Err(anyhow!("failed to infer type")
						.context(name.location.format())
						.context(format!(
							"failed to infer return type for '{}'",
							name.name
						)));
				}

				let rt_identifier =
					body.return_value_identifier.clone().return_value();
				typer.put_symbol(&rt_identifier, return_type.clone())?;

				let function = Declaration::Function {
					name: name.clone(),
					parameters,
					body,
					return_type,
					flags: *flags,
				};
				Ok(function)
			}
			Declaration::FunctionHead {
				name,
				parameters,
				return_type,
				flags,
			} =>
			{
				let parameters: Result<Vec<Parameter>, anyhow::Error> =
					parameters.iter().map(|x| x.analyze(typer)).collect();
				let parameters = parameters?;

				let function = Declaration::FunctionHead {
					name: name.clone(),
					parameters,
					return_type: return_type.clone(),
					flags: *flags,
				};
				Ok(function)
			}
			Declaration::PreprocessorDirective { .. } => unreachable!(),
		}
	}
}

impl Analyzable for Parameter
{
	type Item = Parameter;

	fn analyze(&self, typer: &mut Typer) -> Result<Self::Item, anyhow::Error>
	{
		typer.put_symbol(&self.name, self.value_type.clone())?;
		let value_type = typer.get_symbol(&self.name);
		Ok(Parameter {
			name: self.name.clone(),
			value_type,
		})
	}
}

impl Typed for FunctionBody
{
	fn value_type(&self) -> Option<ValueType>
	{
		match &self.return_value
		{
			Some(value) => value.value_type(),
			None => None,
		}
	}
}

impl Analyzable for FunctionBody
{
	type Item = FunctionBody;

	fn analyze(&self, typer: &mut Typer) -> Result<Self::Item, anyhow::Error>
	{
		let contextual_return_type = typer.contextual_type.take();
		let statements: Result<Vec<Statement>, anyhow::Error> =
			self.statements.iter().map(|x| x.analyze(typer)).collect();
		let statements = statements?;
		let return_value = match &self.return_value
		{
			Some(value) =>
			{
				typer.contextual_type = contextual_return_type;
				let value = value.analyze(typer)?;
				typer.contextual_type = None;
				Some(value)
			}
			None => None,
		};
		Ok(FunctionBody {
			statements,
			return_value,
			return_value_identifier: self.return_value_identifier.clone(),
		})
	}
}

impl Analyzable for Block
{
	type Item = Block;

	fn analyze(&self, typer: &mut Typer) -> Result<Self::Item, anyhow::Error>
	{
		typer.contextual_type = None;
		let statements: Result<Vec<Statement>, anyhow::Error> =
			self.statements.iter().map(|x| x.analyze(typer)).collect();
		let statements = statements?;
		Ok(Block {
			statements,
			location: self.location.clone(),
		})
	}
}

impl Analyzable for Statement
{
	type Item = Statement;

	fn analyze(&self, typer: &mut Typer) -> Result<Self::Item, anyhow::Error>
	{
		typer.contextual_type = None;
		match self
		{
			Statement::Declaration {
				name,
				value: Some(value),
				value_type: Some(declared_type),
				location,
			} =>
			{
				typer.put_symbol(name, Some(declared_type.clone()))?;
				typer.contextual_type = Some(declared_type.clone());
				let value = value.analyze(typer)?;
				typer.contextual_type = None;
				let value_type = if let Some(inferred_type) = value.value_type()
				{
					typer.put_symbol(name, Some(inferred_type.clone()))?;
					if inferred_type.can_be_declared_as(declared_type)
					{
						Some(inferred_type)
					}
					else
					{
						Some(declared_type.clone())
					}
				}
				else
				{
					None
				};
				let stmt = Statement::Declaration {
					name: name.clone(),
					value: Some(value),
					value_type,
					location: location.clone(),
				};
				Ok(stmt)
			}
			Statement::Declaration {
				name,
				value: Some(value),
				value_type: None,
				location,
			} =>
			{
				typer.put_symbol(name, None)?;
				typer.contextual_type = typer.get_symbol(name);
				let value = value.analyze(typer)?;
				typer.contextual_type = None;
				let value_type = value.value_type();
				let value_type = infer_for_declaration(value_type);
				typer.put_symbol(name, value_type.clone())?;
				let stmt = Statement::Declaration {
					name: name.clone(),
					value: Some(value),
					value_type,
					location: location.clone(),
				};
				Ok(stmt)
			}
			Statement::Declaration {
				name,
				value: None,
				value_type: Some(value_type),
				location,
			} =>
			{
				typer.put_symbol(name, Some(value_type.clone()))?;
				let stmt = Statement::Declaration {
					name: name.clone(),
					value: None,
					value_type: Some(value_type.clone()),
					location: location.clone(),
				};
				Ok(stmt)
			}
			Statement::Declaration {
				name,
				value: None,
				value_type: None,
				location,
			} =>
			{
				let value_type = typer.get_symbol(name);
				let value_type = infer_for_declaration(value_type);
				let stmt = Statement::Declaration {
					name: name.clone(),
					value: None,
					value_type,
					location: location.clone(),
				};
				Ok(stmt)
			}
			Statement::Assignment {
				reference,
				value,
				location,
			} =>
			{
				let ref_type = typer.get_type_of_reference(reference)?;
				typer.contextual_type = ref_type;
				let value = value.analyze(typer)?;
				typer.contextual_type = None;
				let value_type = value.value_type();
				let reference =
					reference.analyze_assignment(value_type, typer)?;
				let stmt = Statement::Assignment {
					reference,
					value,
					location: location.clone(),
				};
				Ok(stmt)
			}
			Statement::MethodCall { name, arguments } =>
			{
				let arguments =
					typer.analyze_function_arguments(name, arguments)?;
				let stmt = Statement::MethodCall {
					name: name.clone(),
					arguments,
				};
				Ok(stmt)
			}
			Statement::Loop { location } => Ok(Statement::Loop {
				location: location.clone(),
			}),
			Statement::Goto { label, location } => Ok(Statement::Goto {
				label: label.clone(),
				location: location.clone(),
			}),
			Statement::Label { label, location } => Ok(Statement::Label {
				label: label.clone(),
				location: location.clone(),
			}),
			Statement::If {
				condition,
				then_branch,
				else_branch,
				location,
			} =>
			{
				let condition = condition.analyze(typer)?;
				let then_branch = {
					let branch = then_branch.analyze(typer)?;
					Box::new(branch)
				};
				let else_branch = if let Some(else_branch) = else_branch
				{
					let branch = else_branch.analyze(typer)?;
					Some(Box::new(branch))
				}
				else
				{
					None
				};
				let stmt = Statement::If {
					condition,
					then_branch,
					else_branch,
					location: location.clone(),
				};
				Ok(stmt)
			}
			Statement::Block(block) =>
			{
				let block = block.analyze(typer)?;
				Ok(Statement::Block(block))
			}
		}
	}
}

impl Analyzable for Comparison
{
	type Item = Comparison;

	fn analyze(&self, typer: &mut Typer) -> Result<Self::Item, anyhow::Error>
	{
		let contextual_type = typer.contextual_type.take();
		typer.contextual_type =
			self.right.value_type().or(contextual_type.clone());
		let left = self.left.analyze(typer)?;
		typer.contextual_type = left.value_type().or(contextual_type);
		let right = self.right.analyze(typer)?;
		let expr = Comparison {
			op: self.op,
			left,
			right,
			location: self.location.clone(),
		};
		Ok(expr)
	}
}

impl Analyzable for Array
{
	type Item = Array;

	fn analyze(&self, typer: &mut Typer) -> Result<Self::Item, anyhow::Error>
	{
		let name = self.get_identifier();
		let elements: Result<Vec<Expression>, anyhow::Error> = self
			.elements
			.iter()
			.map(|element| {
				let element = element.analyze(typer)?;
				let element_type = element.value_type();
				typer.contextual_type = element_type.clone();
				typer.put_symbol(&name, element_type)?;
				Ok(element)
			})
			.collect();
		let elements = elements?;

		Ok(Array {
			elements,
			location: self.location.clone(),
			resolution_id: self.resolution_id,
		})
	}
}

impl Typed for Expression
{
	fn value_type(&self) -> Option<ValueType>
	{
		match self
		{
			Expression::Binary { left, .. } => left.value_type(),
			Expression::Unary { expression, .. } => expression.value_type(),
			Expression::PrimitiveLiteral(literal) => literal.value_type(),
			Expression::NakedIntegerLiteral { value_type, .. } =>
			{
				value_type.clone()
			}
			Expression::BitIntegerLiteral { value_type, .. } =>
			{
				value_type.clone()
			}
			Expression::ArrayLiteral {
				element_type,
				array: Array { elements, .. },
			} => match element_type
			{
				Some(element_type) => Some(ValueType::Array {
					element_type: Box::new(element_type.clone()),
					length: elements.len(),
				}),
				None => None,
			},
			Expression::StringLiteral { value_type, .. } => value_type.clone(),
			Expression::Deref {
				reference: _,
				deref_type,
			} => deref_type.clone(),
			Expression::Autocoerce { coerced_type, .. } =>
			{
				Some(coerced_type.clone())
			}
			Expression::PrimitiveCast { coerced_type, .. } =>
			{
				Some(coerced_type.clone())
			}
			Expression::LengthOfArray { .. } => Some(ValueType::Usize),
			Expression::FunctionCall { return_type, .. } => return_type.clone(),
		}
	}
}

impl Analyzable for Expression
{
	type Item = Expression;

	fn analyze(&self, typer: &mut Typer) -> Result<Self::Item, anyhow::Error>
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
				let contextual_type = typer.contextual_type.take();
				typer.contextual_type =
					right.value_type().or(contextual_type.clone());
				let left = left.analyze(typer)?;
				typer.contextual_type = left.value_type().or(contextual_type);
				let right = right.analyze(typer)?;
				let expr = Expression::Binary {
					op: *op,
					left: Box::new(left),
					right: Box::new(right),
					location: location.clone(),
				};
				Ok(expr)
			}
			Expression::Unary {
				op,
				expression,
				location,
			} =>
			{
				let expr = expression.analyze(typer)?;
				let expr = Expression::Unary {
					op: *op,
					expression: Box::new(expr),
					location: location.clone(),
				};
				Ok(expr)
			}
			Expression::PrimitiveLiteral(lit) =>
			{
				Ok(Expression::PrimitiveLiteral(lit.clone()))
			}
			Expression::NakedIntegerLiteral {
				value,
				value_type,
				location,
			} =>
			{
				let value_type = match value_type
				{
					Some(vt) => Some(vt.clone()),
					None =>
					{
						let contextual_type = typer.contextual_type.take();
						filter_for_naked_integer(contextual_type)
					}
				};
				Ok(Expression::NakedIntegerLiteral {
					value: *value,
					value_type,
					location: location.clone(),
				})
			}
			Expression::BitIntegerLiteral {
				value,
				value_type,
				location,
			} =>
			{
				let value_type = match value_type
				{
					Some(vt) => Some(vt.clone()),
					None =>
					{
						let contextual_type = typer.contextual_type.take();
						filter_for_bit_integer(contextual_type)
					}
				};
				Ok(Expression::BitIntegerLiteral {
					value: *value,
					value_type,
					location: location.clone(),
				})
			}
			Expression::ArrayLiteral {
				array,
				element_type,
			} =>
			{
				let name = array.get_identifier();
				if element_type.is_some()
				{
					typer.put_symbol(&name, element_type.clone())?;
					typer.contextual_type = element_type.clone();
				}
				else
				{
					let array_type = typer.contextual_type.take();
					typer.contextual_type =
						array_type.map(|x| x.get_element_type()).flatten();
				};
				let array = array.analyze(typer)?;
				let element_type = typer.get_symbol(&name);
				Ok(Expression::ArrayLiteral {
					array,
					element_type,
				})
			}
			Expression::StringLiteral {
				value_type: Some(vt),
				..
			} =>
			{
				let contextual_type = typer.contextual_type.take();
				if let Some(ct) = contextual_type
				{
					if vt.can_coerce_into(&ct)
					{
						let expr = self.clone();
						Ok(Expression::Autocoerce {
							expression: Box::new(expr),
							coerced_type: ct,
						})
					}
					else
					{
						Ok(self.clone())
					}
				}
				else
				{
					Ok(self.clone())
				}
			}
			Expression::StringLiteral {
				bytes,
				value_type: None,
				location,
			} =>
			{
				let contextual_type = typer.contextual_type.take();
				//println!("?????? coercing string for {:?}", contextual_type);
				let (value_type, coerced_type) =
					coerce_for_string_literal(contextual_type);
				//println!("?????? from {:?}", value_type);
				//println!("?????? into {:?}", coerced_type);
				let expr = Expression::StringLiteral {
					bytes: bytes.clone(),
					value_type,
					location: location.clone(),
				};
				if let Some(coerced_type) = coerced_type
				{
					Ok(Expression::Autocoerce {
						expression: Box::new(expr),
						coerced_type,
					})
				}
				else
				{
					Ok(expr)
				}
			}
			Expression::Deref {
				reference: _,
				deref_type: Some(_),
			} => Ok(self.clone()),
			Expression::Deref {
				reference,
				deref_type: None,
			} => reference.analyze_deref_expression(typer),
			Expression::Autocoerce {
				expression: _,
				coerced_type: _,
			} => Ok(self.clone()),
			Expression::PrimitiveCast {
				expression,
				coerced_type,
				location,
			} =>
			{
				let expr = expression.analyze(typer)?;
				let expr = Expression::PrimitiveCast {
					expression: Box::new(expr),
					coerced_type: coerced_type.clone(),
					location: location.clone(),
				};
				Ok(expr)
			}
			Expression::LengthOfArray { reference } =>
			{
				let base_type = typer.get_symbol(&reference.base);
				let array_type = typer.get_type_of_reference(reference)?;
				//println!("?????? {:?}", array_type);
				match array_type
				{
					Some(ValueType::Array { .. }) =>
					{
						let reference = if base_type == array_type
						{
							reference.clone()
						}
						else
						{
							reference.analyze_length(array_type, typer)?
						};
						Ok(Expression::LengthOfArray { reference })
					}
					Some(ValueType::Slice { .. }) =>
					{
						let mut reference = if base_type == array_type
						{
							reference.clone()
						}
						else
						{
							reference.analyze_length(array_type, typer)?
						};
						let autostep = ReferenceStep::Autodeslice { offset: 1 };
						//println!("######\t\t taking {:?}", autostep);
						reference.steps.push(autostep);
						let expr = Expression::Deref {
							reference,
							deref_type: Some(ValueType::Usize),
						};
						Ok(expr)
					}
					Some(_) =>
					{
						let error = anyhow!("can only take length of array")
							.context(reference.location.format())
							.context("this variable does not have a length");
						Err(error)
					}
					None => Ok(self.clone()),
				}
			}
			Expression::FunctionCall {
				name,
				arguments,
				return_type,
			} =>
			{
				let rt_identifier = name.clone().return_value();
				typer.put_symbol(&rt_identifier, return_type.clone())?;
				let return_type = typer.get_symbol(&rt_identifier);
				let arguments =
					typer.analyze_function_arguments(name, arguments)?;
				let expr = Expression::FunctionCall {
					name: name.clone(),
					arguments,
					return_type,
				};
				Ok(expr)
			}
		}
	}
}

impl Analyzable for ReferenceStep
{
	type Item = ReferenceStep;

	fn analyze(&self, typer: &mut Typer) -> Result<Self::Item, anyhow::Error>
	{
		match self
		{
			ReferenceStep::Element { argument } =>
			{
				typer.contextual_type = Some(ValueType::Usize);
				let argument = argument.analyze(typer)?;
				typer.contextual_type = None;
				Ok(ReferenceStep::Element {
					argument: Box::new(argument),
				})
			}
			ReferenceStep::Member { member: _ } => Ok(self.clone()),
			ReferenceStep::Autodeslice { offset: _ } => Ok(self.clone()),
			ReferenceStep::Autoderef => Ok(ReferenceStep::Autoderef),
			ReferenceStep::Autoview => Ok(ReferenceStep::Autoview),
		}
	}
}

impl Reference
{
	fn analyze_length(
		&self,
		value_type: Option<ValueType>,
		typer: &mut Typer,
	) -> Result<Self, anyhow::Error>
	{
		self.analyze_assignment(value_type, typer)
	}

	fn analyze_assignment(
		&self,
		value_type: Option<ValueType>,
		typer: &mut Typer,
	) -> Result<Self, anyhow::Error>
	{
		let steps: Result<Vec<ReferenceStep>, anyhow::Error> =
			self.steps.iter().map(|step| step.analyze(typer)).collect();
		let steps: Vec<ReferenceStep> = steps?;

		//println!("###### {:?}", self.location.format());

		let base_type = typer.get_symbol(&self.base);
		//println!("######\t knowing {:?}", base_type);

		let mut address_depth = self.address_depth;
		let steps = if let Some(base_type) = base_type
		{
			let previous_steps = steps;
			let mut steps = Vec::new();

			let mut current_type = base_type;
			for step in previous_steps.into_iter()
			{
				match step
				{
					ReferenceStep::Element { argument: _ } =>
					{
						for _i in 0..MAX_AUTODEREF_DEPTH
						{
							match current_type
							{
								ValueType::Pointer { deref_type } =>
								{
									let step = ReferenceStep::Autoderef;
									//println!("######\t\t taking {:?}", step);
									steps.push(step);
									current_type = *deref_type;
								}
								ValueType::View { deref_type } =>
								{
									let step = ReferenceStep::Autoview;
									//println!("######\t\t taking {:?}", step);
									steps.push(step);
									current_type = *deref_type;
								}
								_ => break,
							}
						}
						match current_type
						{
							ValueType::Slice { .. } =>
							{
								let autostep =
									ReferenceStep::Autodeslice { offset: 0 };
								//println!("######\t\t taking {:?}", autostep);
								steps.push(autostep);
							}
							_ => (),
						}
						match current_type.get_element_type()
						{
							Some(element_type) =>
							{
								current_type = element_type;
							}
							None => unreachable!(),
						}
					}
					ReferenceStep::Member { member: _ } => unimplemented!(),
					ReferenceStep::Autoderef => match current_type
					{
						ValueType::Pointer { deref_type } =>
						{
							current_type = *deref_type;
						}
						_ => unreachable!(),
					},
					ReferenceStep::Autoview => match current_type
					{
						ValueType::View { deref_type } =>
						{
							current_type = *deref_type;
						}
						_ => unreachable!(),
					},
					ReferenceStep::Autodeslice { offset: 0 } => (),
					ReferenceStep::Autodeslice { offset: 1 } =>
					{
						current_type = ValueType::Usize;
					}
					ReferenceStep::Autodeslice { offset: _ } => unreachable!(),
				}
				steps.push(step);
			}
			for _i in 0..MAX_AUTODEREF_DEPTH
			{
				match current_type
				{
					ct if value_type.as_ref() == Some(&ct) =>
					{
						address_depth = 0;
						break;
					}
					ValueType::Pointer { deref_type } =>
					{
						let step = ReferenceStep::Autoderef;
						//println!("######\t\t taking {:?}", step);
						steps.push(step);
						current_type = *deref_type;
					}
					_ => break,
				}
			}
			steps
		}
		else
		{
			steps
		};

		//println!("######\t storing {:?}", value_type);
		let full_type =
			build_type_of_reference(value_type, &steps, address_depth);
		//println!("######\t\t in {:?}", full_type);
		typer.put_symbol(&self.base, full_type)?;

		Ok(Reference {
			base: self.base.clone(),
			steps,
			address_depth,
			location: self.location.clone(),
		})
	}

	fn analyze_deref_expression(
		&self,
		typer: &mut Typer,
	) -> Result<Expression, anyhow::Error>
	{
		let steps: Result<Vec<ReferenceStep>, anyhow::Error> =
			self.steps.iter().map(|step| step.analyze(typer)).collect();
		let steps = steps?;

		//println!("###### {:?}", self.location.format());
		let known_type = typer.get_type_of_reference(self)?;
		//println!("######\t knowing {:?}", known_type);
		let ref_type = typer.get_symbol(&self.base);
		//println!("######\t\t from {:?}", ref_type);
		let contextual_type = typer.contextual_type.take();
		//println!("######\t\t and expecting {:?}", contextual_type);

		match (known_type, contextual_type, ref_type)
		{
			(Some(x), Some(y), Some(ref_type)) if x == y =>
			{
				//println!("######\t\t with match {:?}", x);
				self.autoderef(ref_type, y, steps, typer)
			}
			(Some(x), Some(y), _) if x == y =>
			{
				//println!("######\t\t matched {:?}", x);
				let base_type = Some(x);
				let deref_type = Some(y);
				let full_type = build_type_of_reference(
					base_type,
					&steps,
					self.address_depth,
				);
				//println!("######\t\t from {:?}", full_type);
				typer.put_symbol(&self.base, full_type.clone())?;
				Ok(Expression::Deref {
					reference: Reference {
						base: self.base.clone(),
						steps,
						address_depth: self.address_depth,
						location: self.location.clone(),
					},
					deref_type,
				})
			}
			(Some(x), Some(y), Some(ref_type)) if x.can_autoderef_into(&y) =>
			{
				self.autoderef(ref_type, y, steps, typer)
			}
			(Some(def), None, Some(ref_type)) =>
			{
				//println!("######\t\t with default {:?}", def);
				self.autoderef(ref_type, def, steps, typer)
			}
			(Some(def), _, _) =>
			{
				//println!("######\t\t defaulting {:?}", def);
				let base_type = Some(def);
				let deref_type = base_type.clone();
				let full_type = build_type_of_reference(
					base_type,
					&steps,
					self.address_depth,
				);
				//println!("######\t\t from {:?}", full_type);
				typer.put_symbol(&self.base, full_type.clone())?;
				Ok(Expression::Deref {
					reference: Reference {
						base: self.base.clone(),
						steps,
						address_depth: self.address_depth,
						location: self.location.clone(),
					},
					deref_type,
				})
			}
			(None, deref_type, _) =>
			{
				//println!("######\t\t getting {:?}", deref_type);
				let base_type = deref_type.clone();
				let full_type = build_type_of_reference(
					base_type,
					&steps,
					self.address_depth,
				);
				//println!("######\t\t from {:?}", full_type);
				typer.put_symbol(&self.base, full_type.clone())?;
				Ok(Expression::Deref {
					reference: Reference {
						base: self.base.clone(),
						steps,
						address_depth: self.address_depth,
						location: self.location.clone(),
					},
					deref_type,
				})
			}
		}
	}

	fn autoderef(
		&self,
		known_ref_type: ValueType,
		target_type: ValueType,
		steps: Vec<ReferenceStep>,
		typer: &mut Typer,
	) -> Result<Expression, anyhow::Error>
	{
		//println!("######\t\t autoderef into {:?}", target_type);

		let mut available_steps = steps.into_iter().peekable();
		let mut taken_steps = Vec::new();
		let mut current_type = known_ref_type;
		let mut coercion = None;
		let mut take_address = false;

		for _i in 0..MAX_AUTODEREF_DEPTH
		{
			match (current_type, &target_type, available_steps.peek())
			{
				(ct, tt, None) if &ct == tt =>
				{
					break;
				}
				(ct, tt, None) if ct.can_coerce_into(tt) =>
				{
					coercion = Some((ct, tt.clone()));
					break;
				}
				(ct, ValueType::Pointer { deref_type }, None)
					if &ct == deref_type.as_ref()
						&& self.address_depth > 0
						&& !take_address =>
				{
					//println!("######\t\t taking address");
					take_address = true;
					break;
				}
				(ct, ValueType::Pointer { deref_type }, None)
					if ct.can_coerce_into(deref_type.as_ref())
						&& self.address_depth > 0
						&& !take_address =>
				{
					//println!("######\t\t taking address");
					take_address = true;
					current_type = ValueType::Pointer {
						deref_type: Box::new(ct),
					};
					coercion = Some((current_type, target_type.clone()));
					break;
				}
				(
					ValueType::Pointer { deref_type },
					_,
					Some(ReferenceStep::Element { argument }),
				) => match deref_type.as_ref()
				{
					ValueType::ExtArray { element_type } =>
					{
						let step = ReferenceStep::Element {
							argument: argument.clone(),
						};
						//println!("######\t\t taking {:?}", step);
						taken_steps.push(step);
						available_steps.next();
						current_type = element_type.as_ref().clone();
					}
					_ =>
					{
						let step = ReferenceStep::Autoderef;
						//println!("######\t\t taking {:?}", step);
						taken_steps.push(step);
						current_type = *deref_type;
					}
				},
				(
					ValueType::View { deref_type },
					_,
					Some(ReferenceStep::Element { argument }),
				) => match deref_type.as_ref()
				{
					ValueType::ExtArray { element_type } =>
					{
						let step = ReferenceStep::Element {
							argument: argument.clone(),
						};
						//println!("######\t\t taking {:?}", step);
						taken_steps.push(step);
						available_steps.next();
						current_type = element_type.as_ref().clone();
					}
					_ =>
					{
						let step = ReferenceStep::Autoview;
						//println!("######\t\t taking {:?}", step);
						taken_steps.push(step);
						current_type = *deref_type;
					}
				},
				(ValueType::Pointer { deref_type }, _, _) =>
				{
					let step = ReferenceStep::Autoderef;
					//println!("######\t\t taking {:?}", step);
					taken_steps.push(step);
					current_type = *deref_type;
				}
				(ValueType::View { deref_type }, _, _) =>
				{
					let step = ReferenceStep::Autoview;
					//println!("######\t\t taking {:?}", step);
					taken_steps.push(step);
					current_type = *deref_type;
				}
				(
					ValueType::Array {
						element_type,
						length: _,
					},
					_,
					Some(ReferenceStep::Element { argument }),
				) =>
				{
					let step = ReferenceStep::Element {
						argument: argument.clone(),
					};
					//println!("######\t\t taking {:?}", step);
					taken_steps.push(step);
					available_steps.next();
					current_type = *element_type;
				}
				(
					ValueType::Slice { element_type },
					_,
					Some(ReferenceStep::Element { argument }),
				) =>
				{
					let autostep = ReferenceStep::Autodeslice { offset: 0 };
					//println!("######\t\t taking {:?}", autostep);
					taken_steps.push(autostep);

					let step = ReferenceStep::Element {
						argument: argument.clone(),
					};
					//println!("######\t\t taking {:?}", step);
					taken_steps.push(step);
					available_steps.next();
					current_type = *element_type;
				}
				(_, _, None) if self.address_depth >= 2 =>
				{
					// This is invalid, but we want a proper error, not an
					// autoderef failure, so just skip the autoderef.
					//println!("######\t\t bailing out");
					let base_type = Some(target_type.clone());
					let full_type = build_type_of_reference(
						base_type,
						&taken_steps,
						self.address_depth,
					);
					typer.put_symbol(&self.base, full_type.clone())?;
					let expr = Expression::Deref {
						reference: Reference {
							base: self.base.clone(),
							steps: taken_steps,
							address_depth: self.address_depth,
							location: self.location.clone(),
						},
						deref_type: Some(target_type),
					};
					return Ok(expr);
				}
				(ct, tt, step) =>
				{
					return Err(anyhow!("failed to autoderef")
						.context(format!("target type: {:?}", tt))
						.context(format!("current type: {:?}", ct))
						.context(format!("available step: {:?}", step))
						.context(format!("ad: {:?}", self.address_depth))
						.context(format!("taken address: {:?}", take_address))
						.context(self.location.format())
						.context("failed to infer type of expression"));
				}
			}
		}

		let address_depth = if take_address { 1 } else { 0 };
		let (deref_type, coerced_type) = match coercion
		{
			Some((x, y)) => (Some(x), Some(y)),
			None => (Some(target_type), None),
		};
		let base_type = deref_type.clone();
		let full_type =
			build_type_of_reference(base_type, &taken_steps, address_depth);
		//println!("######\t\t coercing {:?}", coerced_type);
		//println!("######\t\t from {:?}", full_type);
		typer.put_symbol(&self.base, full_type.clone())?;
		let expr = Expression::Deref {
			reference: Reference {
				base: self.base.clone(),
				steps: taken_steps,
				address_depth,
				location: self.location.clone(),
			},
			deref_type,
		};
		if let Some(coerced_type) = coerced_type
		{
			Ok(Expression::Autocoerce {
				expression: Box::new(expr),
				coerced_type,
			})
		}
		else
		{
			Ok(expr)
		}
	}
}

fn build_type_of_reference(
	base_type: Option<ValueType>,
	steps: &[ReferenceStep],
	address_depth: u8,
) -> Option<ValueType>
{
	if let Some(base_type) = base_type
	{
		let mut full_type = base_type;
		for step in steps.iter().rev()
		{
			match step
			{
				ReferenceStep::Element { argument: _ } =>
				{
					full_type = ValueType::ExtArray {
						element_type: Box::new(full_type),
					};
				}
				ReferenceStep::Member { member: _ } => unimplemented!(),
				ReferenceStep::Autoderef =>
				{
					full_type = ValueType::Pointer {
						deref_type: Box::new(full_type),
					};
				}
				ReferenceStep::Autoview =>
				{
					full_type = ValueType::View {
						deref_type: Box::new(full_type),
					};
				}
				ReferenceStep::Autodeslice { offset: _ } => (),
			}
		}
		for _i in 0..address_depth
		{
			match full_type
			{
				ValueType::Pointer { deref_type } =>
				{
					full_type = *deref_type;
				}
				_ => break,
			}
		}
		Some(full_type)
	}
	else
	{
		None
	}
}

fn infer_for_declaration(value_type: Option<ValueType>) -> Option<ValueType>
{
	match value_type
	{
		Some(ValueType::Pointer { .. }) => None,
		Some(vt) => Some(vt),
		None => None,
	}
}

fn filter_for_naked_integer(value_type: Option<ValueType>)
	-> Option<ValueType>
{
	match value_type
	{
		Some(ValueType::Int8) => value_type,
		Some(ValueType::Int16) => value_type,
		Some(ValueType::Int32) => value_type,
		Some(ValueType::Int64) => value_type,
		Some(ValueType::Int128) => value_type,
		Some(ValueType::Uint8) => value_type,
		Some(ValueType::Uint16) => value_type,
		Some(ValueType::Uint32) => value_type,
		Some(ValueType::Uint64) => value_type,
		Some(ValueType::Uint128) => value_type,
		Some(ValueType::Usize) => value_type,
		Some(_) => Some(ValueType::Int32),
		None => None,
	}
}

fn filter_for_bit_integer(value_type: Option<ValueType>) -> Option<ValueType>
{
	match value_type
	{
		Some(ValueType::Uint8) => value_type,
		Some(ValueType::Uint16) => value_type,
		Some(ValueType::Uint32) => value_type,
		Some(ValueType::Uint64) => value_type,
		Some(ValueType::Usize) => value_type,
		Some(ValueType::Pointer { .. }) => value_type,
		Some(ValueType::View { .. }) => value_type,
		Some(..) => Some(ValueType::Uint64),
		None => None,
	}
}

fn coerce_for_string_literal(
	value_type: Option<ValueType>,
) -> (Option<ValueType>, Option<ValueType>)
{
	match value_type
	{
		Some(ValueType::String) => (value_type, None),
		Some(vt) if vt == ValueType::for_byte_string() =>
		{
			(Some(vt.clone()), Some(vt))
		}
		Some(vt) if ValueType::for_byte_string().can_coerce_into(&vt) =>
		{
			(Some(ValueType::for_byte_string()), Some(vt))
		}
		Some(_) => (Some(ValueType::String), None),
		None => (None, None),
	}
}
