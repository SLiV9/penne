/**/

use crate::common::*;

use anyhow::anyhow;

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

	fn get_element_type_of_array(
		&self,
		name: &Identifier,
	) -> Result<Option<ValueType>, anyhow::Error>
	{
		match self.symbols.get(&name.resolution_id)
		{
			Some((old_identifier, other_type)) =>
			{
				if let Some(element_type) = other_type.get_element_type()
				{
					Ok(Some(element_type))
				}
				else
				{
					Err(anyhow!(
						"first occurrence {}",
						old_identifier.location.format()
					)
					.context(name.location.format())
					.context(format!(
						"conflicting types for '{}', got {:?} expected array",
						name.name, other_type,
					)))
				}
			}
			None => Ok(None),
		}
	}

	fn get_type_of_reference(
		&self,
		reference: &Reference,
	) -> Result<Option<ValueType>, anyhow::Error>
	{
		match reference
		{
			Reference::Identifier(identifier) =>
			{
				Ok(self.get_symbol(identifier))
			}
			Reference::ArrayElement { name, .. } =>
			{
				self.get_element_type_of_array(name)
			}
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
				value_type,
				location,
			} =>
			{
				typer.put_symbol(name, value_type.clone())?;
				typer.contextual_type = typer.get_symbol(name);
				let value = value.analyze(typer)?;
				typer.contextual_type = None;
				let value_type = value.value_type();
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
				typer.contextual_type = match reference
				{
					Reference::Identifier(name) => typer.get_symbol(name),
					Reference::ArrayElement { name, argument: _ } =>
					{
						typer.get_element_type_of_array(name)?
					}
				};
				let value = value.analyze(typer)?;
				typer.contextual_type = None;
				let value_type = value.value_type();
				let reference = match reference
				{
					Reference::Identifier(name) =>
					{
						typer.put_symbol(name, value_type)?;
						Reference::Identifier(name.clone())
					}
					Reference::ArrayElement { name, argument } =>
					{
						typer.contextual_type = Some(ValueType::Usize);
						let argument = argument.analyze(typer)?;
						let array_type =
							value_type.map(|vt| ValueType::ExtArray {
								element_type: Box::new(vt),
							});
						typer.put_symbol(name, array_type)?;
						Reference::ArrayElement {
							name: name.clone(),
							argument: Box::new(argument),
						}
					}
				};
				let stmt = Statement::Assignment {
					reference,
					value,
					location: location.clone(),
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
			Expression::StringLiteral(_literal) => None,
			Expression::Deref {
				reference: _,
				ref_type: _,
				deref_type,
			} => deref_type.clone(),
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
						match contextual_type
						{
							Some(ValueType::Int8) => contextual_type,
							Some(ValueType::Int16) => contextual_type,
							Some(ValueType::Int32) => contextual_type,
							Some(ValueType::Int64) => contextual_type,
							Some(ValueType::Int128) => contextual_type,
							Some(ValueType::Uint8) => contextual_type,
							Some(ValueType::Uint16) => contextual_type,
							Some(ValueType::Uint32) => contextual_type,
							Some(ValueType::Uint64) => contextual_type,
							Some(ValueType::Uint128) => contextual_type,
							Some(ValueType::Usize) => contextual_type,
							Some(ValueType::Bool) => Some(ValueType::Int32),
							Some(ValueType::Array { .. }) =>
							{
								Some(ValueType::Int32)
							}
							Some(ValueType::Slice { .. }) =>
							{
								Some(ValueType::Int32)
							}
							Some(ValueType::ExtArray { .. }) =>
							{
								Some(ValueType::Int32)
							}
							Some(ValueType::Pointer { .. }) =>
							{
								Some(ValueType::Int32)
							}
							Some(ValueType::View { .. }) =>
							{
								Some(ValueType::Int32)
							}
							None => None,
						}
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
						match contextual_type
						{
							Some(ValueType::Int8) => Some(ValueType::Uint64),
							Some(ValueType::Int16) => Some(ValueType::Uint64),
							Some(ValueType::Int32) => Some(ValueType::Uint64),
							Some(ValueType::Int64) => Some(ValueType::Uint64),
							Some(ValueType::Int128) => Some(ValueType::Uint64),
							Some(ValueType::Uint8) => contextual_type,
							Some(ValueType::Uint16) => contextual_type,
							Some(ValueType::Uint32) => contextual_type,
							Some(ValueType::Uint64) => contextual_type,
							Some(ValueType::Uint128) => Some(ValueType::Uint64),
							Some(ValueType::Usize) => contextual_type,
							Some(ValueType::Bool) => Some(ValueType::Uint64),
							Some(ValueType::Array { .. }) =>
							{
								Some(ValueType::Uint64)
							}
							Some(ValueType::Slice { .. }) =>
							{
								Some(ValueType::Uint64)
							}
							Some(ValueType::ExtArray { .. }) =>
							{
								Some(ValueType::Uint64)
							}
							Some(ValueType::Pointer { .. }) => contextual_type,
							Some(ValueType::View { .. }) => contextual_type,
							None => None,
						}
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
			Expression::StringLiteral(lit) =>
			{
				Ok(Expression::StringLiteral(lit.clone()))
			}
			Expression::Deref {
				reference: _,
				ref_type: Some(_),
				deref_type: Some(_),
			} => Ok(self.clone()),
			Expression::Deref {
				reference: Reference::Identifier(name),
				ref_type: _,
				deref_type: _,
			} =>
			{
				let known_type = typer.get_symbol(name);
				let contextual_type = typer.contextual_type.take();
				let (ref_type, deref_type) =
					autoderef(known_type, contextual_type, 0);
				typer.put_symbol(name, ref_type.clone())?;
				Ok(Expression::Deref {
					reference: Reference::Identifier(name.clone()),
					ref_type,
					deref_type,
				})
			}
			Expression::Deref {
				reference: Reference::ArrayElement { name, argument },
				ref_type: _,
				deref_type: _,
			} =>
			{
				let known_type = typer.get_symbol(name);
				let contextual_type = typer.contextual_type.take();
				let (ref_type, deref_type) =
					autoderef(known_type, contextual_type, 1);
				typer.put_symbol(name, ref_type.clone())?;
				typer.contextual_type = Some(ValueType::Usize);
				let argument = argument.analyze(typer)?;
				let expr = Expression::Deref {
					reference: Reference::ArrayElement {
						name: name.clone(),
						argument: Box::new(argument),
					},
					ref_type,
					deref_type,
				};
				Ok(expr)
			}
			Expression::LengthOfArray { reference } =>
			{
				let array_type = typer.get_type_of_reference(reference)?;
				match array_type
				{
					Some(ValueType::Array { .. }) =>
					{
						Ok(Expression::LengthOfArray {
							reference: reference.clone(),
						})
					}
					Some(ValueType::Slice { .. }) =>
					{
						Ok(Expression::LengthOfArray {
							reference: reference.clone(),
						})
					}
					Some(_) =>
					{
						let error = anyhow!("can only take length of array")
							.context(reference.location().format())
							.context("this variable does not have a length");
						Err(error)
					}
					None => Ok(Expression::LengthOfArray {
						reference: reference.clone(),
					}),
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

fn autoderef(
	known_type: Option<ValueType>,
	contextual_type: Option<ValueType>,
	array_depth: usize,
) -> (Option<ValueType>, Option<ValueType>)
{
	if array_depth > 0
	{
		let slice_type = match contextual_type.clone()
		{
			Some(vt) => Some(ValueType::ExtArray {
				element_type: Box::new(vt),
			}),
			None => None,
		};
		let backup_slice_type = slice_type.clone();
		let (ref_type, slice_deref_type) =
			autoderef(known_type, slice_type, array_depth - 1);
		if let Some(slice_deref_type) = slice_deref_type
		{
			match slice_deref_type.get_element_type()
			{
				Some(deref_type) => (ref_type, Some(deref_type)),
				None => (backup_slice_type, contextual_type),
			}
		}
		else
		{
			(None, None)
		}
	}
	else
	{
		match (known_type, contextual_type)
		{
			(Some(x), Some(y)) if x == y => (Some(x), Some(y)),
			(Some(x), Some(y)) if x.can_autoderef_into(&y) =>
			{
				(Some(x), Some(y))
			}
			(Some(vt), _) => (Some(vt.clone()), Some(vt)),
			(None, Some(vt)) => (Some(vt.clone()), Some(vt)),
			(None, None) => (None, None),
		}
	}
}
