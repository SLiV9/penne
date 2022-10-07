//
// Part of penne
// Copyright (c) 2020 Sander in 't Veld
// License: MIT
//

use crate::common::*;
use crate::error::Error;
use crate::parser::{MAX_ADDRESS_DEPTH, MAX_REFERENCE_DEPTH};

pub const MAX_AUTODEREF_DEPTH: usize =
	MAX_REFERENCE_DEPTH + MAX_ADDRESS_DEPTH as usize;

pub fn analyze(program: Vec<Declaration>) -> Vec<Declaration>
{
	let mut typer = Typer {
		symbols: std::collections::HashMap::new(),
		functions: std::collections::HashMap::new(),
		contextual_type: None,
	};
	// Predeclare all functions so that they can reference each other.
	let declarations: Vec<Declaration> = program
		.into_iter()
		.map(|x| predeclare(x, &mut typer))
		.collect();
	// After collecting all declarations, analyze the function bodies.
	declarations
		.into_iter()
		.map(|x| x.analyze(&mut typer))
		.collect()
}

struct Typer
{
	symbols: std::collections::HashMap<u32, Symbol>,
	functions: std::collections::HashMap<u32, Function>,
	contextual_type: Option<Poisonable<ValueType>>,
}

struct Symbol
{
	identifier: Identifier,
	value_type: Poisonable<ValueType>,
}

struct Function
{
	parameter_types: Vec<Poisonable<ValueType>>,
}

fn do_update_symbol(
	symbol: &mut Symbol,
	new_identifier: &Identifier,
	vt: ValueType,
) -> Result<(), Error>
{
	match &symbol.value_type
	{
		Ok(ot) if ot == &vt => Ok(()),
		Ok(ot) if ot.can_autoderef_into(&vt) => Ok(()),
		Ok(ot) if vt.can_autoderef_into(ot) =>
		{
			symbol.identifier = new_identifier.clone();
			symbol.value_type = Ok(vt);
			Ok(())
		}
		Ok(ot) => Err(Error::ConflictingTypes {
			name: new_identifier.name.clone(),
			current_type: vt,
			previous_type: ot.clone(),
			location: new_identifier.location.clone(),
			previous: symbol.identifier.location.clone(),
		}),
		Err(_poison) => Ok(()),
	}
}

impl Typer
{
	fn put_symbol(
		&mut self,
		identifier: &Identifier,
		value_type: Option<Poisonable<ValueType>>,
	) -> Result<(), Error>
	{
		match value_type
		{
			Some(Ok(vt)) =>
			{
				if let Some(symbol) =
					self.symbols.get_mut(&identifier.resolution_id)
				{
					do_update_symbol(symbol, identifier, vt)
				}
				else
				{
					self.symbols.insert(
						identifier.resolution_id,
						Symbol {
							identifier: identifier.clone(),
							value_type: Ok(vt.clone()),
						},
					);
					Ok(())
				}
			}
			Some(Err(poison)) =>
			{
				if let Some(symbol) =
					self.symbols.get_mut(&identifier.resolution_id)
				{
					symbol.value_type = Err(poison);
					Ok(())
				}
				else
				{
					self.symbols.insert(
						identifier.resolution_id,
						Symbol {
							identifier: identifier.clone(),
							value_type: Err(poison),
						},
					);
					Ok(())
				}
			}
			None => Ok(()),
		}
	}

	fn get_symbol(&self, name: &Identifier) -> Option<Poisonable<ValueType>>
	{
		match self.symbols.get(&name.resolution_id)
		{
			Some(symbol) => match &symbol.value_type
			{
				Ok(vt) => Some(Ok(vt.clone())),
				Err(_poison) => Some(Err(Poison::Poisoned)),
			},
			None => None,
		}
	}

	fn get_symbol_back(
		&self,
		name: &Identifier,
	) -> Option<Poisonable<ValueType>>
	{
		match self.symbols.get(&name.resolution_id)
		{
			Some(symbol) => Some(symbol.value_type.clone()),
			None => None,
		}
	}

	fn get_type_of_base(
		&self,
		base: &Poisonable<Identifier>,
	) -> Option<Poisonable<ValueType>>
	{
		match base
		{
			Ok(base) => self.get_symbol(&base),
			Err(_poison) => Some(Err(Poison::Poisoned)),
		}
	}

	fn get_type_of_reference(
		&self,
		reference: &Reference,
	) -> Option<Poisonable<ValueType>>
	{
		let resolution_id = match &reference.base
		{
			Ok(base) => base.resolution_id,
			Err(_poison) => return Some(Err(Poison::Poisoned)),
		};

		if let Some(symbol) = self.symbols.get(&resolution_id)
		{
			let Symbol {
				identifier: old_identifier,
				value_type: base_type,
			} = symbol;
			let mut x = match base_type
			{
				Ok(base_type) => base_type.fully_dereferenced(),
				Err(_poison) => return Some(Err(Poison::Poisoned)),
			};
			let mut num_steps_taken = 0;
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
								// TODO use num_step_taken to cut off reference
								let _ = num_steps_taken;
								let location = reference.location.clone();
								return Some(Err(Poison::Error {
									error: Error::NotAnArray {
										current_type: x,
										location,
										previous: old_identifier
											.location
											.clone(),
									},
									partial: None,
								}));
							}
						}
					}
					ReferenceStep::Member { .. } => (),
					ReferenceStep::Autodeslice { .. } => (),
					ReferenceStep::Autoderef => (),
					ReferenceStep::Autoview => (),
				}
				num_steps_taken += 1;
			}
			for _i in 0..reference.address_depth
			{
				x = ValueType::Pointer {
					deref_type: Box::new(x),
				};
			}
			Some(Ok(x))
		}
		else
		{
			None
		}
	}

	fn declare_function_parameters(
		&mut self,
		identifier: &Identifier,
		parameters: &[Parameter],
	)
	{
		let parameter_types: Vec<Poisonable<ValueType>> =
			parameters.iter().map(|p| p.value_type.clone()).collect();
		let function = Function { parameter_types };
		self.functions.insert(identifier.resolution_id, function);
	}

	fn analyze_function_arguments(
		&mut self,
		identifier: &Identifier,
		arguments: Vec<Expression>,
	) -> Vec<Expression>
	{
		let parameter_types: Vec<Poisonable<ValueType>> =
			match self.functions.get(&identifier.resolution_id)
			{
				Some(function) => function.parameter_types.clone(),
				None => Vec::new(),
			};
		let parameter_hints = parameter_types
			.into_iter()
			.chain(std::iter::repeat(Err(Poison::Poisoned)));
		let arguments: Vec<Expression> = arguments
			.into_iter()
			.zip(parameter_hints)
			.map(|(a, p)| {
				self.contextual_type = Some(p.clone());
				a.analyze(self)
			})
			.collect();
		arguments
	}
}

pub trait Typed
{
	fn value_type(&self) -> Option<Poisonable<ValueType>>;
}

pub trait StaticallyTyped
{
	fn static_value_type(&self) -> ValueType;
}

impl Typed for PrimitiveLiteral
{
	fn value_type(&self) -> Option<Poisonable<ValueType>>
	{
		Some(Ok(self.static_value_type()))
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

fn predeclare(declaration: Declaration, typer: &mut Typer) -> Declaration
{
	match declaration
	{
		Declaration::Constant {
			name,
			value,
			value_type,
			flags,
		} => match typer.put_symbol(&name, Some(value_type.clone()))
		{
			Ok(()) => Declaration::Constant {
				name,
				value,
				value_type,
				flags,
			},
			Err(error) => Declaration::Poison(Poison::Error {
				error,
				partial: Some(Box::new(Declaration::Constant {
					name,
					value,
					value_type,
					flags,
				})),
			}),
		},
		Declaration::Function {
			name,
			parameters,
			body,
			return_type,
			flags,
		} =>
		{
			let parameters: Vec<Parameter> = parameters
				.into_iter()
				.map(|x| x.clone().analyze(typer))
				.collect();

			typer.declare_function_parameters(&name, &parameters);

			let rv_identifier = name.return_value();
			let rv_type = return_type.clone().map(|x| Ok(x));
			match typer.put_symbol(&rv_identifier, rv_type)
			{
				Ok(()) => Declaration::Function {
					name,
					parameters,
					body,
					return_type,
					flags,
				},
				Err(error) => Declaration::Poison(Poison::Error {
					error,
					partial: Some(Box::new(Declaration::Function {
						name,
						parameters,
						body,
						return_type,
						flags,
					})),
				}),
			}
		}
		Declaration::FunctionHead {
			name,
			parameters,
			return_type,
			flags,
		} =>
		{
			let parameters: Vec<Parameter> = parameters
				.into_iter()
				.map(|x| x.clone().analyze(typer))
				.collect();

			typer.declare_function_parameters(&name, &parameters);

			let rv_identifier = name.return_value();
			let rv_type = return_type.clone().map(|x| Ok(x));
			match typer.put_symbol(&rv_identifier, rv_type)
			{
				Ok(()) => Declaration::FunctionHead {
					name,
					parameters,
					return_type,
					flags,
				},
				Err(error) => Declaration::Poison(Poison::Error {
					error,
					partial: Some(Box::new(Declaration::FunctionHead {
						name,
						parameters,
						return_type,
						flags,
					})),
				}),
			}
		}
		Declaration::PreprocessorDirective { .. } => unreachable!(),
		Declaration::Poison(Poison::Error {
			error,
			partial: Some(declaration),
		}) =>
		{
			let declaration = predeclare(*declaration, typer);
			Declaration::Poison(Poison::Error {
				error,
				partial: Some(Box::new(declaration)),
			})
		}
		Declaration::Poison(Poison::Error {
			error: _,
			partial: None,
		}) => declaration,
		Declaration::Poison(Poison::Poisoned) => declaration,
	}
}

trait Analyzable
{
	type Item;

	fn analyze(self, typer: &mut Typer) -> Self::Item;
}

impl Typed for Declaration
{
	fn value_type(&self) -> Option<Poisonable<ValueType>>
	{
		match self
		{
			Declaration::Constant { value_type, .. } =>
			{
				Some(value_type.clone())
			}
			Declaration::Function { return_type, .. }
			| Declaration::FunctionHead { return_type, .. } =>
			{
				return_type.clone().map(|x| Ok(x))
			}
			Declaration::PreprocessorDirective { .. } => unreachable!(),
			Declaration::Poison(_) => Some(Err(Poison::Poisoned)),
		}
	}
}

impl Analyzable for Declaration
{
	type Item = Declaration;

	fn analyze(self, typer: &mut Typer) -> Self::Item
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
				let value = value.analyze(typer);
				Declaration::Constant {
					name,
					value,
					value_type,
					flags,
				}
			}
			Declaration::Function {
				name,
				parameters,
				body: Err(poisoned_body),
				return_type,
				flags,
			} =>
			{
				let parameters: Vec<Parameter> =
					parameters.into_iter().map(|x| x.analyze(typer)).collect();

				Declaration::Function {
					name,
					parameters,
					body: Err(poisoned_body),
					return_type,
					flags,
				}
			}
			Declaration::Function {
				name,
				parameters,
				body: Ok(body),
				return_type,
				flags,
			} =>
			{
				let parameters: Vec<Parameter> =
					parameters.into_iter().map(|x| x.analyze(typer)).collect();

				// Pre-analyze the function body because it might contain
				// untyped declarations, e.g. "var x;", whose types won't be
				// determined in the first pass.
				typer.contextual_type = return_type.clone().map(|x| Ok(x));
				let prebody: FunctionBody = body.clone().analyze(typer);
				// Pre-analyze the statements in reverse, because there might
				// be chains of untyped declarations, e.g.
				//   var x = 1;
				//   var y = x;
				// whose types won't be determined in the forward pass.
				typer.contextual_type = return_type.clone().map(|x| Ok(x));
				for statement in prebody.statements.into_iter().rev()
				{
					let _unused: Statement = statement.analyze(typer);
				}

				typer.contextual_type = return_type.clone().map(|x| Ok(x));
				let body = body.analyze(typer);

				match analyze_return_value(&name, &return_type, &body, typer)
				{
					Ok(()) => Declaration::Function {
						name,
						parameters,
						body: Ok(body),
						return_type,
						flags,
					},
					Err(error) => Declaration::Poison(Poison::Error {
						error,
						partial: Some(Box::new(Declaration::Function {
							name,
							parameters,
							body: Ok(body),
							return_type,
							flags,
						})),
					}),
				}
			}
			Declaration::FunctionHead {
				name,
				parameters,
				return_type,
				flags,
			} =>
			{
				let parameters: Vec<Parameter> =
					parameters.into_iter().map(|x| x.analyze(typer)).collect();

				Declaration::FunctionHead {
					name,
					parameters,
					return_type,
					flags,
				}
			}
			Declaration::PreprocessorDirective { .. } => unreachable!(),
			Declaration::Poison(Poison::Error {
				error,
				partial: Some(declaration),
			}) =>
			{
				let declaration = declaration.analyze(typer);
				Declaration::Poison(Poison::Error {
					error,
					partial: Some(Box::new(declaration)),
				})
			}
			Declaration::Poison(Poison::Error {
				error: _,
				partial: None,
			}) => self,
			Declaration::Poison(Poison::Poisoned) => self,
		}
	}
}

fn analyze_return_value(
	function: &Identifier,
	return_type: &Option<ValueType>,
	body: &FunctionBody,
	typer: &mut Typer,
) -> Result<(), Error>
{
	match (return_type, &body.return_value, body.value_type())
	{
		(_, _, Some(Err(_poison))) =>
		{
			// This error will already be thrown by the body.
			Ok(())
		}
		(Some(_), Some(_), Some(Ok(vt))) =>
		{
			let rv_identifier = function.return_value();
			typer.put_symbol(&rv_identifier, Some(Ok(vt)))
		}
		(Some(rt), Some(_), None) => Err(Error::AmbiguousReturnValue {
			declared_type: rt.clone(),
			location_of_return_value: body
				.return_value_identifier
				.location
				.clone(),
			location_of_declaration: function.location.clone(),
		}),
		(Some(rt), None, _) => Err(Error::MissingReturnValue {
			declared_type: rt.clone(),
			location: body.return_value_identifier.location.clone(),
			location_of_declaration: function.location.clone(),
		}),
		(None, Some(_), Some(Ok(rvt))) => Err(Error::MissingReturnType {
			inferred_type: rvt.clone(),
			location_of_return_value: body
				.return_value_identifier
				.location
				.clone(),
			location_of_declaration: function.location.clone(),
		}),
		(None, Some(_), None) => Err(Error::MissingAmbiguousReturnType {
			location_of_return_value: body
				.return_value_identifier
				.location
				.clone(),
			location_of_declaration: function.location.clone(),
		}),
		(None, None, Some(Ok(_))) => unreachable!(),
		(None, None, None) => Ok(()),
	}
}

impl Analyzable for Parameter
{
	type Item = Parameter;

	fn analyze(self, typer: &mut Typer) -> Self::Item
	{
		let value_type = match &self.name
		{
			Ok(name) =>
			{
				match typer.put_symbol(name, Some(self.value_type.clone()))
				{
					Ok(()) =>
					{
						let value_type = typer.get_symbol_back(name);
						value_type.unwrap_or(self.value_type)
					}
					Err(error) => Err(error.into()),
				}
			}
			Err(_poison) => self.value_type,
		};
		Parameter {
			name: self.name,
			value_type,
		}
	}
}

impl Typed for FunctionBody
{
	fn value_type(&self) -> Option<Poisonable<ValueType>>
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

	fn analyze(self, typer: &mut Typer) -> Self::Item
	{
		let contextual_return_type = typer.contextual_type.take();
		let statements: Vec<Statement> = self
			.statements
			.into_iter()
			.map(|x| x.analyze(typer))
			.collect();
		let return_value = match self.return_value
		{
			Some(value) =>
			{
				typer.contextual_type = contextual_return_type;
				let value = value.analyze(typer);
				typer.contextual_type = None;
				Some(value)
			}
			None => None,
		};
		FunctionBody {
			statements,
			return_value,
			return_value_identifier: self.return_value_identifier,
		}
	}
}

impl Analyzable for Block
{
	type Item = Block;

	fn analyze(self, typer: &mut Typer) -> Self::Item
	{
		typer.contextual_type = None;
		let statements: Vec<Statement> = self
			.statements
			.into_iter()
			.map(|x| x.analyze(typer))
			.collect();
		Block {
			statements,
			location: self.location,
		}
	}
}

impl Analyzable for Statement
{
	type Item = Statement;

	fn analyze(self, typer: &mut Typer) -> Self::Item
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
				let recoverable_error =
					typer.put_symbol(&name, Some(declared_type.clone()));
				typer.contextual_type = Some(declared_type.clone());
				let value = value.analyze(typer);
				typer.contextual_type = None;
				let value_type = if let Err(error) = recoverable_error
				{
					Some(Err(error.into()))
				}
				else if let Some(inferred_type) = value.value_type()
				{
					let result =
						typer.put_symbol(&name, Some(inferred_type.clone()));
					match (result, inferred_type, declared_type.clone())
					{
						(Err(error), _, _) => Some(Err(error.into())),
						(Ok(()), Ok(inferred_type), Ok(declared_type)) =>
						{
							if inferred_type.can_be_declared_as(&declared_type)
							{
								Some(Ok(inferred_type))
							}
							else
							{
								Some(Ok(declared_type))
							}
						}
						(Ok(()), _, Err(poison)) => Some(Err(poison)),
						(Ok(()), Err(poison), Ok(_)) => Some(Err(poison)),
					}
				}
				else
				{
					None
				};
				Statement::Declaration {
					name,
					value: Some(value),
					value_type,
					location,
				}
			}
			Statement::Declaration {
				name,
				value: Some(value),
				value_type: None,
				location,
			} =>
			{
				typer.contextual_type = typer.get_symbol(&name);
				let value = value.analyze(typer);
				typer.contextual_type = None;
				let value_type = value.value_type();
				let value_type = infer_for_declaration(value_type);
				let recoverable_error =
					typer.put_symbol(&name, value_type.clone());
				let value_type = match recoverable_error
				{
					Ok(()) => value_type,
					Err(error) => Some(Err(error.into())),
				};
				Statement::Declaration {
					name,
					value: Some(value),
					value_type,
					location,
				}
			}
			Statement::Declaration {
				name,
				value: None,
				value_type: Some(value_type),
				location,
			} =>
			{
				let recoverable_error =
					typer.put_symbol(&name, Some(value_type.clone()));
				let value_type = match recoverable_error
				{
					Ok(()) => Some(value_type),
					Err(error) => Some(Err(error.into())),
				};
				Statement::Declaration {
					name,
					value: None,
					value_type,
					location,
				}
			}
			Statement::Declaration {
				name,
				value: None,
				value_type: None,
				location,
			} =>
			{
				let value_type = typer.get_symbol(&name);
				let value_type = infer_for_declaration(value_type);
				Statement::Declaration {
					name,
					value: None,
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
				let ref_type = typer.get_type_of_reference(&reference);
				typer.contextual_type = ref_type;
				let value = value.analyze(typer);
				typer.contextual_type = None;
				let value_type = value.value_type();
				let reference = reference.analyze_assignment(value_type, typer);
				Statement::Assignment {
					reference,
					value,
					location,
				}
			}
			Statement::MethodCall { name, arguments } =>
			{
				let arguments =
					typer.analyze_function_arguments(&name, arguments);
				Statement::MethodCall { name, arguments }
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
				let condition = condition.analyze(typer);
				let then_branch = {
					let branch = then_branch.analyze(typer);
					Box::new(branch)
				};
				let else_branch = if let Some(else_branch) = else_branch
				{
					let branch = else_branch.branch.analyze(typer);
					Some(Else {
						branch: Box::new(branch),
						location_of_else: else_branch.location_of_else,
					})
				}
				else
				{
					None
				};
				Statement::If {
					condition,
					then_branch,
					else_branch,
					location,
				}
			}
			Statement::Block(block) =>
			{
				let block = block.analyze(typer);
				Statement::Block(block)
			}
			Statement::Poison(Poison::Error {
				error,
				partial: Some(statement),
			}) =>
			{
				let statement = statement.analyze(typer);
				Statement::Poison(Poison::Error {
					error,
					partial: Some(Box::new(statement)),
				})
			}
			Statement::Poison(Poison::Error {
				error: _,
				partial: None,
			}) => self,
			Statement::Poison(Poison::Poisoned) => self,
		}
	}
}

impl Analyzable for Comparison
{
	type Item = Comparison;

	fn analyze(self, typer: &mut Typer) -> Self::Item
	{
		let contextual_type = typer.contextual_type.take();
		typer.contextual_type =
			self.right.value_type().or(contextual_type.clone());
		let left = self.left.analyze(typer);
		typer.contextual_type = left.value_type().or(contextual_type);
		let right = self.right.analyze(typer);
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
	type Item = Array;

	fn analyze(self, typer: &mut Typer) -> Self::Item
	{
		let name = self.get_identifier();
		let elements: Vec<Expression> = self
			.elements
			.into_iter()
			.map(|element| {
				let element = element.analyze(typer);
				let element_type = element.value_type();
				typer.contextual_type = element_type.clone();
				match typer.put_symbol(&name, element_type)
				{
					Ok(()) => element,
					Err(error) => Expression::Poison(Poison::Error {
						error,
						partial: Some(Box::new(element)),
					}),
				}
			})
			.collect();

		Array {
			elements,
			location: self.location,
			resolution_id: self.resolution_id,
		}
	}
}

impl Typed for Expression
{
	fn value_type(&self) -> Option<Poisonable<ValueType>>
	{
		match self
		{
			Expression::Binary { left, .. } => left.value_type(),
			Expression::Unary { expression, .. } => expression.value_type(),
			Expression::PrimitiveLiteral { literal, .. } =>
			{
				literal.value_type()
			}
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
				Some(Ok(element_type)) => Some(Ok(ValueType::Array {
					element_type: Box::new(element_type.clone()),
					length: elements.len(),
				})),
				Some(Err(_poison)) => Some(Err(Poison::Poisoned)),
				None => None,
			},
			Expression::StringLiteral { value_type, .. } => value_type.clone(),
			Expression::Deref {
				reference: _,
				deref_type,
			} => deref_type.clone(),
			Expression::Autocoerce { coerced_type, .. } =>
			{
				Some(Ok(coerced_type.clone()))
			}
			Expression::PrimitiveCast { coerced_type, .. } =>
			{
				Some(Ok(coerced_type.clone()))
			}
			Expression::LengthOfArray { .. } => Some(Ok(ValueType::Usize)),
			Expression::FunctionCall { return_type, .. } => return_type.clone(),
			Expression::Poison(_) => Some(Err(Poison::Poisoned)),
		}
	}
}

impl Analyzable for Expression
{
	type Item = Expression;

	fn analyze(self, typer: &mut Typer) -> Self::Item
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
				let contextual_type = typer.contextual_type.take();
				typer.contextual_type =
					right.value_type().or(contextual_type.clone());
				let left = left.analyze(typer);
				typer.contextual_type = left.value_type().or(contextual_type);
				let right = right.analyze(typer);
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
				let expr = expression.analyze(typer);
				Expression::Unary {
					op,
					expression: Box::new(expr),
					location,
					location_of_op,
				}
			}
			Expression::PrimitiveLiteral { .. } => self,
			Expression::NakedIntegerLiteral {
				value,
				value_type,
				location,
			} =>
			{
				let value_type = match value_type
				{
					Some(Ok(vt)) => Some(Ok(vt.clone())),
					Some(Err(_poison)) => Some(Err(Poison::Poisoned)),
					None =>
					{
						let contextual_type = typer.contextual_type.take();
						filter_for_naked_integer(contextual_type)
					}
				};
				Expression::NakedIntegerLiteral {
					value,
					value_type,
					location,
				}
			}
			Expression::BitIntegerLiteral {
				value,
				value_type,
				location,
			} =>
			{
				let value_type = match value_type
				{
					Some(Ok(vt)) => Some(Ok(vt.clone())),
					Some(Err(_poison)) => Some(Err(Poison::Poisoned)),
					None =>
					{
						let contextual_type = typer.contextual_type.take();
						filter_for_bit_integer(contextual_type)
					}
				};
				Expression::BitIntegerLiteral {
					value,
					value_type,
					location,
				}
			}
			Expression::ArrayLiteral {
				array,
				element_type,
			} =>
			{
				let name = array.get_identifier();
				let put_result = if element_type.is_some()
				{
					let result = typer.put_symbol(&name, element_type.clone());
					typer.contextual_type = element_type.clone();
					result
				}
				else
				{
					let array_type = typer.contextual_type.take();
					typer.contextual_type = match array_type
					{
						Some(Ok(x)) => match x.get_element_type()
						{
							Some(y) => Some(Ok(y)),
							None => None,
						},
						Some(Err(_poison)) => Some(Err(Poison::Poisoned)),
						None => None,
					};
					Ok(())
				};
				let array = array.analyze(typer);
				match put_result
				{
					Ok(()) =>
					{
						let element_type = typer.get_symbol(&name);

						Expression::ArrayLiteral {
							array,
							element_type,
						}
					}
					Err(error) => Expression::Poison(Poison::Error {
						error,
						partial: Some(Box::new(Expression::ArrayLiteral {
							array,
							element_type,
						})),
					}),
				}
			}
			Expression::StringLiteral {
				value_type: Some(Ok(ref vt)),
				..
			} =>
			{
				let contextual_type = typer.contextual_type.take();
				match contextual_type
				{
					Some(Ok(ct)) if vt.can_coerce_into(&ct) =>
					{
						let expr = self;
						Expression::Autocoerce {
							expression: Box::new(expr),
							coerced_type: ct,
						}
					}
					Some(Ok(_)) => self,
					Some(Err(_poison)) => self,
					None => self,
				}
			}
			Expression::StringLiteral {
				value_type: Some(Err(_)),
				..
			} => self,
			Expression::StringLiteral {
				bytes,
				value_type: None,
				location,
			} =>
			{
				let contextual_type = typer.contextual_type.take();
				let (value_type, coerced_type) =
					coerce_for_string_literal(contextual_type);
				let expr = Expression::StringLiteral {
					bytes,
					value_type,
					location,
				};
				if let Some(coerced_type) = coerced_type
				{
					Expression::Autocoerce {
						expression: Box::new(expr),
						coerced_type,
					}
				}
				else
				{
					expr
				}
			}
			Expression::Deref {
				reference: _,
				deref_type: Some(_),
			} => self,
			Expression::Deref {
				reference,
				deref_type: None,
			} => reference.analyze_deref_expression(typer),
			Expression::Autocoerce {
				expression: _,
				coerced_type: _,
			} => self,
			Expression::PrimitiveCast {
				expression,
				coerced_type,
				location,
				location_of_type,
			} =>
			{
				let expr = expression.analyze(typer);
				Expression::PrimitiveCast {
					expression: Box::new(expr),
					coerced_type,
					location,
					location_of_type,
				}
			}
			Expression::LengthOfArray { reference } =>
			{
				let base_type = typer.get_type_of_base(&reference.base);
				let array_type = typer.get_type_of_reference(&reference);
				match array_type
				{
					Some(Ok(ValueType::Array { .. })) =>
					{
						let reference = reference
							.analyze_length(base_type, array_type, typer);
						Expression::LengthOfArray { reference }
					}
					Some(Ok(ValueType::Slice { .. })) =>
					{
						let mut reference = reference
							.analyze_length(base_type, array_type, typer);
						let autostep = ReferenceStep::Autodeslice { offset: 1 };
						reference.steps.push(autostep);
						Expression::Deref {
							reference,
							deref_type: Some(Ok(ValueType::Usize)),
						}
					}
					Some(Ok(current_type)) =>
					{
						// TODO determine location of declaration?
						let previous = reference.location.clone();
						Expression::Poison(Poison::Error {
							error: Error::NotAnArrayWithLength {
								current_type,
								location: reference.location.clone(),
								previous,
							},
							partial: Some(Box::new(
								Expression::LengthOfArray { reference },
							)),
						})
					}
					Some(Err(_poison)) =>
					{
						Expression::LengthOfArray { reference }
					}
					None => Expression::LengthOfArray { reference },
				}
			}
			Expression::FunctionCall {
				name,
				arguments,
				return_type,
			} =>
			{
				let rv_identifier = name.return_value();
				let recoverable_error =
					typer.put_symbol(&rv_identifier, return_type.clone());
				let return_type = typer.get_symbol(&rv_identifier);
				let arguments =
					typer.analyze_function_arguments(&name, arguments);
				let expr = Expression::FunctionCall {
					name,
					arguments,
					return_type,
				};
				match recoverable_error
				{
					Ok(()) => expr,
					Err(error) => Expression::Poison(Poison::Error {
						error,
						partial: Some(Box::new(expr)),
					}),
				}
			}
			Expression::Poison(_) => self,
		}
	}
}

impl Analyzable for ReferenceStep
{
	type Item = ReferenceStep;

	fn analyze(self, typer: &mut Typer) -> Self::Item
	{
		match self
		{
			ReferenceStep::Element { argument } =>
			{
				typer.contextual_type = Some(Ok(ValueType::Usize));
				let argument = argument.analyze(typer);
				typer.contextual_type = None;
				ReferenceStep::Element {
					argument: Box::new(argument),
				}
			}
			ReferenceStep::Member { member: _ } => self,
			ReferenceStep::Autodeslice { offset: _ } => self,
			ReferenceStep::Autoderef => ReferenceStep::Autoderef,
			ReferenceStep::Autoview => ReferenceStep::Autoview,
		}
	}
}

fn analyze_assignment_steps(
	base_type: ValueType,
	previous_steps: Vec<ReferenceStep>,
	value_type: &Option<Poisonable<ValueType>>,
	address_depth: u8,
) -> (Vec<ReferenceStep>, u8)
{
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
							steps.push(step);
							current_type = *deref_type;
						}
						ValueType::View { deref_type } =>
						{
							let step = ReferenceStep::Autoview;
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
						let autostep = ReferenceStep::Autodeslice { offset: 0 };
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
		match value_type
		{
			Some(Ok(vt)) if vt == &current_type =>
			{
				return (steps, 0);
			}
			_ => (),
		}
		match current_type
		{
			ValueType::Pointer { deref_type } =>
			{
				let step = ReferenceStep::Autoderef;
				steps.push(step);
				current_type = *deref_type;
			}
			_ => break,
		}
	}
	(steps, address_depth)
}

impl Reference
{
	fn analyze_length(
		self,
		base_type: Option<Poisonable<ValueType>>,
		array_type: Option<Poisonable<ValueType>>,
		typer: &mut Typer,
	) -> Self
	{
		match (base_type, array_type)
		{
			(Some(Ok(x)), Some(Ok(y))) if x == y => self,
			(_, array_type) => self.analyze_assignment(array_type, typer),
		}
	}

	fn analyze_assignment(
		self,
		value_type: Option<Poisonable<ValueType>>,
		typer: &mut Typer,
	) -> Self
	{
		let steps: Vec<ReferenceStep> = self
			.steps
			.into_iter()
			.map(|step| step.analyze(typer))
			.collect();

		let base = match &self.base
		{
			Ok(base) => base,
			Err(_poison) =>
			{
				return Reference {
					base: self.base,
					steps,
					address_depth: self.address_depth,
					location: self.location,
				}
			}
		};

		let base_type = typer.get_symbol(base);

		let (steps, address_depth) = match base_type
		{
			Some(Ok(base_type)) => analyze_assignment_steps(
				base_type,
				steps,
				&value_type,
				self.address_depth,
			),
			Some(Err(_poison)) => (steps, self.address_depth),
			None => (steps, self.address_depth),
		};

		let full_type =
			build_type_of_reference(value_type, &steps, address_depth);
		let base = match typer.put_symbol(base, full_type)
		{
			Ok(()) => self.base,
			Err(error) => Err(Poison::Error {
				error,
				partial: Some(base.clone()),
			}),
		};

		Reference {
			base,
			steps,
			address_depth,
			location: self.location,
		}
	}

	fn analyze_deref_expression(mut self, typer: &mut Typer) -> Expression
	{
		let known_type = typer.get_type_of_reference(&self);
		let contextual_type = typer.contextual_type.take();

		self.steps = self
			.steps
			.into_iter()
			.map(|step| step.analyze(typer))
			.collect();

		let base = match &self.base
		{
			Ok(base) => base,
			Err(_poison) =>
			{
				return Expression::Deref {
					reference: self,
					deref_type: known_type,
				};
			}
		};

		let ref_type = typer.get_symbol(base);
		match (known_type, contextual_type, ref_type)
		{
			(Some(Ok(x)), Some(Ok(y)), Some(Ok(ref_type))) if x == y =>
			{
				self.autoderef(ref_type, y, typer)
			}
			(Some(Ok(x)), Some(Ok(y)), None) if x == y =>
			{
				let base_type = Some(Ok(x));
				let deref_type = Some(Ok(y));
				let full_type = build_type_of_reference(
					base_type,
					&self.steps,
					self.address_depth,
				);
				match typer.put_symbol(base, full_type.clone())
				{
					Ok(()) => Expression::Deref {
						reference: self,
						deref_type,
					},
					Err(error) => Expression::Poison(Poison::Error {
						error,
						partial: Some(Box::new(Expression::Deref {
							reference: self,
							deref_type,
						})),
					}),
				}
			}
			(Some(Ok(x)), Some(Ok(y)), Some(Ok(ref_type)))
				if x.can_autoderef_into(&y) =>
			{
				self.autoderef(ref_type, y, typer)
			}
			(Some(Ok(def)), None, Some(Ok(ref_type))) =>
			{
				self.autoderef(ref_type, def, typer)
			}
			(Some(default_type_or_poison), _, _) =>
			{
				let base_type = Some(default_type_or_poison);
				let deref_type = base_type.clone();
				let full_type = build_type_of_reference(
					base_type,
					&self.steps,
					self.address_depth,
				);
				match typer.put_symbol(base, full_type.clone())
				{
					Ok(()) => Expression::Deref {
						reference: self,
						deref_type,
					},
					Err(error) => Expression::Poison(Poison::Error {
						error,
						partial: Some(Box::new(Expression::Deref {
							reference: self,
							deref_type,
						})),
					}),
				}
			}
			(None, deref_type, _) =>
			{
				let base_type = deref_type.clone();
				let full_type = build_type_of_reference(
					base_type,
					&self.steps,
					self.address_depth,
				);
				match typer.put_symbol(base, full_type.clone())
				{
					Ok(()) => Expression::Deref {
						reference: self,
						deref_type,
					},
					Err(error) => Expression::Poison(Poison::Error {
						error,
						partial: Some(Box::new(Expression::Deref {
							reference: self,
							deref_type,
						})),
					}),
				}
			}
		}
	}

	fn autoderef(
		self,
		known_ref_type: ValueType,
		target_type: ValueType,
		typer: &mut Typer,
	) -> Expression
	{
		let base = match &self.base
		{
			Ok(base) => base,
			Err(_poison) =>
			{
				return Expression::Deref {
					reference: self,
					deref_type: Some(Ok(known_ref_type)),
				};
			}
		};

		let mut available_steps = self.steps.into_iter().peekable();
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
					take_address = true;
					break;
				}
				(ct, ValueType::Pointer { deref_type }, None)
					if ct.can_coerce_into(deref_type.as_ref())
						&& self.address_depth > 0
						&& !take_address =>
				{
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
						taken_steps.push(step);
						available_steps.next();
						current_type = element_type.as_ref().clone();
					}
					_ =>
					{
						let step = ReferenceStep::Autoderef;
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
						taken_steps.push(step);
						available_steps.next();
						current_type = element_type.as_ref().clone();
					}
					_ =>
					{
						let step = ReferenceStep::Autoview;
						taken_steps.push(step);
						current_type = *deref_type;
					}
				},
				(ValueType::Pointer { deref_type }, _, _) =>
				{
					let step = ReferenceStep::Autoderef;
					taken_steps.push(step);
					current_type = *deref_type;
				}
				(ValueType::View { deref_type }, _, _) =>
				{
					let step = ReferenceStep::Autoview;
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
					taken_steps.push(autostep);

					let step = ReferenceStep::Element {
						argument: argument.clone(),
					};
					taken_steps.push(step);
					available_steps.next();
					current_type = *element_type;
				}
				(_, _, None) if self.address_depth >= 2 =>
				{
					// This is invalid, but we want a proper error, not an
					// autoderef failure, so just skip the autoderef.
					let expr = Expression::Deref {
						reference: Reference {
							base: self.base,
							steps: taken_steps,
							address_depth: self.address_depth,
							location: self.location,
						},
						deref_type: Some(Ok(target_type)),
					};
					return expr;
				}
				(ct, tt, step) =>

					panic!(
						"failed to autoderef {}, target type: {:?}, current type: {:?}, available step: {:?}, ad: {:?}, taken address: {:?}",
						self.location.format(),
						tt,
						ct,
						step,
						self.address_depth,
						take_address),
			}
		}

		let address_depth = if take_address { 1 } else { 0 };
		let (deref_type, coerced_type) = match coercion
		{
			Some((x, y)) => (Some(Ok(x)), Some(y)),
			None => (Some(Ok(target_type)), None),
		};
		let base_type = deref_type.clone();
		let full_type =
			build_type_of_reference(base_type, &taken_steps, address_depth);
		let expr = Expression::Deref {
			reference: Reference {
				base: Ok(base.clone()),
				steps: taken_steps,
				address_depth,
				location: self.location,
			},
			deref_type,
		};
		let expr = if let Some(coerced_type) = coerced_type
		{
			Expression::Autocoerce {
				expression: Box::new(expr),
				coerced_type,
			}
		}
		else
		{
			expr
		};
		match typer.put_symbol(base, full_type.clone())
		{
			Ok(()) => expr,
			Err(error) => Expression::Poison(Poison::Error {
				error,
				partial: Some(Box::new(expr)),
			}),
		}
	}
}

fn build_type_of_reference(
	base_type: Option<Poisonable<ValueType>>,
	steps: &[ReferenceStep],
	address_depth: u8,
) -> Option<Poisonable<ValueType>>
{
	match base_type
	{
		Some(Ok(base_type)) =>
		{
			let full_type = build_type_of_ref1(base_type, steps, address_depth);
			Some(Ok(full_type))
		}
		Some(Err(_poison)) => Some(Err(Poison::Poisoned)),
		None => None,
	}
}

fn build_type_of_ref1(
	base_type: ValueType,
	steps: &[ReferenceStep],
	address_depth: u8,
) -> ValueType
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
	full_type
}

fn infer_for_declaration(
	value_type: Option<Poisonable<ValueType>>,
) -> Option<Poisonable<ValueType>>
{
	match value_type
	{
		Some(Ok(ValueType::Pointer { .. })) => None,
		Some(Ok(x)) => Some(Ok(x)),
		Some(Err(_poison)) => Some(Err(Poison::Poisoned)),
		None => None,
	}
}

fn filter_for_naked_integer(
	value_type: Option<Poisonable<ValueType>>,
) -> Option<Poisonable<ValueType>>
{
	match value_type
	{
		Some(Ok(ValueType::Int8)) => value_type,
		Some(Ok(ValueType::Int16)) => value_type,
		Some(Ok(ValueType::Int32)) => value_type,
		Some(Ok(ValueType::Int64)) => value_type,
		Some(Ok(ValueType::Int128)) => value_type,
		Some(Ok(ValueType::Uint8)) => value_type,
		Some(Ok(ValueType::Uint16)) => value_type,
		Some(Ok(ValueType::Uint32)) => value_type,
		Some(Ok(ValueType::Uint64)) => value_type,
		Some(Ok(ValueType::Uint128)) => value_type,
		Some(Ok(ValueType::Usize)) => value_type,
		Some(Ok(_)) => Some(Ok(ValueType::Int32)),
		Some(Err(_poison)) => Some(Err(Poison::Poisoned)),
		None => None,
	}
}

fn filter_for_bit_integer(
	value_type: Option<Poisonable<ValueType>>,
) -> Option<Poisonable<ValueType>>
{
	match value_type
	{
		Some(Ok(ValueType::Uint8)) => value_type,
		Some(Ok(ValueType::Uint16)) => value_type,
		Some(Ok(ValueType::Uint32)) => value_type,
		Some(Ok(ValueType::Uint64)) => value_type,
		Some(Ok(ValueType::Usize)) => value_type,
		Some(Ok(ValueType::Pointer { .. })) => value_type,
		Some(Ok(ValueType::View { .. })) => value_type,
		Some(Ok(_)) => Some(Ok(ValueType::Uint64)),
		Some(Err(_poison)) => Some(Err(Poison::Poisoned)),
		None => None,
	}
}

fn coerce_for_string_literal(
	value_type: Option<Poisonable<ValueType>>,
) -> (Option<Poisonable<ValueType>>, Option<ValueType>)
{
	match value_type
	{
		Some(Ok(ValueType::String)) => (value_type, None),
		Some(Ok(vt)) if vt == ValueType::for_byte_string() =>
		{
			(Some(Ok(vt.clone())), Some(vt))
		}
		Some(Ok(vt)) if ValueType::for_byte_string().can_coerce_into(&vt) =>
		{
			(Some(Ok(ValueType::for_byte_string())), Some(vt))
		}
		Some(Ok(_)) => (Some(Ok(ValueType::String)), None),
		Some(Err(_poison)) => (Some(Err(Poison::Poisoned)), None),
		None => (None, None),
	}
}
