//
// Part of penne
// Copyright (c) 2020 Sander in 't Veld
// License: MIT
//

//! During the type inference stage, the types of values and variables are
//! determined, and `autoderef` expressions are inserted.

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
		structures: std::collections::HashMap::new(),
		contextual_type: None,
	};
	// Prealign all structures so that their use as a value type is defined.
	// Structures may be declared in any order, so aligning nested structures
	// may take multiple steps.
	let mut declarations = program;
	let max_prealign_steps = declarations.len();
	for _ in 0..max_prealign_steps
	{
		declarations = declarations
			.into_iter()
			.map(|x| prealign(x, &mut typer))
			.collect();
		if !declarations.iter().any(|x| needs_aligning(x))
		{
			break;
		}
	}
	// Predeclare all functions so that they can reference each other.
	declarations = declarations
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
	structures: std::collections::HashMap<u32, Structure>,
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

struct Structure
{
	identifier: Identifier,
	members: Vec<Member>,
}

fn do_update_symbol(
	symbol: &mut Symbol,
	new_identifier: &Identifier,
	vt: ValueType,
) -> Result<(), Error>
{
	let ot = match &symbol.value_type
	{
		Ok(ot) => ot,
		Err(_poison) => return Ok(()),
	};

	if ot == &vt
	{
		Ok(())
	}
	else if ot.can_be_concretization_of(&vt)
	{
		Ok(())
	}
	else if symbol.identifier.is_authoritative && vt.can_be_declared_as(ot)
	{
		// Keep symbol.identifier the same.
		symbol.value_type = Ok(vt);
		Ok(())
	}
	else if new_identifier.is_authoritative && vt.can_be_concretization_of(ot)
	{
		symbol.identifier = new_identifier.clone();
		symbol.value_type = Ok(vt);
		Ok(())
	}
	else if new_identifier.is_authoritative && vt.can_coerce_into(ot)
	{
		symbol.identifier = new_identifier.clone();
		symbol.value_type = Ok(vt);
		Ok(())
	}
	else
	{
		Err(Error::ConflictingTypes {
			name: new_identifier.name.clone(),
			current_type: vt,
			previous_type: ot.clone(),
			location: new_identifier.location.clone(),
			previous: symbol.identifier.location.clone(),
		})
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
			Some(symbol) if name.is_authoritative =>
			{
				Some(symbol.value_type.clone())
			}
			Some(symbol) => match &symbol.value_type
			{
				Ok(vt) => Some(Ok(vt.clone())),
				Err(_poison) => Some(Err(Poison::Poisoned)),
			},
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
			Ok(base) => self.get_symbol(&base.inferred()),
			Err(_poison) => Some(Err(Poison::Poisoned)),
		}
	}

	fn get_type_of_reference(
		&self,
		reference: &mut Reference,
	) -> Option<Poisonable<ValueType>>
	{
		let resolution_id = match &reference.base
		{
			Ok(base) => base.resolution_id,
			Err(_poison) => return Some(Err(Poison::Poisoned)),
		};

		let base = match self.symbols.get(&resolution_id)
		{
			Some(base) => base,
			None => return None,
		};
		let Symbol {
			identifier: old_identifier,
			value_type: base_type,
		} = base;

		let mut x = match base_type
		{
			Ok(base_type) => base_type.fully_dereferenced(),
			Err(_poison) => return Some(Err(Poison::Poisoned)),
		};
		let mut num_steps_taken = 0;
		for step in &mut reference.steps
		{
			match step
			{
				ReferenceStep::Element {
					argument: _,
					is_endless: _,
				} =>
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
									previous: old_identifier.location.clone(),
								},
								partial: None,
							}));
						}
					}
				}
				ReferenceStep::Member { member, offset } =>
				{
					let structure = match &x
					{
						ValueType::Struct {
							identifier,
							size_in_bytes: _,
						} => identifier,
						ValueType::Word {
							identifier,
							size_in_bytes: _,
						} => identifier,
						ValueType::UnresolvedStructOrWord {
							identifier: Some(_),
						} => return Some(Err(Poison::Poisoned)),
						ValueType::UnresolvedStructOrWord {
							identifier: None,
						} => return None,
						_ =>
						{
							// TODO use num_step_taken to cut off reference
							let _ = num_steps_taken;
							let location = reference.location.clone();
							let error = Error::NotAStructure {
								current_type: x,
								location,
								previous: old_identifier.location.clone(),
							};
							reference.base = Err(error.into());
							return Some(Err(Poison::Poisoned));
						}
					};
					// Set resolution id and offset now, because we could
					// not know the type of this structure during scoping.
					match self.analyze_member_access(&structure, member)
					{
						Ok(i) =>
						{
							*offset = Some(i);
						}
						Err(poison) =>
						{
							reference.base = match poison
							{
								Poison::Error { error, partial: _ } =>
								{
									Err(error.into())
								}
								Poison::Poisoned => Err(Poison::Poisoned),
							};
							return Some(Err(Poison::Poisoned));
						}
					};
					// Once we have scoped the member, we can get its type.
					match self.get_symbol(&member)
					{
						Some(Ok(member_type)) =>
						{
							x = member_type.fully_dereferenced();
						}
						Some(Err(poison)) =>
						{
							// TODO use num_step_taken to cut off reference
							let _ = num_steps_taken;
							return Some(Err(poison));
						}
						None =>
						{
							return Some(Err(Poison::Poisoned));
						}
					}
				}
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

	fn align_struct(
		&mut self,
		identifier: &Identifier,
		members: &[Member],
		structural_type: Poisonable<ValueType>,
	) -> Poisonable<ValueType>
	{
		let structure = Structure {
			identifier: identifier.clone(),
			members: members.to_vec(),
		};
		self.structures.insert(identifier.resolution_id, structure);

		let structural_type = match structural_type
		{
			Ok(x) => x,
			Err(error) => return Err(error),
		};

		// TODO determine alignment of struct / of its members
		let alignment = 1;

		let mut total_size_in_bytes = 0;
		for member in members
		{
			match &member.value_type
			{
				Ok(vt) => match vt.known_aligned_size_in_bytes()
				{
					Some(size_in_bytes) =>
					{
						total_size_in_bytes += size_in_bytes;
					}
					None =>
					{
						// We do not yet have enough type information.
						return Ok(structural_type);
					}
				},
				Err(_poison) =>
				{
					return Err(Poison::Poisoned);
				}
			}
		}

		// A struct without members has to be at least 1 byte.
		if total_size_in_bytes == 0
		{
			total_size_in_bytes = 1;
		}

		// Round up to the nearest alignment.
		let aligned_size_in_bytes =
			(total_size_in_bytes + alignment - 1) / alignment;

		match structural_type
		{
			ValueType::Struct {
				identifier,
				size_in_bytes,
			} =>
			{
				assert_eq!(size_in_bytes, aligned_size_in_bytes);
				Ok(ValueType::Struct {
					identifier,
					size_in_bytes,
				})
			}
			ValueType::Word {
				identifier,
				size_in_bytes: declared_size_in_bytes,
			} =>
			{
				if declared_size_in_bytes == aligned_size_in_bytes
				{
					Ok(ValueType::Word {
						identifier,
						size_in_bytes: declared_size_in_bytes,
					})
				}
				else
				{
					Err(Poison::Error {
						error: Error::WordSizeMismatch {
							inferred_size_in_bits: 8 * aligned_size_in_bytes,
							declared_size_in_bits: 8 * declared_size_in_bytes,
							location_of_identifier: identifier.location.clone(),
							location_of_keyword: identifier.location.clone(),
						},
						partial: Some(ValueType::Word {
							identifier,
							size_in_bytes: declared_size_in_bytes,
						}),
					})
				}
			}
			ValueType::UnresolvedStructOrWord { .. } => Ok(ValueType::Struct {
				identifier: identifier.clone(),
				size_in_bytes: aligned_size_in_bytes,
			}),
			_ => unreachable!(),
		}
	}

	fn analyze_member_access(
		&self,
		base: &Identifier,
		access: &mut Identifier,
	) -> Result<usize, Poison>
	{
		let structure = match self.structures.get(&base.resolution_id)
		{
			Some(structure) => structure,
			None => unreachable!(),
		};
		for (offset, member) in structure.members.iter().enumerate()
		{
			let name = match &member.name
			{
				Ok(name) => name,
				Err(_poison) => return Err(Poison::Poisoned),
			};
			if name.name == access.name
			{
				// Set resolution id now, because we could not know the
				// type of this structure during scoping.
				access.resolution_id = name.resolution_id;

				return Ok(offset);
			}
		}
		let error = Error::UndefinedMember {
			name_of_member: access.name.clone(),
			name_of_structure: base.name.clone(),
			location: access.location.clone(),
			location_of_declaration: structure.identifier.location.clone(),
		};
		Err(Poison::Error {
			error,
			partial: None,
		})
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
			let parameters: Vec<Parameter> =
				parameters.into_iter().map(|x| x.analyze(typer)).collect();

			typer.declare_function_parameters(&name, &parameters);

			let rv_identifier = name.return_value();
			let rv_type = return_type.clone();
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
			let parameters: Vec<Parameter> =
				parameters.into_iter().map(|x| x.analyze(typer)).collect();

			typer.declare_function_parameters(&name, &parameters);

			let rv_identifier = name.return_value();
			let rv_type = return_type.clone();
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
		Declaration::Structure {
			name,
			members,
			structural_type,
			flags,
		} =>
		{
			let members: Vec<Member> = members
				.into_iter()
				.map(|x| {
					typer.contextual_type = Some(structural_type.clone());
					x.analyze(typer)
				})
				.collect();

			let structural_type =
				typer.align_struct(&name, &members, structural_type);
			let result = typer.put_symbol(&name, Some(structural_type.clone()));
			let structural_type = match (result, structural_type)
			{
				(Ok(()), structural_type) => structural_type,
				(Err(error), Ok(structural_type)) => Err(Poison::Error {
					error,
					partial: Some(structural_type),
				}),
				(Err(_error), structural_type) => structural_type,
			};

			Declaration::Structure {
				name,
				members,
				structural_type,
				flags,
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

fn needs_aligning(declaration: &Declaration) -> bool
{
	match declaration
	{
		Declaration::Constant { .. } => false,
		Declaration::Function { .. } => false,
		Declaration::FunctionHead { .. } => true,
		Declaration::Structure {
			name: _,
			members: _,
			structural_type,
			flags: _,
		} => match structural_type
		{
			Ok(ValueType::UnresolvedStructOrWord { .. }) => true,
			Ok(_) => false,
			Err(_) => false,
		},
		Declaration::PreprocessorDirective { .. } => unreachable!(),
		Declaration::Poison(Poison::Error { .. }) => false,
		Declaration::Poison(Poison::Poisoned) => false,
	}
}

fn prealign(declaration: Declaration, typer: &mut Typer) -> Declaration
{
	match declaration
	{
		Declaration::Constant { .. } => declaration,
		Declaration::Function { .. } => declaration,
		Declaration::FunctionHead { .. } => declaration,
		Declaration::Structure {
			name,
			members,
			structural_type,
			flags,
		} =>
		{
			let members: Vec<Member> = members
				.into_iter()
				.map(|x| {
					typer.contextual_type = Some(structural_type.clone());
					x.analyze(typer)
				})
				.collect();

			let structural_type =
				typer.align_struct(&name, &members, structural_type);
			let result = typer.put_symbol(&name, Some(structural_type.clone()));
			let structural_type = match (result, structural_type)
			{
				(Ok(()), structural_type) => structural_type,
				(Err(error), Ok(structural_type)) => Err(Poison::Error {
					error,
					partial: Some(structural_type),
				}),
				(Err(_error), structural_type) => structural_type,
			};

			Declaration::Structure {
				name,
				members,
				structural_type,
				flags,
			}
		}
		Declaration::PreprocessorDirective { .. } => unreachable!(),
		Declaration::Poison(Poison::Error { .. }) => declaration,
		Declaration::Poison(Poison::Poisoned) => declaration,
	}
}

trait Analyzable
{
	fn analyze(self, typer: &mut Typer) -> Self;
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
			Declaration::Function { return_type, .. } => return_type.clone(),
			Declaration::FunctionHead { return_type, .. } =>
			{
				return_type.clone()
			}
			Declaration::Structure {
				structural_type, ..
			} => Some(structural_type.clone()),
			Declaration::PreprocessorDirective { .. } => unreachable!(),
			Declaration::Poison(_) => Some(Err(Poison::Poisoned)),
		}
	}
}

impl Analyzable for Declaration
{
	fn analyze(self, typer: &mut Typer) -> Self
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
				let value = match typer
					.put_symbol(&name.inferred(), value.value_type())
				{
					Ok(()) => value,
					Err(error) => Expression::Poison(Poison::Error {
						error,
						partial: Some(Box::new(value)),
					}),
				};
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
				typer.contextual_type = return_type.clone();
				let prebody: FunctionBody = body.clone().analyze(typer);
				// Pre-analyze the statements in reverse, because there might
				// be chains of untyped declarations, e.g.
				//   var x = 1;
				//   var y = x;
				// whose types won't be determined in the forward pass.
				typer.contextual_type = return_type.clone();
				for statement in prebody.statements.into_iter().rev()
				{
					let _unused: Statement = statement.analyze(typer);
				}

				typer.contextual_type = return_type.clone();
				let body = body.analyze(typer);

				let return_type =
					analyze_return_value(&name, return_type, &body, typer);
				Declaration::Function {
					name,
					parameters,
					body: Ok(body),
					return_type,
					flags,
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
			Declaration::Structure { .. } =>
			{
				// Structures are typed before anything else, because they are
				// types that may appear through the other declarations.
				self
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
	return_type: Option<Poisonable<ValueType>>,
	body: &FunctionBody,
	typer: &mut Typer,
) -> Option<Poisonable<ValueType>>
{
	let return_type = match return_type.transpose()
	{
		Ok(x) => x,
		Err(poison) => return Some(Err(poison)),
	};
	let body_value_type = match body.value_type().transpose()
	{
		Ok(x) => x,
		Err(_poison) => return Some(Err(Poison::Poisoned)),
	};
	let result = match (&return_type, &body.return_value, body_value_type)
	{
		(Some(rt), Some(_), Some(vt)) =>
		{
			let rv_identifier = function.return_value().inferred();
			match typer.put_symbol(&rv_identifier, Some(Ok(vt)))
			{
				Ok(()) => Ok(()),
				Err(Error::ConflictingTypes {
					current_type: vt, ..
				}) => Err(Error::ConflictingReturnValue {
					inferred_type: vt,
					declared_type: rt.clone(),
					location_of_return_value: body
						.return_value_identifier
						.location
						.clone(),
					location_of_declaration: function.location.clone(),
				}),
				Err(error) => Err(error),
			}
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
		(None, Some(_), Some(rvt)) => Err(Error::MissingReturnType {
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
		(None, None, Some(_)) => unreachable!(),
		(None, None, None) => Ok(()),
	};
	match result
	{
		Ok(()) => Ok(return_type).transpose(),
		Err(error) => Some(Err(error.into())),
	}
}

impl Analyzable for Member
{
	fn analyze(self, typer: &mut Typer) -> Self
	{
		let contextual_structure_type = typer.contextual_type.take();
		let value_type = match &self.name
		{
			Ok(name) =>
			{
				let value_type = self.value_type.analyze(typer);
				let is_legal = match (contextual_structure_type, &value_type)
				{
					(Some(Ok(_)), Err(_)) => true,
					(Some(Ok(ValueType::Struct { .. })), Ok(vt)) =>
					{
						vt.can_be_struct_member()
					}
					(Some(Ok(ValueType::Word { .. })), Ok(vt)) =>
					{
						vt.can_be_word_member()
					}
					(
						Some(Ok(ValueType::UnresolvedStructOrWord { .. })),
						Ok(vt),
					) => vt.can_be_struct_member(),
					(Some(Ok(_)), _) => unreachable!(),
					(Some(Err(_)), _) => true,
					(None, _) => unreachable!(),
				};
				let value_type = value_type.and_then(|vt| {
					if is_legal
					{
						Ok(vt)
					}
					else
					{
						Err(Poison::Error {
							error: Error::IllegalMemberType {
								value_type: vt.clone(),
								location: name.location.clone(),
							},
							partial: Some(vt.clone()),
						})
					}
				});
				match typer.put_symbol(name, Some(value_type.clone()))
				{
					Ok(()) => typer.get_symbol(name).unwrap_or(value_type),
					Err(error) => Err(error.into()),
				}
			}
			Err(_poison) => self.value_type,
		};
		Member {
			name: self.name,
			value_type,
		}
	}
}

impl Analyzable for Parameter
{
	fn analyze(self, typer: &mut Typer) -> Self
	{
		let value_type = match &self.name
		{
			Ok(name) =>
			{
				let value_type = self.value_type.analyze(typer);
				match typer.put_symbol(name, Some(value_type.clone()))
				{
					Ok(()) => typer.get_symbol(name).unwrap_or(value_type),
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
	fn analyze(self, typer: &mut Typer) -> Self
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
	fn analyze(self, typer: &mut Typer) -> Self
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
	fn analyze(self, typer: &mut Typer) -> Self
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
				let declared_type = declared_type.analyze(typer);
				let recoverable_error =
					typer.put_symbol(&name, Some(declared_type.clone()));
				let declared_type =
					typer.get_symbol(&name).unwrap_or(declared_type);
				typer.contextual_type = Some(declared_type.clone());
				let value = value.analyze(typer);
				typer.contextual_type = None;
				let value_type = if let Err(error) = recoverable_error
				{
					Some(Err(error.into()))
				}
				else if let Some(inferred_type) = value.value_type()
				{
					let result = typer.put_symbol(
						&name.inferred(),
						Some(inferred_type.clone()),
					);
					match (result, inferred_type, declared_type)
					{
						(Err(error), _, _) => Some(Err(error.into())),
						(Ok(()), _, Err(poison)) => Some(Err(poison)),
						(Ok(()), Err(poison), Ok(_)) => Some(Err(poison)),
						(Ok(()), Ok(_), Ok(declared_type)) =>
						{
							let resulting_type = typer
								.get_symbol(&name)
								.unwrap_or(Ok(declared_type));
							Some(resulting_type)
						}
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
				typer.contextual_type = typer.get_symbol(&name.inferred());
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
				value_type: Some(declared_type),
				location,
			} =>
			{
				let declared_type = declared_type.analyze(typer);
				let recoverable_error =
					typer.put_symbol(&name, Some(declared_type.clone()));
				let value_type = match recoverable_error
				{
					Ok(()) => Some(declared_type),
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
				let value_type = typer.get_symbol(&name.inferred());
				let value_type = infer_for_declaration(value_type);
				Statement::Declaration {
					name,
					value: None,
					value_type,
					location,
				}
			}
			Statement::Assignment {
				mut reference,
				value,
				location,
			} =>
			{
				let ref_type = typer.get_type_of_reference(&mut reference);
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
	fn analyze(self, typer: &mut Typer) -> Self
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
	fn analyze(self, typer: &mut Typer) -> Self
	{
		let name = self.get_identifier().inferred();
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
			Expression::Structural {
				structural_type, ..
			} => Some(structural_type.clone()),
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
	fn analyze(self, typer: &mut Typer) -> Self
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
					Some(Err(poison)) => Some(Err(poison)),
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
					Some(Err(poison)) => Some(Err(poison)),
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
						let element_type = typer.get_symbol(&name.inferred());

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
			Expression::Structural {
				members,
				structural_type,
				location,
			} => analyze_structural(members, structural_type, location, typer),
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
				// We cannot infer the type of `expression` from context,
				// because the context will always think it is `coerced_type`.
				typer.contextual_type = None;
				let expr = expression.analyze(typer);
				Expression::PrimitiveCast {
					expression: Box::new(expr),
					coerced_type,
					location,
					location_of_type,
				}
			}
			Expression::LengthOfArray { mut reference } =>
			{
				let base_type = typer.get_type_of_base(&reference.base);
				let array_type = typer.get_type_of_reference(&mut reference);
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
				let rv_identifier = name.return_value().inferred();
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

fn analyze_structural(
	members: Vec<MemberExpression>,
	structural_type: Poisonable<ValueType>,
	location: Location,
	typer: &mut Typer,
) -> Expression
{
	match structural_type.analyze(typer)
	{
		Ok(structural_type) =>
		{
			let structure_identifier = match &structural_type
			{
				ValueType::Struct { identifier, .. } => identifier,
				ValueType::Word { identifier, .. } => identifier,
				_ => unreachable!(),
			};
			let members = members
				.into_iter()
				.map(|member| {
					let mut contextual_type = None;
					let (name, offset) = match member.name
					{
						Ok(mut name) =>
						{
							let offset = typer.analyze_member_access(
								structure_identifier,
								&mut name,
							);
							contextual_type = typer.get_symbol(&name);
							match offset
							{
								Ok(offset) => (Ok(name), Some(offset)),
								Err(poison) =>
								{
									let poison = match poison
									{
										Poison::Error { error, partial: _ } =>
										{
											Poison::Error {
												error,
												partial: Some(name),
											}
										}
										Poison::Poisoned => Poison::Poisoned,
									};
									(Err(poison), None)
								}
							}
						}
						Err(poison) => (Err(poison), None),
					};
					typer.contextual_type = contextual_type;
					let expression = member.expression.analyze(typer);
					MemberExpression {
						name,
						offset,
						expression,
					}
				})
				.collect();
			Expression::Structural {
				members,
				structural_type: Ok(structural_type),
				location,
			}
		}
		Err(poison) =>
		{
			let poison = match poison
			{
				Poison::Error {
					error,
					partial: Some(structural_type),
				} => Poison::Error {
					error,
					partial: Some(Box::new(Expression::Structural {
						members,
						structural_type: Ok(structural_type),
						location,
					})),
				},
				Poison::Error {
					error,
					partial: None,
				} => Poison::Error {
					error,
					partial: None,
				},
				Poison::Poisoned => Poison::Poisoned,
			};
			Expression::Poison(poison)
		}
	}
}

impl Analyzable for ReferenceStep
{
	fn analyze(self, typer: &mut Typer) -> Self
	{
		match self
		{
			ReferenceStep::Element {
				argument,
				is_endless,
			} =>
			{
				typer.contextual_type = Some(Ok(ValueType::Usize));
				let argument = argument.analyze(typer);
				typer.contextual_type = None;
				ReferenceStep::Element {
					argument: Box::new(argument),
					is_endless,
				}
			}
			ReferenceStep::Member {
				member: _,
				offset: _,
			} => self,
			ReferenceStep::Autodeslice { offset: _ } => self,
			ReferenceStep::Autoderef => ReferenceStep::Autoderef,
			ReferenceStep::Autoview => ReferenceStep::Autoview,
		}
	}
}

fn analyze_assignment_steps(
	typer: &mut Typer,
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
		let step = match step
		{
			ReferenceStep::Element {
				argument,
				is_endless: _,
			} =>
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
				let is_endless = match &current_type
				{
					ValueType::Array { .. } => Some(false),
					ValueType::Slice { .. } => Some(false),
					ValueType::EndlessArray { .. } => Some(true),
					ValueType::Arraylike { .. } => None,
					_ => None,
				};
				match current_type.get_element_type()
				{
					Some(element_type) =>
					{
						current_type = element_type;
					}
					None => unreachable!(),
				}
				ReferenceStep::Element {
					argument,
					is_endless,
				}
			}
			ReferenceStep::Member { member, offset } =>
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
				match typer.get_symbol(&member)
				{
					Some(Ok(member_type)) =>
					{
						current_type = member_type;
					}
					Some(Err(_poison)) => unreachable!(),
					None => unreachable!(),
				}
				ReferenceStep::Member { member, offset }
			}
			ReferenceStep::Autoderef => match current_type
			{
				ValueType::Pointer { deref_type } =>
				{
					current_type = *deref_type;
					step
				}
				_ => unreachable!(),
			},
			ReferenceStep::Autoview => match current_type
			{
				ValueType::View { deref_type } =>
				{
					current_type = *deref_type;
					step
				}
				_ => unreachable!(),
			},
			ReferenceStep::Autodeslice { offset: 0 } => step,
			ReferenceStep::Autodeslice { offset: 1 } =>
			{
				current_type = ValueType::Usize;
				step
			}
			ReferenceStep::Autodeslice { offset: _ } => unreachable!(),
		};
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
				typer,
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
		let known_type = typer.get_type_of_reference(&mut self);
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
				self.simple_deref(base_type, deref_type, typer)
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
			(Some(Ok(def)), _, _) =>
			{
				let base_type = Some(Ok(def));
				let deref_type = base_type.clone();
				self.simple_deref(base_type, deref_type, typer)
			}
			(Some(Err(base_type_poison)), _, _) =>
			{
				// Keep the error.
				let poison = match base_type_poison
				{
					Poison::Error { error, partial: _ } => Poison::Error {
						error,
						partial: Some(Box::new(Expression::Deref {
							reference: self,
							deref_type: Some(Err(Poison::Poisoned)),
						})),
					},
					Poison::Poisoned => Poison::Poisoned,
				};
				Expression::Poison(poison)
			}
			(None, deref_type, _) =>
			{
				let base_type = deref_type.clone();
				self.simple_deref(base_type, deref_type, typer)
			}
		}
	}

	fn simple_deref(
		self,
		base_type: Option<Poisonable<ValueType>>,
		deref_type: Option<Poisonable<ValueType>>,
		typer: &mut Typer,
	) -> Expression
	{
		let base = match &self.base
		{
			Ok(base) => base,
			Err(_poison) => unreachable!(),
		};

		let full_type =
			build_type_of_reference(base_type, &self.steps, self.address_depth);
		match typer.put_symbol(base, full_type)
		{
			Ok(()) =>
			{
				let is_concrete =
					self.steps.iter().all(|step| step.is_concrete());
				let deref_type = deref_type.filter(|_| is_concrete);
				Expression::Deref {
					reference: self,
					deref_type,
				}
			}
			Err(error) => Expression::Poison(Poison::Error {
				error,
				partial: Some(Box::new(Expression::Deref {
					reference: self,
					deref_type,
				})),
			}),
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
			Err(_poison) => unreachable!(),
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
					if ct.can_coerce_address_into_pointer_to(deref_type)
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
					Some(ReferenceStep::Element {
						argument,
						is_endless,
					}),
				) => match deref_type.as_ref()
				{
					ValueType::Arraylike { element_type } =>
					{
						let step = ReferenceStep::Element {
							argument: argument.clone(),
							is_endless: *is_endless,
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
					Some(ReferenceStep::Element {
						argument,
						is_endless,
					}),
				) => match deref_type.as_ref()
				{
					ValueType::Arraylike { element_type } =>
					{
						let step = ReferenceStep::Element {
							argument: argument.clone(),
							is_endless: *is_endless,
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
					Some(ReferenceStep::Element {
						argument,
						is_endless: _,
					}),
				) =>
				{
					let step = ReferenceStep::Element {
						argument: argument.clone(),
						is_endless: Some(false),
					};
					taken_steps.push(step);
					available_steps.next();
					current_type = *element_type;
				}

				(
					ValueType::EndlessArray { element_type },
					_,
					Some(ReferenceStep::Element {
						argument,
						is_endless: _,
					}),
				) =>
				{
					let step = ReferenceStep::Element {
						argument: argument.clone(),
						is_endless: Some(true),
					};
					taken_steps.push(step);
					available_steps.next();
					current_type = *element_type;
				}

				(
					ValueType::Slice { element_type },
					_,
					Some(ReferenceStep::Element {
						argument,
						is_endless: _,
					}),
				) =>
				{
					let autostep = ReferenceStep::Autodeslice { offset: 0 };
					taken_steps.push(autostep);

					let step = ReferenceStep::Element {
						argument: argument.clone(),
						is_endless: Some(false),
					};
					taken_steps.push(step);
					available_steps.next();
					current_type = *element_type;
				}

				(
					ValueType::Struct { .. },
					_,
					Some(ReferenceStep::Member { member, offset }),
				)
				| (
					ValueType::Word { .. },
					_,
					Some(ReferenceStep::Member { member, offset }),
				) =>
				{
					let member_type = match typer.get_symbol(&member)
					{
						Some(Ok(member_type)) => member_type,
						Some(Err(_)) => unreachable!(),
						None => unreachable!(),
					};
					let step = ReferenceStep::Member {
						member: member.clone(),
						offset: offset.clone(),
					};
					taken_steps.push(step);
					available_steps.next();
					current_type = member_type;
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

				(ct, tt, step) => panic!(
					"failed to autoderef {}, target type: {:?}, current type: \
					 {:?}, available step: {:?}, ad: {:?}, taken address: {:?}",
					self.location.format(),
					tt,
					ct,
					step,
					self.address_depth,
					take_address
				),
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
			ReferenceStep::Element { .. } =>
			{
				full_type = ValueType::Arraylike {
					element_type: Box::new(full_type),
				};
			}
			ReferenceStep::Member { .. } =>
			{
				full_type =
					ValueType::UnresolvedStructOrWord { identifier: None };
			}
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

impl Analyzable for Poisonable<ValueType>
{
	fn analyze(self, typer: &mut Typer) -> Self
	{
		self.and_then(|x| analyze_type(x, typer).map_err(|e| e.into()))
	}
}

fn analyze_type(
	value_type: ValueType,
	typer: &mut Typer,
) -> Poisonable<ValueType>
{
	match value_type
	{
		ValueType::UnresolvedStructOrWord {
			identifier: Some(identifier),
		} =>
		{
			let current_type = ValueType::UnresolvedStructOrWord {
				identifier: Some(identifier.clone()),
			};
			match typer.put_symbol(&identifier, Some(Ok(current_type)))
			{
				Ok(()) => match typer.get_symbol(&identifier)
				{
					Some(result) => result,
					None => Ok(ValueType::UnresolvedStructOrWord {
						identifier: Some(identifier),
					}),
				},
				Err(error) => Err(error.into()),
			}
		}
		ValueType::UnresolvedStructOrWord { identifier: None } =>
		{
			Ok(ValueType::UnresolvedStructOrWord { identifier: None })
		}
		ValueType::Struct {
			identifier,
			size_in_bytes,
		} => Ok(ValueType::Struct {
			identifier,
			size_in_bytes,
		}),
		ValueType::Word {
			identifier,
			size_in_bytes,
		} => Ok(ValueType::Word {
			identifier,
			size_in_bytes,
		}),
		ValueType::Int8 => Ok(ValueType::Int8),
		ValueType::Int16 => Ok(ValueType::Int16),
		ValueType::Int32 => Ok(ValueType::Int32),
		ValueType::Int64 => Ok(ValueType::Int64),
		ValueType::Int128 => Ok(ValueType::Int128),
		ValueType::Uint8 => Ok(ValueType::Uint8),
		ValueType::Uint16 => Ok(ValueType::Uint16),
		ValueType::Uint32 => Ok(ValueType::Uint32),
		ValueType::Uint64 => Ok(ValueType::Uint64),
		ValueType::Uint128 => Ok(ValueType::Uint128),
		ValueType::Usize => Ok(ValueType::Usize),
		ValueType::Bool => Ok(ValueType::Bool),
		ValueType::Char => Ok(ValueType::Char),
		ValueType::String => Ok(ValueType::String),
		ValueType::Array {
			element_type,
			length,
		} => Poison::apply_regardless(
			analyze_type(*element_type, typer),
			|element_type| ValueType::Array {
				element_type: Box::new(element_type),
				length,
			},
		),
		ValueType::Slice { element_type } => Poison::apply_regardless(
			analyze_type(*element_type, typer),
			|element_type| ValueType::Slice {
				element_type: Box::new(element_type),
			},
		),
		ValueType::EndlessArray { element_type } => Poison::apply_regardless(
			analyze_type(*element_type, typer),
			|element_type| ValueType::EndlessArray {
				element_type: Box::new(element_type),
			},
		),
		ValueType::Arraylike { element_type } => Poison::apply_regardless(
			analyze_type(*element_type, typer),
			|element_type| ValueType::Arraylike {
				element_type: Box::new(element_type),
			},
		),
		ValueType::Pointer { deref_type } => Poison::apply_regardless(
			analyze_type(*deref_type, typer),
			|deref_type| ValueType::Pointer {
				deref_type: Box::new(deref_type),
			},
		),
		ValueType::View { deref_type } => Poison::apply_regardless(
			analyze_type(*deref_type, typer),
			|deref_type| ValueType::View {
				deref_type: Box::new(deref_type),
			},
		),
	}
}
