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
use crate::value_type::MAXIMUM_ALIGNMENT;

use enumset::EnumSet;

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
	let mut declarations = program;
	for declaration in visit_structures(&mut declarations)
	{
		prealign(declaration, &mut typer);
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

fn visit_structures(declarations: &mut [Declaration]) -> Vec<&mut Declaration>
{
	let mut declarations: Vec<&mut Declaration> = declarations
		.iter_mut()
		.filter(|x| get_structure_depth(x).is_some())
		.collect();
	// Sort the structure declarations by their depth, from low to high.
	declarations.sort_by_key(|x| get_structure_depth(x));
	declarations
}

fn get_structure_depth(declaration: &Declaration) -> Option<u32>
{
	match declaration
	{
		Declaration::Constant { .. } => None,
		Declaration::Function { .. } => None,
		Declaration::FunctionHead { .. } => None,
		Declaration::Structure { depth, .. } => match depth
		{
			Some(Ok(depth)) => Some(*depth),
			Some(Err(_poison)) => Some(0),
			None => unreachable!(),
		},
		Declaration::Import { .. } => None,
		Declaration::Poison(_) => None,
	}
}

struct Typer
{
	symbols: std::collections::HashMap<u32, Symbol>,
	functions: std::collections::HashMap<u32, Function>,
	structures: std::collections::HashMap<u32, Structure>,
	contextual_type: Option<Poisonable<ValueType>>,
}

#[derive(Debug)]
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

	if ot == &vt || ot.can_be_concretization_of(&vt)
	{
		Ok(())
	}
	else if symbol.identifier.is_authoritative && vt.can_be_declared_as(ot)
	{
		// Keep symbol.identifier the same.
		symbol.value_type = Ok(vt);
		Ok(())
	}
	else if new_identifier.is_authoritative
		&& (vt.can_be_concretization_of(ot) || vt.can_coerce_into(ot))
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
							value_type: Ok(vt),
						},
					);
					Ok(())
				}
			}
			Some(Err(poison)) =>
			{
				self.poison_symbol(identifier, poison);
				Ok(())
			}
			None => Ok(()),
		}
	}

	fn poison_symbol(&mut self, identifier: &Identifier, poison: Poison)
	{
		if let Some(symbol) = self.symbols.get_mut(&identifier.resolution_id)
		{
			symbol.value_type = Err(poison);
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
			Err(_poison) =>
			{
				reference.base = Err(Poison::Poisoned);
				return Some(Err(Poison::Poisoned));
			}
		};
		for (num_steps_taken, step) in reference.steps.iter_mut().enumerate()
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
							let error = Error::NotAnArray {
								current_type: x,
								location,
								previous: old_identifier.location.clone(),
							};
							return Some(Err(Poison::Error(error)));
						}
					}
				}
				ReferenceStep::Member { member, offset } =>
				{
					let structure = match &x
					{
						ValueType::Struct { identifier } => identifier,
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
					match self.analyze_member_access(structure, member)
					{
						Ok(i) =>
						{
							*offset = Some(i);
						}
						Err(poison) =>
						{
							reference.base = Err(poison);
							return Some(Err(Poison::Poisoned));
						}
					};
					// Once we have scoped the member, we can get its type.
					match self.get_symbol(member)
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
			.map(|(argument, parameter_hint)| {
				self.contextual_type = Some(parameter_hint.clone());
				let expr = argument.analyze(self);
				match (expr.value_type(), parameter_hint)
				{
					(Some(Ok(vt)), Ok(pt)) if vt == pt => expr,
					(Some(Ok(vt)), Ok(pt)) if vt.can_coerce_into(&pt) =>
					{
						Expression::Autocoerce {
							expression: Box::new(expr),
							coerced_type: pt,
						}
					}
					_ => expr,
				}
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

		let needs_to_be_aligned = match structural_type
		{
			ValueType::Struct { .. } => false,
			ValueType::Word { .. } => true,
			_ => unreachable!(),
		};

		let mut total_alignment = 1;
		let mut total_size_in_bytes = 0;
		for member in members
		{
			match &member.value_type
			{
				Ok(vt) => match vt.known_size_in_bytes_as_word_member()
				{
					Some(size_in_bytes) =>
					{
						let alignment = size_in_bytes
							.next_power_of_two()
							.min(MAXIMUM_ALIGNMENT);
						total_size_in_bytes =
							align(total_size_in_bytes, alignment);
						total_size_in_bytes += size_in_bytes;
						if alignment > total_alignment
						{
							total_alignment = alignment;
						}
					}
					None =>
					{
						assert!(!needs_to_be_aligned);
					}
				},
				Err(_poison) =>
				{
					return Err(Poison::Poisoned);
				}
			}
		}

		let aligned_size_in_bytes = align(total_size_in_bytes, total_alignment);

		match structural_type
		{
			ValueType::Struct { .. } => Ok(structural_type),
			ValueType::Word {
				identifier,
				size_in_bytes: declared_size_in_bytes,
			} =>
			{
				if aligned_size_in_bytes <= declared_size_in_bytes
				{
					Ok(ValueType::Word {
						identifier,
						size_in_bytes: declared_size_in_bytes,
					})
				}
				else
				{
					let error = Error::WordSizeMismatch {
						inferred_size_in_bits: 8 * aligned_size_in_bytes,
						declared_size_in_bits: 8 * declared_size_in_bytes,
						location_of_identifier: identifier.location.clone(),
						location_of_keyword: identifier.location,
					};
					Err(Poison::Error(error))
				}
			}
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
		Err(Poison::Error(error))
	}
}

fn align(current_size: usize, alignment: usize) -> usize
{
	// Round up to the nearest multiple of alignment.
	alignment * ((current_size + alignment - 1) / alignment)
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
			depth,
			location_of_declaration,
			location_of_type,
		} =>
		{
			let value_type = match value_type.analyze(typer)
			{
				Ok(value_type) => match fix_type_for_flags(
					value_type,
					FixContext::Const,
					&flags,
					&location_of_type,
					&location_of_declaration,
				)
				{
					Ok(vt) if !vt.can_be_constant() =>
					{
						Err(Poison::Error(Error::IllegalConstantType {
							value_type: vt,
							location: name.location.clone(),
						}))
					}
					Ok(value_type) => Ok(value_type),
					Err(error) => Err(error.into()),
				},
				Err(poison) => Err(poison),
			};

			match typer.put_symbol(&name, Some(value_type.clone()))
			{
				Ok(()) => Declaration::Constant {
					name,
					value,
					value_type,
					flags,
					depth,
					location_of_declaration,
					location_of_type,
				},
				Err(error) => Declaration::Poison(Poison::Error(error)),
			}
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
			let parameters: Vec<Parameter> = parameters
				.into_iter()
				.map(|x| {
					x.analyze_and_fix(&flags, &location_of_declaration, typer)
				})
				.collect();

			typer.declare_function_parameters(&name, &parameters);

			let return_type = fix_return_type_for_flags(
				return_type.analyze(typer),
				&flags,
				&location_of_declaration,
				&location_of_return_type,
			);

			let rv_identifier = name.return_value();
			let rv_type = Some(return_type.clone());
			match typer.put_symbol(&rv_identifier, rv_type)
			{
				Ok(()) => Declaration::Function {
					name,
					parameters,
					body,
					return_type,
					flags,
					location_of_declaration,
					location_of_return_type,
				},
				Err(error) => Declaration::Poison(Poison::Error(error)),
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
			let parameters: Vec<Parameter> = parameters
				.into_iter()
				.map(|x| {
					x.analyze_and_fix(&flags, &location_of_declaration, typer)
				})
				.collect();

			typer.declare_function_parameters(&name, &parameters);

			let return_type = fix_return_type_for_flags(
				return_type.analyze(typer),
				&flags,
				&location_of_declaration,
				&location_of_return_type,
			);

			let rv_identifier = name.return_value();
			let rv_type = Some(return_type.clone());
			match typer.put_symbol(&rv_identifier, rv_type)
			{
				Ok(()) => Declaration::FunctionHead {
					name,
					parameters,
					return_type,
					flags,
					location_of_declaration,
					location_of_return_type,
				},
				Err(error) => Declaration::Poison(Poison::Error(error)),
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
			// Analyze the members again because cyclical structures will have
			// members with type UnresolvedStructOrWord instead of Poisoned.
			let members: Vec<Member> = members
				.into_iter()
				.map(|x| {
					typer.contextual_type = Some(structural_type.clone());
					x.analyze_and_fix(&flags, &location_of_declaration, typer)
				})
				.collect();

			let structural_type =
				typer.align_struct(&name, &members, structural_type);
			let result = typer.put_symbol(&name, Some(structural_type.clone()));
			let structural_type = match (structural_type, result)
			{
				(Ok(_), Err(error)) => Err(Poison::Error(error)),
				(structural_type, _) => structural_type,
			};

			Declaration::Structure {
				name,
				members,
				structural_type,
				flags,
				depth,
				location_of_declaration,
			}
		}
		Declaration::Import { .. } => declaration,
		Declaration::Poison(_) => declaration,
	}
}

fn prealign(declaration: &mut Declaration, typer: &mut Typer)
{
	match declaration
	{
		Declaration::Constant { .. } => (),
		Declaration::Function { .. } => (),
		Declaration::FunctionHead { .. } => (),
		Declaration::Structure {
			name,
			members: _,
			structural_type: _,
			flags: _,
			depth: Some(Err(_poison)),
			location_of_declaration: _,
		} =>
		{
			typer.poison_symbol(name, Poison::Poisoned);
		}
		Declaration::Structure {
			name,
			members,
			structural_type,
			flags,
			depth: _,
			location_of_declaration,
		} =>
		{
			*members = std::mem::take(members)
				.into_iter()
				.map(|x| {
					typer.contextual_type = Some(structural_type.clone());
					x.analyze_and_fix(flags, location_of_declaration, typer)
				})
				.collect();

			let aligned_type =
				typer.align_struct(name, members, structural_type.clone());
			let result = typer.put_symbol(name, Some(aligned_type.clone()));
			*structural_type = match (aligned_type, result)
			{
				(Ok(_), Err(error)) => Err(Poison::Error(error)),
				(structural_type, _) => structural_type,
			};
		}
		Declaration::Import { .. } => (),
		Declaration::Poison(_) => (),
	}
}

trait Analyzable
{
	fn analyze(self, typer: &mut Typer) -> Self;
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
				depth,
				location_of_declaration,
				location_of_type,
			} =>
			{
				typer.contextual_type = Some(value_type.clone());
				let value = value.analyze(typer);
				let value = match typer
					.put_symbol(&name.inferred(), value.value_type())
				{
					Ok(()) => value,
					Err(error) => Expression::Poison(Poison::Error(error)),
				};
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
			Declaration::Function {
				name,
				parameters,
				body: Err(poisoned_body),
				return_type,
				flags,
				location_of_declaration,
				location_of_return_type,
			} => Declaration::Function {
				name,
				parameters,
				body: Err(poisoned_body),
				return_type,
				flags,
				location_of_declaration,
				location_of_return_type,
			},
			Declaration::Function {
				name,
				parameters,
				body: Ok(body),
				return_type,
				flags,
				location_of_declaration,
				location_of_return_type,
			} =>
			{
				// All parameters are analyzed before any function bodies.

				// Do not use Void as the contextual type becaue we want an
				// unbiased suggestion for the type of the return value.
				let contextual_return_type = match &return_type
				{
					Ok(ValueType::Void) => None,
					Ok(vt) => Some(Ok(vt.clone())),
					Err(poison) => Some(Err(poison.clone())),
				};

				// Pre-analyze the function body because it might contain
				// untyped declarations, e.g. "var x;", whose types won't be
				// determined in the first pass.
				typer.contextual_type = contextual_return_type.clone();
				let prebody: FunctionBody = body.clone().analyze(typer);
				// Pre-analyze the statements in reverse, because there might
				// be chains of untyped declarations, e.g.
				//   var x = 1;
				//   var y = x;
				// whose types won't be determined in the forward pass.
				typer.contextual_type = contextual_return_type.clone();
				for statement in prebody.statements.into_iter().rev()
				{
					let _unused: Statement = statement.analyze(typer);
				}

				typer.contextual_type = contextual_return_type;
				let body = body.analyze(typer);

				let return_type =
					analyze_return_value(&name, return_type, &body, typer);
				Declaration::Function {
					name,
					parameters,
					body: Ok(body),
					return_type,
					flags,
					location_of_declaration,
					location_of_return_type,
				}
			}
			Declaration::FunctionHead { .. } =>
			{
				// All parameters are analyzed before any function bodies.
				self
			}
			Declaration::Structure { .. } =>
			{
				// Structures are typed before anything else, because they are
				// types that may appear through the other declarations.
				self
			}
			Declaration::Import { .. } => self,
			Declaration::Poison(_) => self,
		}
	}
}

fn analyze_return_value(
	function: &Identifier,
	return_type: Poisonable<ValueType>,
	body: &FunctionBody,
	typer: &mut Typer,
) -> Poisonable<ValueType>
{
	let return_type = match return_type
	{
		Ok(ValueType::Void) => None,
		Ok(x) => Some(x),
		Err(poison) => return Err(poison),
	};
	let body_value_type = match body.value_type().transpose()
	{
		Ok(x) => x,
		Err(_poison) => return Err(Poison::Poisoned),
	};
	let result = match (return_type, &body.return_value, body_value_type)
	{
		(Some(rt), Some(_), Some(vt)) =>
		{
			let rv_identifier = function.return_value().inferred();
			match typer.put_symbol(&rv_identifier, Some(Ok(vt)))
			{
				Ok(()) => Ok(rt),
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
			inferred_type: rvt,
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
		(None, None, None) => Ok(ValueType::Void),
	};
	match result
	{
		Ok(return_type) => Ok(return_type),
		Err(error) => Err(error.into()),
	}
}

impl Member
{
	fn analyze_and_fix(
		self,
		flags: &EnumSet<DeclarationFlag>,
		location_of_declaration: &Location,
		typer: &mut Typer,
	) -> Self
	{
		let contextual_structure_type = typer.contextual_type.take();
		let (in_struct, in_word) = match contextual_structure_type
		{
			Some(Ok(ValueType::Struct { .. })) => (true, false),
			Some(Ok(ValueType::Word { .. })) => (false, true),
			Some(Ok(_)) => unreachable!(),
			Some(Err(_)) => (false, false),
			None => unreachable!(),
		};

		let value_type = match self.value_type.analyze(typer)
		{
			Ok(value_type) =>
			{
				let value_type = fix_type_for_flags(
					value_type,
					FixContext::Member,
					flags,
					&self.location_of_type,
					location_of_declaration,
				);
				let is_legal = match &value_type
				{
					Err(_) => true,
					Ok(vt) if in_word => vt.can_be_word_member(),
					Ok(vt) if in_struct => vt.can_be_struct_member(),
					Ok(_) => true,
				};
				match value_type
				{
					Ok(vt) if is_legal => Ok(vt),
					Ok(vt) => match &self.name
					{
						Ok(name) =>
						{
							Err(Poison::Error(Error::IllegalMemberType {
								value_type: vt,
								in_word,
								location: name.location.clone(),
							}))
						}
						Err(_poison) => Err(Poison::Poisoned),
					},
					Err(error) => Err(error.into()),
				}
			}
			Err(poison) => Err(poison),
		};
		let value_type = match &self.name
		{
			Ok(name) => match typer.put_symbol(name, Some(value_type.clone()))
			{
				Ok(()) => typer.get_symbol(name).unwrap_or(value_type),
				Err(error) => Err(error.into()),
			},
			Err(_poison) => Err(Poison::Poisoned),
		};
		Member {
			name: self.name,
			value_type,
			location_of_type: self.location_of_type,
		}
	}
}

impl Parameter
{
	fn analyze_and_fix(
		self,
		flags: &EnumSet<DeclarationFlag>,
		location_of_declaration: &Location,
		typer: &mut Typer,
	) -> Self
	{
		let value_type = match self.value_type.analyze(typer)
		{
			Ok(value_type) =>
			{
				let value_type = fix_type_for_flags(
					value_type,
					FixContext::Parameter,
					flags,
					&self.location_of_type,
					location_of_declaration,
				);
				match value_type
				{
					Ok(vt) if vt.can_be_parameter() => Ok(vt),
					Ok(vt) => match &self.name
					{
						Ok(name) =>
						{
							Err(Poison::Error(Error::IllegalParameterType {
								value_type: vt,
								location: name.location.clone(),
							}))
						}
						Err(_poison) => Err(Poison::Poisoned),
					},
					Err(error) => Err(error.into()),
				}
			}
			Err(poison) => Err(poison),
		};
		let value_type = match &self.name
		{
			Ok(name) => match typer.put_symbol(name, Some(value_type.clone()))
			{
				Ok(()) => typer.get_symbol(name).unwrap_or(value_type),
				Err(error) => Err(error.into()),
			},
			Err(_poison) => Err(Poison::Poisoned),
		};
		Parameter {
			name: self.name,
			value_type,
			location_of_type: self.location_of_type,
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
						(
							Err(Error::ConflictingTypes {
								name,
								current_type,
								previous_type,
								location: _,
								previous,
							}),
							_,
							_,
						) =>
						{
							let error = Error::ConflictingTypesInAssignment {
								name,
								current_type,
								previous_type,
								location: value.location().clone(),
								previous,
							};
							Some(Err(error.into()))
						}
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
				let reference = reference.analyze_assignment(
					value_type,
					Some(&value),
					typer,
				);
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
			Statement::Poison(_) => self,
		}
	}
}

impl Analyzable for Comparison
{
	fn analyze(self, typer: &mut Typer) -> Self
	{
		let contextual_type = typer.contextual_type.take();
		typer.contextual_type =
			self.right.value_type().or_else(|| contextual_type.clone());
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
					Err(error) => Expression::Poison(Poison::Error(error)),
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
			Expression::StringLiteral { bytes, location: _ } =>
			{
				Some(Ok(ValueType::Array {
					element_type: Box::new(ValueType::Uint8),
					length: bytes.len(),
				}))
			}
			Expression::Structural {
				structural_type, ..
			} => Some(structural_type.clone()),
			Expression::Parenthesized { inner, location: _ } =>
			{
				inner.value_type()
			}
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
			Expression::SizeOfStructure { .. } => Some(Ok(ValueType::Usize)),
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
					right.value_type().or_else(|| contextual_type.clone());
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
					Some(Ok(vt)) => Some(Ok(vt)),
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
					Some(Ok(vt)) => Some(Ok(vt)),
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
					typer.contextual_type = element_type;
					result
				}
				else
				{
					let array_type = typer.contextual_type.take();
					typer.contextual_type = match array_type
					{
						Some(Ok(x)) => x.get_element_type().map(Ok),
						Some(Err(_poison)) => Some(Err(Poison::Poisoned)),
						None => None,
					};
					Ok(())
				};
				let array = array.analyze(typer);
				match put_result
				{
					Ok(()) if array.elements.is_empty() =>
					{
						let element_type = typer.contextual_type.take();
						Expression::ArrayLiteral {
							array,
							element_type,
						}
					}
					Ok(()) =>
					{
						let element_type = typer.get_symbol(&name.inferred());
						Expression::ArrayLiteral {
							array,
							element_type,
						}
					}
					Err(error) => Expression::Poison(Poison::Error(error)),
				}
			}
			Expression::StringLiteral { .. } => self,
			Expression::Structural {
				members,
				structural_type,
				location,
			} => analyze_structural(members, structural_type, location, typer),
			Expression::Parenthesized { inner, location } =>
			{
				let inner = inner.analyze(typer);
				Expression::Parenthesized {
					inner: Box::new(inner),
					location,
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
			} => match analyze_type(coerced_type, typer)
			{
				Ok(coerced_type) =>
				{
					// We cannot infer the type of `expression` from context,
					// because it will always be `coerced_type`.
					typer.contextual_type = None;
					let expr = expression.analyze(typer);
					Expression::PrimitiveCast {
						expression: Box::new(expr),
						coerced_type,
						location,
						location_of_type,
					}
				}
				Err(poison) => Expression::Poison(poison),
			},
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
					Some(Ok(ValueType::ArrayWithNamedLength {
						element_type: _,
						ref named_length,
					})) =>
					{
						let usize_check = typer.put_symbol(
							named_length,
							Some(Ok(ValueType::Usize)),
						);
						let reference = reference
							.analyze_length(base_type, array_type, typer);
						let Reference {
							base,
							steps: _,
							address_depth: _,
							location,
						} = reference;
						let base = match (usize_check, base)
						{
							(Ok(()), Ok(_)) => Ok(named_length.clone()),
							(Ok(()), Err(poison)) => Err(poison),
							(Err(error), _) => Err(error.into()),
						};
						let reference = Reference {
							base,
							steps: Vec::new(),
							address_depth: 0,
							location,
						};
						Expression::Deref {
							reference,
							deref_type: Some(Ok(ValueType::Usize)),
						}
					}
					Some(Ok(ValueType::Slice { .. }))
					| Some(Ok(ValueType::SlicePointer { .. })) =>
					{
						let mut reference = reference
							.analyze_length(base_type, array_type, typer);
						let autostep = ReferenceStep::Autodeslice {
							offset: DesliceOffset::Length,
						};
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
						let error = Error::NotAnArrayWithLength {
							current_type,
							location: reference.location.clone(),
							previous,
						};
						Expression::Poison(Poison::Error(error))
					}
					Some(Err(_poison)) =>
					{
						Expression::LengthOfArray { reference }
					}
					None => Expression::LengthOfArray { reference },
				}
			}
			Expression::SizeOfStructure { name } =>
			{
				let unresolved_type = ValueType::UnresolvedStructOrWord {
					identifier: Some(name.clone()),
				};
				let structure_type = analyze_type(unresolved_type, typer);
				match structure_type
				{
					Ok(ValueType::Struct { .. }) =>
					{
						Expression::SizeOfStructure { name }
					}
					Ok(ValueType::Word { .. }) =>
					{
						Expression::SizeOfStructure { name }
					}
					Ok(_other) => unreachable!(),
					Err(poison) => Expression::Poison(poison),
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
					typer.put_symbol(&rv_identifier, return_type);
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
					Err(error) => Expression::Poison(Poison::Error(error)),
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
								Err(poison) => (Err(poison), None),
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
		Err(poison) => Expression::Poison(poison),
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
						let autostep = ReferenceStep::Autodeslice {
							offset: DesliceOffset::ArrayByView,
						};
						steps.push(autostep);
					}
					ValueType::SlicePointer { .. } =>
					{
						let autostep = ReferenceStep::Autodeslice {
							offset: DesliceOffset::ArrayByPointer,
						};
						steps.push(autostep);
					}
					_ => (),
				}
				let is_endless = match current_type
				{
					ValueType::Array { .. } => Some(false),
					ValueType::ArrayWithNamedLength { .. } => Some(false),
					ValueType::Slice { .. } => Some(false),
					ValueType::SlicePointer { .. } => Some(false),
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
			ReferenceStep::Autodeslice { ref offset } => match offset
			{
				DesliceOffset::ArrayByView => step,
				DesliceOffset::ArrayByPointer => step,
				DesliceOffset::Length =>
				{
					current_type = ValueType::Usize;
					step
				}
			},
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
			(_, array_type) => self.analyze_assignment(array_type, None, typer),
		}
	}

	fn analyze_assignment(
		self,
		value_type: Option<Poisonable<ValueType>>,
		assignment_value: Option<&Expression>,
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

		let member = steps.iter().rev().find_map(|step| step.get_member());
		let member_error = match member
		{
			Some(member) => typer.put_symbol(&member, value_type.clone()),
			None => Ok(()),
		};

		let full_type =
			build_type_of_reference(value_type, &steps, address_depth);
		let assignment_error = match typer.put_symbol(base, full_type)
		{
			Ok(()) => member_error,
			Err(error) => Err(error),
		};
		let base = match (assignment_error, assignment_value)
		{
			(Ok(()), _) => self.base,
			(
				Err(Error::ConflictingTypes {
					name,
					current_type,
					previous_type,
					location: _,
					previous,
				}),
				Some(assignment_value),
			) =>
			{
				let error = Error::ConflictingTypesInAssignment {
					name,
					current_type,
					previous_type,
					location: assignment_value.location().clone(),
					previous,
				};
				Err(Poison::Error(error))
			}
			(Err(error), _) => Err(Poison::Error(error)),
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
				let poison = base_type_poison;
				Expression::Poison(poison)
			}
			(None, Some(Ok(ValueType::SlicePointer { element_type })), _) =>
			{
				// We cannot do a normal simple deref because SlicePointer
				// is not a concretization of array.
				let base_type = Some(Ok(ValueType::Pointer {
					deref_type: Box::new(ValueType::Arraylike {
						element_type: element_type.clone(),
					}),
				}));
				let deref_type =
					Some(Ok(ValueType::SlicePointer { element_type }));
				self.simple_deref(base_type, deref_type, typer)
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
			Err(error) => Expression::Poison(Poison::Error(error)),
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
				(ct, tt, None)
					if ct.can_coerce_address_into(tt)
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
					ValueType::ArrayWithNamedLength {
						element_type,
						named_length: _,
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
					let autostep = ReferenceStep::Autodeslice {
						offset: DesliceOffset::ArrayByView,
					};
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
					ValueType::SlicePointer { element_type },
					_,
					Some(ReferenceStep::Element {
						argument,
						is_endless: _,
					}),
				) =>
				{
					let autostep = ReferenceStep::Autodeslice {
						offset: DesliceOffset::ArrayByPointer,
					};
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
					ValueType::Arraylike { element_type },
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
					let member_type = match typer.get_symbol(member)
					{
						Some(Ok(member_type)) => member_type,
						Some(Err(_)) => unreachable!(),
						None => unreachable!(),
					};
					let step = ReferenceStep::Member {
						member: member.clone(),
						offset: *offset,
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
		match typer.put_symbol(base, full_type)
		{
			Ok(()) => expr,
			Err(error) => Expression::Poison(Poison::Error(error)),
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

impl Analyzable for Poisonable<ValueType>
{
	fn analyze(self, typer: &mut Typer) -> Self
	{
		self.and_then(|x| analyze_type(x, typer))
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
			typer.put_symbol(&identifier, Some(Ok(current_type)))?;
			match typer.get_symbol(&identifier)
			{
				Some(result) => result,
				None => Ok(ValueType::UnresolvedStructOrWord {
					identifier: Some(identifier),
				}),
			}
		}
		ValueType::UnresolvedStructOrWord { identifier: None } =>
		{
			Ok(ValueType::UnresolvedStructOrWord { identifier: None })
		}
		ValueType::Struct { .. } => Ok(value_type),
		ValueType::Word { .. } => Ok(value_type),
		ValueType::Void => Ok(ValueType::Void),
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
		ValueType::Array {
			element_type,
			length,
		} =>
		{
			let element_type = analyze_type(*element_type, typer)?;
			Ok(ValueType::Array {
				element_type: Box::new(element_type),
				length,
			})
		}
		ValueType::ArrayWithNamedLength {
			element_type,
			named_length,
		} =>
		{
			let element_type = analyze_type(*element_type, typer)?;
			// HIER
			Ok(ValueType::ArrayWithNamedLength {
				element_type: Box::new(element_type),
				named_length: todo,
			})
		}
		ValueType::Slice { element_type } =>
		{
			let element_type = analyze_type(*element_type, typer)?;
			Ok(ValueType::Slice {
				element_type: Box::new(element_type),
			})
		}
		ValueType::SlicePointer { element_type } =>
		{
			let element_type = analyze_type(*element_type, typer)?;
			Ok(ValueType::SlicePointer {
				element_type: Box::new(element_type),
			})
		}
		ValueType::EndlessArray { element_type } =>
		{
			let element_type = analyze_type(*element_type, typer)?;
			Ok(ValueType::EndlessArray {
				element_type: Box::new(element_type),
			})
		}
		ValueType::Arraylike { element_type } =>
		{
			let element_type = analyze_type(*element_type, typer)?;
			Ok(ValueType::Arraylike {
				element_type: Box::new(element_type),
			})
		}
		ValueType::Pointer { deref_type } =>
		{
			let deref_type = analyze_type(*deref_type, typer)?;
			Ok(ValueType::Pointer {
				deref_type: Box::new(deref_type),
			})
		}
		ValueType::View { deref_type } =>
		{
			let deref_type = analyze_type(*deref_type, typer)?;
			Ok(ValueType::View {
				deref_type: Box::new(deref_type),
			})
		}
	}
}

fn fix_return_type_for_flags(
	return_type: Poisonable<ValueType>,
	flags: &EnumSet<DeclarationFlag>,
	location_of_type: &Location,
	location_of_declaration: &Location,
) -> Poisonable<ValueType>
{
	let value_type = match return_type
	{
		Ok(ValueType::Void) => return Ok(ValueType::Void),
		Ok(value_type) => value_type,
		Err(poison) => return Err(poison),
	};

	let fixed_type = fix_type_for_flags(
		value_type,
		FixContext::Returned,
		flags,
		location_of_type,
		location_of_declaration,
	);

	let value_type = match fixed_type
	{
		Ok(value_type) => value_type,
		Err(error) => return Err(error.into()),
	};

	if value_type.can_be_returned()
	{
		Ok(value_type)
	}
	else
	{
		let error = Error::IllegalReturnType {
			value_type,
			location: location_of_type.clone(),
		};
		Err(error.into())
	}
}

enum FixContext
{
	Const,
	Member,
	Parameter,
	Returned,
}

fn fix_type_for_flags(
	value_type: ValueType,
	context: FixContext,
	flags: &EnumSet<DeclarationFlag>,
	location_of_type: &Location,
	location_of_declaration: &Location,
) -> Result<ValueType, Error>
{
	if flags.contains(DeclarationFlag::External)
	{
		match value_type
		{
			ValueType::Arraylike { element_type } =>
			{
				let element_type = externalize_type(
					*element_type,
					location_of_type,
					location_of_declaration,
				)?;
				Ok(ValueType::View {
					deref_type: Box::new(ValueType::EndlessArray {
						element_type: Box::new(element_type),
					}),
				})
			}
			_ => externalize_type(
				value_type,
				location_of_type,
				location_of_declaration,
			),
		}
	}
	else
	{
		match value_type
		{
			ValueType::Arraylike { element_type } =>
			{
				Ok(ValueType::Slice { element_type })
			}
			ValueType::Struct { .. } => match context
			{
				FixContext::Parameter | FixContext::Returned =>
				{
					let deref_type = Box::new(value_type);
					Ok(ValueType::View { deref_type })
				}
				FixContext::Const | FixContext::Member => Ok(value_type),
			},
			ValueType::Pointer { deref_type } => match *deref_type
			{
				ValueType::Arraylike { element_type } =>
				{
					Ok(ValueType::SlicePointer { element_type })
				}
				_ => Ok(ValueType::Pointer { deref_type }),
			},
			_ => Ok(value_type),
		}
	}
}

fn externalize_type(
	value_type: ValueType,
	location_of_type: &Location,
	location_of_declaration: &Location,
) -> Result<ValueType, Error>
{
	match value_type
	{
		ValueType::Arraylike { element_type } =>
		{
			let element_type = externalize_type(
				*element_type,
				location_of_type,
				location_of_declaration,
			)?;
			Ok(ValueType::EndlessArray {
				element_type: Box::new(element_type),
			})
		}
		ValueType::Pointer { deref_type } =>
		{
			let deref_type = externalize_type(
				*deref_type,
				location_of_type,
				location_of_declaration,
			)?;
			Ok(ValueType::Pointer {
				deref_type: Box::new(deref_type),
			})
		}
		ValueType::View { deref_type } =>
		{
			let deref_type = externalize_type(
				*deref_type,
				location_of_type,
				location_of_declaration,
			)?;
			Ok(ValueType::View {
				deref_type: Box::new(deref_type),
			})
		}
		ValueType::Int8 => Ok(value_type),
		ValueType::Int16 => Ok(value_type),
		ValueType::Int32 => Ok(value_type),
		ValueType::Int64 => Ok(value_type),
		//ValueType::Int128 is not ok
		ValueType::Uint8 => Ok(value_type),
		ValueType::Uint16 => Ok(value_type),
		ValueType::Uint32 => Ok(value_type),
		ValueType::Uint64 => Ok(value_type),
		//ValueType::Uint128 is not ok
		ValueType::Usize => Ok(value_type),
		//ValueType::Bool is not ok
		_ => Err(Error::TypeNotAllowedInExtern {
			value_type,
			location_of_type: location_of_type.clone(),
			location_of_declaration: location_of_declaration.clone(),
		}),
	}
}
