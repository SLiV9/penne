//
// Part of penne
// Copyright (c) 2020 Sander in 't Veld
// License: MIT
//

use crate::common::*;
use crate::error::Error;

pub fn analyze(program: Vec<Declaration>) -> Vec<Declaration>
{
	let mut analyzer = Analyzer {
		variable_stack: Vec::new(),
		function_list: Vec::new(),
		structures: Vec::new(),
		resolution_id: 1,
	};
	let mut declarations: Vec<Declaration> = program;
	// Predeclare all declarations so that they can reference each other.
	declarations = declarations
		.into_iter()
		.map(|x| predeclare(x, &mut analyzer))
		.collect();
	// After collecting, analyze function bodies and structure members.
	declarations = declarations
		.into_iter()
		.map(|x| x.analyze(&mut analyzer))
		.collect();
	// After collecting again, store the priority of structures.
	analyzer.determine_structure_depths();
	declarations = declarations
		.into_iter()
		.map(|x| postanalyze(x, &mut analyzer))
		.collect();
	declarations
}

struct Analyzer
{
	variable_stack: Vec<Vec<Identifier>>,
	function_list: Vec<Identifier>,
	structures: Vec<Structure>,
	resolution_id: u32,
}

struct Structure
{
	identifier: Identifier,
	contained_structure_ids: std::collections::HashSet<u32>,
	depth: Option<Poisonable<u32>>,
}

impl Analyzer
{
	fn declare_constant(
		&mut self,
		identifier: Identifier,
	) -> Result<Identifier, Error>
	{
		match self.declare_variable(identifier)
		{
			Ok(identifier) => Ok(identifier),
			Err(error) =>
			{
				let error = match error
				{
					Error::DuplicateDeclarationVariable {
						name,
						location,
						previous,
					} => Error::DuplicateDeclarationConstant {
						name,
						location,
						previous,
					},
					error => error,
				};
				Err(error)
			}
		}
	}

	fn declare_variable(
		&mut self,
		identifier: Identifier,
	) -> Result<Identifier, Error>
	{
		let mut recoverable_error = None;
		for scope in &self.variable_stack
		{
			if let Some(previous_identifier) =
				scope.iter().find(|x| x.name == identifier.name)
			{
				recoverable_error = Some(Error::DuplicateDeclarationVariable {
					name: identifier.name.clone(),
					location: identifier.location.clone(),
					previous: previous_identifier.location.clone(),
				});
				break;
			}
		}

		let identifier = Identifier {
			resolution_id: self.resolution_id,
			is_authoritative: true,
			..identifier
		};
		self.resolution_id += 1;

		if let Some(scope) = self.variable_stack.last_mut()
		{
			scope.push(identifier.clone());
		}
		else
		{
			self.variable_stack.push(vec![identifier.clone()]);
		}

		if let Some(error) = recoverable_error
		{
			Err(error)
		}
		else
		{
			Ok(identifier)
		}
	}

	fn use_variable(&self, identifier: Identifier)
		-> Result<Identifier, Error>
	{
		for scope in &self.variable_stack
		{
			if let Some(previous_identifier) =
				scope.iter().find(|x| x.name == identifier.name)
			{
				return Ok(Identifier {
					resolution_id: previous_identifier.resolution_id,
					is_authoritative: false,
					..identifier
				});
			}
		}

		Err(Error::UndefinedVariable {
			name: identifier.name,
			location: identifier.location,
		})
	}

	fn declare_function(
		&mut self,
		identifier: Identifier,
	) -> Result<Identifier, Error>
	{
		let recoverable_error = self
			.function_list
			.iter()
			.find(|x| x.name == identifier.name)
			.map(|previous_identifier| Error::DuplicateDeclarationFunction {
				name: identifier.name.clone(),
				location: identifier.location.clone(),
				previous: previous_identifier.location.clone(),
			});

		let identifier = Identifier {
			resolution_id: self.resolution_id,
			is_authoritative: true,
			..identifier
		};
		self.resolution_id += 1;

		self.function_list.push(identifier.clone());

		if let Some(error) = recoverable_error
		{
			Err(error)
		}
		else
		{
			Ok(identifier)
		}
	}

	fn use_function(&self, identifier: Identifier)
		-> Result<Identifier, Error>
	{
		if let Some(declaration_identifier) = self
			.function_list
			.iter()
			.find(|x| x.name == identifier.name)
		{
			Ok(Identifier {
				resolution_id: declaration_identifier.resolution_id,
				is_authoritative: false,
				..identifier
			})
		}
		else
		{
			Err(Error::UndefinedFunction {
				name: identifier.name.clone(),
				location: identifier.location.clone(),
			})
		}
	}

	fn declare_struct(
		&mut self,
		identifier: Identifier,
	) -> Result<Identifier, Error>
	{
		let recoverable_error = self
			.structures
			.iter()
			.map(|x| &x.identifier)
			.find(|x| x.name == identifier.name)
			.map(|previous_identifier| Error::DuplicateDeclarationStructure {
				name: identifier.name.clone(),
				location: identifier.location.clone(),
				previous: previous_identifier.location.clone(),
			});

		let identifier = Identifier {
			resolution_id: self.resolution_id,
			is_authoritative: true,
			..identifier
		};
		self.resolution_id += 1;

		self.structures.push(Structure {
			identifier: identifier.clone(),
			contained_structure_ids: Default::default(),
			depth: None,
		});

		if let Some(error) = recoverable_error
		{
			Err(error)
		}
		else
		{
			Ok(identifier)
		}
	}

	fn use_struct(&self, identifier: Identifier) -> Result<Identifier, Error>
	{
		if let Some(declaration_identifier) = self
			.structures
			.iter()
			.map(|x| &x.identifier)
			.find(|x| x.name == identifier.name)
		{
			Ok(Identifier {
				resolution_id: declaration_identifier.resolution_id,
				is_authoritative: false,
				..identifier
			})
		}
		else
		{
			Err(Error::UndefinedStructure {
				name: identifier.name.clone(),
				location: identifier.location.clone(),
			})
		}
	}

	fn found_structure_member(
		&mut self,
		name_of_structure: &Identifier,
		name_of_member: &Identifier,
		member_type: Poisonable<ValueType>,
	) -> Poisonable<ValueType>
	{
		match member_type
		{
			Ok(value_type) => match value_type
			{
				ValueType::Int8 => Ok(value_type),
				ValueType::Int16 => Ok(value_type),
				ValueType::Int32 => Ok(value_type),
				ValueType::Int64 => Ok(value_type),
				ValueType::Int128 => Ok(value_type),
				ValueType::Uint8 => Ok(value_type),
				ValueType::Uint16 => Ok(value_type),
				ValueType::Uint32 => Ok(value_type),
				ValueType::Uint64 => Ok(value_type),
				ValueType::Uint128 => Ok(value_type),
				ValueType::Usize => Ok(value_type),
				ValueType::Bool => Ok(value_type),
				ValueType::Array {
					element_type,
					length,
				} =>
				{
					let element_type = self.found_structure_member(
						name_of_structure,
						name_of_member,
						Ok(*element_type),
					)?;
					Ok(ValueType::Array {
						element_type: Box::new(element_type),
						length,
					})
				}
				ValueType::Slice { element_type } =>
				{
					let element_type = self.found_structure_member(
						name_of_structure,
						name_of_member,
						Ok(*element_type),
					)?;
					Ok(ValueType::Slice {
						element_type: Box::new(element_type),
					})
				}
				ValueType::EndlessArray { element_type } =>
				{
					let element_type = self.found_structure_member(
						name_of_structure,
						name_of_member,
						Ok(*element_type),
					)?;
					Ok(ValueType::EndlessArray {
						element_type: Box::new(element_type),
					})
				}
				ValueType::Arraylike { element_type } =>
				{
					let element_type = self.found_structure_member(
						name_of_structure,
						name_of_member,
						Ok(*element_type),
					)?;
					Ok(ValueType::Arraylike {
						element_type: Box::new(element_type),
					})
				}
				ValueType::Struct {
					identifier,
					size_in_bytes,
				} =>
				{
					let identifier = self.found_structure_member_1(
						name_of_structure,
						name_of_member,
						identifier,
					)?;
					Ok(ValueType::Struct {
						identifier,
						size_in_bytes,
					})
				}
				ValueType::Word {
					identifier,
					size_in_bytes,
				} =>
				{
					let identifier = self.found_structure_member_1(
						name_of_structure,
						name_of_member,
						identifier,
					)?;
					Ok(ValueType::Word {
						identifier,
						size_in_bytes,
					})
				}
				ValueType::UnresolvedStructOrWord {
					identifier: Some(identifier),
				} =>
				{
					let identifier = self.found_structure_member_1(
						name_of_structure,
						name_of_member,
						identifier,
					)?;
					Ok(ValueType::UnresolvedStructOrWord {
						identifier: Some(identifier),
					})
				}
				ValueType::UnresolvedStructOrWord { identifier: None } =>
				{
					unreachable!()
				}
				ValueType::Pointer { .. } => Ok(value_type),
				ValueType::View { .. } => Ok(value_type),
			},
			Err(poison) => Err(poison),
		}
	}

	fn found_structure_member_1(
		&mut self,
		name_of_container: &Identifier,
		name_of_member: &Identifier,
		name_of_containee: Identifier,
	) -> Poisonable<Identifier>
	{
		let container_id = name_of_container.resolution_id;
		let containee_id = name_of_containee.resolution_id;

		let transitive_ids = self
			.structures
			.iter()
			.find(|x| x.identifier.resolution_id == containee_id)
			.map(|x| {
				let mut ids = x.contained_structure_ids.clone();
				ids.insert(containee_id);
				ids
			})
			.unwrap_or_else(|| unreachable!());

		let mut container = self
			.structures
			.iter_mut()
			.find(|x| x.identifier.resolution_id == container_id)
			.unwrap_or_else(|| unreachable!());

		if container.contained_structure_ids.contains(&container_id)
		{
			return Err(Poison::Poisoned);
		}

		container.contained_structure_ids =
			&container.contained_structure_ids | &transitive_ids;

		if container.contained_structure_ids.contains(&container_id)
		{
			let error = Error::CyclicalStructure {
				name: name_of_container.name.clone(),
				location_of_member: name_of_member.location.clone(),
				location_of_declaration: name_of_container.location.clone(),
			};
			return Err(Poison::Error(error));
		}

		for other in &mut self.structures
		{
			if other.contained_structure_ids.contains(&container_id)
			{
				other.contained_structure_ids =
					&other.contained_structure_ids | &transitive_ids;
			}
		}

		Ok(name_of_containee)
	}

	fn determine_structure_depths(&mut self)
	{
		assert!(self.structures.len() < u32::MAX as usize);
		let len = self.structures.len() as u32;
		for depth in 0..len
		{
			let mut resolved = std::collections::HashSet::new();
			for structure in &mut self.structures
			{
				if structure.depth.is_none()
					&& structure.contained_structure_ids.is_empty()
				{
					structure.depth = Some(Ok(depth));
					resolved.insert(structure.identifier.resolution_id);
				}
			}
			if resolved.is_empty()
			{
				break;
			}
			for structure in &mut self.structures
			{
				structure.contained_structure_ids =
					&structure.contained_structure_ids - &resolved;
			}
		}
		for structure in &mut self.structures
		{
			if structure.depth.is_none()
			{
				structure.depth = Some(Err(Poison::Poisoned));
			}
		}
	}

	fn determine_structure_depth(
		&self,
		identifier: &Identifier,
	) -> Option<Poisonable<u32>>
	{
		self.structures
			.iter()
			.find(|x| x.identifier.resolution_id == identifier.resolution_id)
			.and_then(|x| x.depth.clone())
	}

	fn create_anonymous_resolution_id(&mut self) -> u32
	{
		let id = self.resolution_id;
		self.resolution_id += 1;
		id
	}

	fn push_scope(&mut self)
	{
		self.variable_stack.push(Vec::new());
	}

	fn pop_scope(&mut self)
	{
		self.variable_stack.pop();
	}
}

fn predeclare(declaration: Declaration, analyzer: &mut Analyzer)
	-> Declaration
{
	match declaration
	{
		Declaration::Constant { .. } =>
		{
			// Constants have to be declared from top to bottom, just to avoid
			// having to deal with cyclical definitions.
			declaration
		}
		Declaration::Function {
			name,
			parameters,
			body,
			return_type,
			flags,
			location_of_declaration,
			location_of_return_type,
		} => match analyzer.declare_function(name)
		{
			Ok(name) => Declaration::Function {
				name,
				parameters,
				body,
				return_type,
				flags,
				location_of_declaration,
				location_of_return_type,
			},
			Err(error) => Declaration::Poison(Poison::Error(error)),
		},
		Declaration::FunctionHead {
			name,
			parameters,
			return_type,
			flags,
			location_of_declaration,
			location_of_return_type,
		} => match analyzer.declare_function(name)
		{
			Ok(name) => Declaration::FunctionHead {
				name,
				parameters,
				return_type,
				flags,
				location_of_declaration,
				location_of_return_type,
			},
			Err(error) => Declaration::Poison(Poison::Error(error)),
		},
		Declaration::Structure {
			name,
			members,
			structural_type,
			flags,
			depth,
			location_of_declaration,
		} => match analyzer.declare_struct(name)
		{
			Ok(name) => Declaration::Structure {
				name,
				members,
				structural_type,
				flags,
				depth,
				location_of_declaration,
			},
			Err(error) => Declaration::Poison(Poison::Error(error)),
		},
		Declaration::PreprocessorDirective { .. } => unreachable!(),
		Declaration::Poison(_) => declaration,
	}
}

fn postanalyze(declaration: Declaration, analyzer: &mut Analyzer)
	-> Declaration
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
			depth: _,
			location_of_declaration,
		} =>
		{
			let depth = analyzer.determine_structure_depth(&name);
			Declaration::Structure {
				name,
				members,
				structural_type,
				flags,
				depth,
				location_of_declaration,
			}
		}
		Declaration::PreprocessorDirective { .. } => unreachable!(),
		Declaration::Poison(Poison::Error { .. }) => declaration,
		Declaration::Poison(Poison::Poisoned) => declaration,
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
				name,
				value,
				value_type,
				flags,
				location_of_declaration,
				location_of_type,
			} =>
			{
				let value = value.analyze(analyzer);
				let value_type = value_type.analyze(analyzer);
				// Declare the constant after analyzing its definition,
				// to disallow reflexive definitions.
				match analyzer.declare_constant(name)
				{
					Ok(name) => Declaration::Constant {
						name,
						value,
						value_type,
						flags,
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
				let name = analyzer.use_function(name);

				analyzer.push_scope();
				let parameters: Vec<Parameter> = parameters
					.into_iter()
					.map(|x| x.analyze(analyzer))
					.collect();
				let body = match body
				{
					Ok(body) => Ok(body.analyze(analyzer)),
					Err(poison) => Err(poison),
				};
				analyzer.pop_scope();

				match name
				{
					Ok(name) =>
					{
						let return_type = return_type.map(|x| {
							x.and_then(|x| {
								let return_type = analyze_type(x, analyzer)?;
								Ok(return_type)
							})
						});
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
				let name = analyzer.use_function(name);

				analyzer.push_scope();
				let parameters: Vec<Parameter> = parameters
					.into_iter()
					.map(|x| x.analyze(analyzer))
					.collect();
				analyzer.pop_scope();

				match name
				{
					Ok(name) =>
					{
						let return_type = return_type.map(|x| {
							x.and_then(|x| {
								let return_type = analyze_type(x, analyzer)?;
								Ok(return_type)
							})
						});
						Declaration::FunctionHead {
							name,
							parameters,
							return_type,
							flags,
							location_of_declaration,
							location_of_return_type,
						}
					}
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
				analyzer.push_scope();
				let members: Vec<Member> = members
					.into_iter()
					.map(|x| {
						x.analyze(analyzer)
							.analyze_wellfoundedness(&name, analyzer)
					})
					.collect();
				analyzer.pop_scope();

				let structural_type = structural_type.analyze(analyzer);
				Declaration::Structure {
					name,
					members,
					structural_type,
					flags,
					depth,
					location_of_declaration,
				}
			}
			Declaration::PreprocessorDirective { .. } => unreachable!(),
			Declaration::Poison(_) => self,
		}
	}
}

impl Analyzable for Member
{
	fn analyze(self, analyzer: &mut Analyzer) -> Self
	{
		let name = self.name.and_then(|name| {
			analyzer.declare_variable(name).map_err(|e| e.into())
		});
		let value_type = self.value_type.analyze(analyzer);
		Member {
			name,
			value_type,
			location_of_type: self.location_of_type,
		}
	}
}

impl Member
{
	fn analyze_wellfoundedness(
		self,
		name_of_structure: &Identifier,
		analyzer: &mut Analyzer,
	) -> Self
	{
		let value_type = if let Ok(name) = &self.name
		{
			analyzer.found_structure_member(
				name_of_structure,
				name,
				self.value_type,
			)
		}
		else
		{
			self.value_type
		};
		Member { value_type, ..self }
	}
}

impl Analyzable for Parameter
{
	fn analyze(self, analyzer: &mut Analyzer) -> Self
	{
		let name = self.name.and_then(|name| {
			analyzer.declare_variable(name).map_err(|e| e.into())
		});
		let value_type = self.value_type.analyze(analyzer);
		Parameter {
			name,
			value_type,
			location_of_type: self.location_of_type,
		}
	}
}

impl Analyzable for FunctionBody
{
	fn analyze(self, analyzer: &mut Analyzer) -> Self
	{
		analyzer.push_scope();
		let statements: Vec<Statement> = self
			.statements
			.into_iter()
			.map(|x| x.analyze(analyzer))
			.collect();
		let return_value = self.return_value.map(|v| v.analyze(analyzer));
		analyzer.pop_scope();

		// Return values need a resolution id to help with typing.
		let return_value_identifier = Identifier {
			resolution_id: analyzer.create_anonymous_resolution_id(),
			is_authoritative: false,
			..self.return_value_identifier
		};

		FunctionBody {
			statements,
			return_value,
			return_value_identifier,
		}
	}
}

impl Analyzable for Block
{
	fn analyze(self, analyzer: &mut Analyzer) -> Self
	{
		analyzer.push_scope();
		let statements: Vec<Statement> = self
			.statements
			.into_iter()
			.map(|x| x.analyze(analyzer))
			.collect();
		analyzer.pop_scope();

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
				let value_type = value_type.map(|x| x.analyze(analyzer));
				// Declare the variable after analyzing its definition,
				// to disallow reflexive definitions.
				match analyzer.declare_variable(name)
				{
					Ok(name) => Statement::Declaration {
						name,
						value,
						value_type,
						location,
					},
					Err(error) => Statement::Poison(Poison::Error(error)),
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
				Statement::Assignment {
					reference,
					value,
					location,
				}
			}
			Statement::MethodCall { name, arguments } =>
			{
				let arguments: Vec<Expression> = arguments
					.into_iter()
					.map(|a| a.analyze(analyzer))
					.collect();
				match analyzer.use_function(name)
				{
					Ok(name) => Statement::MethodCall { name, arguments },
					Err(error) => Statement::Poison(Poison::Error(error)),
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
				let then_branch = {
					let branch = then_branch.analyze(analyzer);
					Box::new(branch)
				};
				let else_branch = match else_branch
				{
					Some(else_branch) =>
					{
						let branch = else_branch.branch.analyze(analyzer);
						Some(Else {
							branch: Box::new(branch),
							location_of_else: else_branch.location_of_else,
						})
					}
					None => None,
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
		analyzer.push_scope();
		let elements: Vec<Expression> = self
			.elements
			.into_iter()
			.map(|x| x.analyze(analyzer))
			.collect();
		analyzer.pop_scope();

		// Arrays need a resolution id to help with typing its elements.
		let resolution_id = analyzer.create_anonymous_resolution_id();

		Array {
			elements,
			location: self.location,
			resolution_id,
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
				let expr = expression.analyze(analyzer);
				Expression::Unary {
					op,
					expression: Box::new(expr),
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
				let members: Vec<MemberExpression> = members
					.into_iter()
					.map(|member| {
						// TODO we could analyze name here
						let name = member.name;
						let offset = member.offset;
						let expression = member.expression.analyze(analyzer);
						MemberExpression {
							name,
							offset,
							expression,
						}
					})
					.collect();
				let structural_type = structural_type.analyze(analyzer);
				Expression::Structural {
					members,
					structural_type,
					location,
				}
			}
			Expression::Deref {
				reference,
				deref_type,
			} =>
			{
				let reference = reference.analyze(analyzer);
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
				let expression = expression.analyze(analyzer);
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
				let expression = expression.analyze(analyzer);
				Expression::PrimitiveCast {
					expression: Box::new(expression),
					coerced_type,
					location,
					location_of_type,
				}
			}
			Expression::LengthOfArray { reference } =>
			{
				let reference = reference.analyze(analyzer);
				Expression::LengthOfArray { reference }
			}
			Expression::FunctionCall {
				name,
				arguments,
				return_type,
			} =>
			{
				let arguments: Vec<Expression> = arguments
					.into_iter()
					.map(|a| a.analyze(analyzer))
					.collect();
				match analyzer.use_function(name)
				{
					Ok(name) => Expression::FunctionCall {
						name,
						arguments,
						return_type,
					},
					Err(error) => Expression::Poison(Poison::Error(error)),
				}
			}
			Expression::Poison(_) => self,
		}
	}
}

impl Analyzable for Reference
{
	fn analyze(self, analyzer: &mut Analyzer) -> Self
	{
		let base = self
			.base
			.and_then(|base| analyzer.use_variable(base).map_err(|e| e.into()));
		let steps: Vec<ReferenceStep> = self
			.steps
			.into_iter()
			.map(|step| step.analyze(analyzer))
			.collect();
		Reference {
			base,
			steps,
			address_depth: self.address_depth,
			location: self.location,
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
			ReferenceStep::Member { member, offset } =>
			{
				// We cannot determine the offset of this member until we
				// have done typing, because an expression could be multiple
				// types that have the same member name at different offsets.
				ReferenceStep::Member { member, offset }
			}
			ReferenceStep::Autodeslice { .. } => self,
			ReferenceStep::Autoderef => ReferenceStep::Autoderef,
			ReferenceStep::Autoview => ReferenceStep::Autoview,
		}
	}
}

impl Analyzable for Poisonable<ValueType>
{
	fn analyze(self, analyzer: &mut Analyzer) -> Self
	{
		self.and_then(|x| analyze_type(x, analyzer))
	}
}

fn analyze_type(
	value_type: ValueType,
	analyzer: &mut Analyzer,
) -> Poisonable<ValueType>
{
	match value_type
	{
		ValueType::UnresolvedStructOrWord {
			identifier: Some(identifier),
		} =>
		{
			let identifier = analyzer.use_struct(identifier)?;
			Ok(ValueType::UnresolvedStructOrWord {
				identifier: Some(identifier),
			})
		}
		ValueType::UnresolvedStructOrWord { identifier: None } =>
		{
			Ok(ValueType::UnresolvedStructOrWord { identifier: None })
		}
		ValueType::Struct {
			identifier,
			size_in_bytes,
		} =>
		{
			let identifier = analyzer.use_struct(identifier)?;
			Ok(ValueType::Struct {
				identifier,
				size_in_bytes,
			})
		}
		ValueType::Word {
			identifier,
			size_in_bytes,
		} =>
		{
			let identifier = analyzer.use_struct(identifier)?;
			Ok(ValueType::Word {
				identifier,
				size_in_bytes,
			})
		}
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
			let element_type = analyze_type(*element_type, analyzer)?;
			Ok(ValueType::Array {
				element_type: Box::new(element_type),
				length,
			})
		}
		ValueType::Slice { element_type } =>
		{
			let element_type = analyze_type(*element_type, analyzer)?;
			Ok(ValueType::Slice {
				element_type: Box::new(element_type),
			})
		}
		ValueType::EndlessArray { element_type } =>
		{
			let element_type = analyze_type(*element_type, analyzer)?;
			Ok(ValueType::EndlessArray {
				element_type: Box::new(element_type),
			})
		}
		ValueType::Arraylike { element_type } =>
		{
			let element_type = analyze_type(*element_type, analyzer)?;
			Ok(ValueType::Arraylike {
				element_type: Box::new(element_type),
			})
		}
		ValueType::Pointer { deref_type } =>
		{
			let deref_type = analyze_type(*deref_type, analyzer)?;
			Ok(ValueType::Pointer {
				deref_type: Box::new(deref_type),
			})
		}
		ValueType::View { deref_type } =>
		{
			let deref_type = analyze_type(*deref_type, analyzer)?;
			Ok(ValueType::View {
				deref_type: Box::new(deref_type),
			})
		}
	}
}
