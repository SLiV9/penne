//
// Part of penne
// Copyright (c) 2020 Sander in 't Veld
// License: MIT
//

use crate::common::*;
use crate::error::Error;
use crate::error::{Partiable, Partial};

pub fn analyze(program: Vec<Declaration>) -> Vec<Declaration>
{
	let mut analyzer = Analyzer {
		variable_stack: Vec::new(),
		function_list: Vec::new(),
		struct_list: Vec::new(),
		resolution_id: 1,
	};
	// Predeclare all functions so that they can reference each other.
	let declarations: Vec<Declaration> = program
		.into_iter()
		.map(|x| predeclare(x, &mut analyzer))
		.collect();
	// After collecting all declarations, analyze the function bodies.
	declarations
		.into_iter()
		.map(|x| x.analyze(&mut analyzer))
		.collect()
}

struct Analyzer
{
	variable_stack: Vec<Vec<Identifier>>,
	function_list: Vec<Identifier>,
	struct_list: Vec<Identifier>,
	resolution_id: u32,
}

impl Analyzer
{
	fn declare_constant(
		&mut self,
		identifier: Identifier,
	) -> Partiable<Identifier>
	{
		match self.declare_variable(identifier)
		{
			Ok(identifier) => Ok(identifier),
			Err(Partial { error, partial }) =>
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
				Err(Partial { error, partial })
			}
		}
	}

	fn declare_variable(
		&mut self,
		identifier: Identifier,
	) -> Partiable<Identifier>
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
			Err(Partial {
				error,
				partial: identifier,
			})
		}
		else
		{
			Ok(identifier)
		}
	}

	fn use_variable(&self, identifier: Identifier) -> Partiable<Identifier>
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

		Err(Partial {
			error: Error::UndefinedVariable {
				name: identifier.name.clone(),
				location: identifier.location.clone(),
			},
			partial: identifier,
		})
	}

	fn declare_function(
		&mut self,
		identifier: Identifier,
	) -> Partiable<Identifier>
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
			Err(Partial {
				error,
				partial: identifier,
			})
		}
		else
		{
			Ok(identifier)
		}
	}

	fn use_function(&self, identifier: Identifier) -> Partiable<Identifier>
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
			Err(Partial {
				error: Error::UndefinedFunction {
					name: identifier.name.clone(),
					location: identifier.location.clone(),
				},
				partial: identifier,
			})
		}
	}

	fn declare_struct(
		&mut self,
		identifier: Identifier,
	) -> Partiable<Identifier>
	{
		let recoverable_error = self
			.struct_list
			.iter()
			.find(|x| x.name == identifier.name)
			.map(|previous_identifier| Error::DuplicateDeclarationStruct {
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

		self.struct_list.push(identifier.clone());

		if let Some(error) = recoverable_error
		{
			Err(Partial {
				error,
				partial: identifier,
			})
		}
		else
		{
			Ok(identifier)
		}
	}

	fn use_struct(&self, identifier: Identifier) -> Partiable<Identifier>
	{
		if let Some(declaration_identifier) =
			self.struct_list.iter().find(|x| x.name == identifier.name)
		{
			Ok(Identifier {
				resolution_id: declaration_identifier.resolution_id,
				is_authoritative: false,
				..identifier
			})
		}
		else
		{
			Err(Partial {
				error: Error::UndefinedStruct {
					name: identifier.name.clone(),
					location: identifier.location.clone(),
				},
				partial: identifier,
			})
		}
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
		} => match analyzer.declare_function(name.clone())
		{
			Ok(name) => Declaration::Function {
				name,
				parameters,
				body,
				return_type,
				flags,
			},
			Err(Partial {
				error,
				partial: name,
			}) => Declaration::Poison(Poison::Error {
				error,
				partial: Some(Box::new(Declaration::Function {
					name,
					parameters,
					body,
					return_type,
					flags,
				})),
			}),
		},
		Declaration::FunctionHead {
			name,
			parameters,
			return_type,
			flags,
		} => match analyzer.declare_function(name.clone())
		{
			Ok(name) => Declaration::FunctionHead {
				name,
				parameters,
				return_type,
				flags,
			},
			Err(Partial {
				error,
				partial: name,
			}) => Declaration::Poison(Poison::Error {
				error,
				partial: Some(Box::new(Declaration::FunctionHead {
					name,
					parameters,
					return_type,
					flags,
				})),
			}),
		},
		Declaration::PreprocessorDirective { .. } => unreachable!(),
		Declaration::Poison(Poison::Error {
			error,
			partial: Some(declaration),
		}) =>
		{
			let declaration = predeclare(*declaration, analyzer);
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
					},
					Err(Partial {
						error,
						partial: name,
					}) => Declaration::Poison(Poison::Error {
						error,
						partial: Some(Box::new(Declaration::Constant {
							name,
							value,
							value_type,
							flags,
						})),
					}),
				}
			}
			Declaration::Function {
				name,
				parameters,
				body,
				return_type,
				flags,
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
						let return_type =
							return_type.map(|x| analyze_type(x, analyzer));
						match return_type.transpose()
						{
							Ok(return_type) => Declaration::Function {
								name,
								parameters,
								body,
								return_type,
								flags,
							},
							Err(Partial {
								error,
								partial: return_type,
							}) => Declaration::Poison(Poison::Error {
								error,
								partial: Some(Box::new(
									Declaration::Function {
										name,
										parameters,
										body,
										return_type: Some(return_type),
										flags,
									},
								)),
							}),
						}
					}
					Err(Partial {
						error,
						partial: name,
					}) => Declaration::Poison(Poison::Error {
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
						let return_type =
							return_type.map(|x| analyze_type(x, analyzer));
						match return_type.transpose()
						{
							Ok(return_type) => Declaration::FunctionHead {
								name,
								parameters,
								return_type,
								flags,
							},
							Err(Partial {
								error,
								partial: return_type,
							}) => Declaration::Poison(Poison::Error {
								error,
								partial: Some(Box::new(
									Declaration::FunctionHead {
										name,
										parameters,
										return_type: Some(return_type),
										flags,
									},
								)),
							}),
						}
					}
					Err(Partial {
						error,
						partial: name,
					}) => Declaration::Poison(Poison::Error {
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
				let declaration = declaration.analyze(analyzer);
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

impl Analyzable for Parameter
{
	fn analyze(self, analyzer: &mut Analyzer) -> Self
	{
		let name = self.name.and_then(|name| {
			analyzer.declare_variable(name).map_err(|e| e.into())
		});
		let value_type = self.value_type.analyze(analyzer);
		Parameter { name, value_type }
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
					Err(Partial {
						error,
						partial: name,
					}) => Statement::Poison(Poison::Error {
						error,
						partial: Some(Box::new(Statement::Declaration {
							name,
							value,
							value_type,
							location,
						})),
					}),
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
					Err(Partial {
						error,
						partial: name,
					}) => Statement::Poison(Poison::Error {
						error,
						partial: Some(Box::new(Statement::MethodCall {
							name,
							arguments,
						})),
					}),
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
			Statement::Poison(Poison::Error {
				error,
				partial: Some(statement),
			}) =>
			{
				let statement = statement.analyze(analyzer);
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
					Err(Partial {
						error,
						partial: name,
					}) => Expression::Poison(Poison::Error {
						error,
						partial: Some(Box::new(Expression::FunctionCall {
							name,
							arguments,
							return_type,
						})),
					}),
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
			ReferenceStep::Member { member } =>
			{
				// TODO determine offset of member in struct
				let offset = 0;
				let member = Identifier {
					resolution_id: offset,
					is_authoritative: false,
					..member
				};
				ReferenceStep::Member { member }
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
		self.and_then(|x| analyze_type(x, analyzer).map_err(|e| e.into()))
	}
}

fn analyze_type(
	value_type: ValueType,
	analyzer: &mut Analyzer,
) -> Partiable<ValueType>
{
	match value_type
	{
		ValueType::UnresolvedStructOrWord {
			identifier: Some(identifier),
		} =>
		{
			let identifier = analyzer.use_struct(identifier);
			apply_regardless(identifier, |identifier| {
				ValueType::UnresolvedStructOrWord {
					identifier: Some(identifier),
				}
			})
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
		} => apply_regardless(
			analyze_type(*element_type, analyzer),
			|element_type| ValueType::Array {
				element_type: Box::new(element_type),
				length,
			},
		),
		ValueType::Slice { element_type } => apply_regardless(
			analyze_type(*element_type, analyzer),
			|element_type| ValueType::Slice {
				element_type: Box::new(element_type),
			},
		),
		ValueType::EndlessArray { element_type } => apply_regardless(
			analyze_type(*element_type, analyzer),
			|element_type| ValueType::EndlessArray {
				element_type: Box::new(element_type),
			},
		),
		ValueType::Arraylike { element_type } => apply_regardless(
			analyze_type(*element_type, analyzer),
			|element_type| ValueType::Arraylike {
				element_type: Box::new(element_type),
			},
		),
		ValueType::Pointer { deref_type } => apply_regardless(
			analyze_type(*deref_type, analyzer),
			|deref_type| ValueType::Pointer {
				deref_type: Box::new(deref_type),
			},
		),
		ValueType::View { deref_type } => apply_regardless(
			analyze_type(*deref_type, analyzer),
			|deref_type| ValueType::View {
				deref_type: Box::new(deref_type),
			},
		),
	}
}

fn apply_regardless<T, U, F>(partiable: Partiable<T>, op: F) -> Partiable<U>
where
	F: FnOnce(T) -> U,
{
	match partiable
	{
		Ok(x) => Ok(op(x)),
		Err(Partial { error, partial }) => Err(Partial {
			error,
			partial: op(partial),
		}),
	}
}
