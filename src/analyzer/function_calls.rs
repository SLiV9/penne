//
// Part of penne
// Copyright (c) 2020 Sander in 't Veld
// License: MIT
//

use crate::common::*;
use crate::error::Error;
use crate::typer::Typed;

pub fn analyze(program: Vec<Declaration>) -> Vec<Declaration>
{
	let mut analyzer = Analyzer {
		functions: std::collections::HashMap::new(),
		is_const_evaluated: false,
		is_immediate_function_argument: false,
	};
	for declaration in &program
	{
		declare(&declaration, &mut analyzer);
	}
	program
		.into_iter()
		.map(|x| x.analyze(&mut analyzer))
		.collect()
}

struct Analyzer
{
	functions: std::collections::HashMap<u32, Function>,
	is_const_evaluated: bool,
	is_immediate_function_argument: bool,
}

struct Function
{
	identifier: Identifier,
	parameters: Vec<Parameter>,
}

impl Analyzer
{
	fn declare_function(
		&mut self,
		identifier: &Identifier,
		parameters: &Vec<Parameter>,
	)
	{
		let function = Function {
			identifier: identifier.clone(),
			parameters: parameters.to_vec(),
		};
		self.functions.insert(identifier.resolution_id, function);
	}

	fn use_function(
		&self,
		identifier: &Identifier,
		arguments: &[Expression],
	) -> Result<(), Error>
	{
		let declared = match self.functions.get(&identifier.resolution_id)
		{
			Some(function) => function,
			None => unreachable!(),
		};
		let Function {
			identifier: declaration_identifier,
			parameters,
		} = declared;
		if arguments.len() < parameters.len()
		{
			Err(Error::TooFewArguments {
				location: identifier.location.clone(),
				location_of_declaration: declaration_identifier
					.location
					.clone(),
			})
		}
		else if arguments.len() > parameters.len()
		{
			Err(Error::TooManyArguments {
				location: identifier.location.clone(),
				location_of_declaration: declaration_identifier
					.location
					.clone(),
			})
		}
		else
		{
			for (parameter, argument) in parameters.iter().zip(arguments.iter())
			{
				match (&parameter.value_type, argument.value_type())
				{
					(Ok(p), Some(Ok(a))) if p != &a => match &parameter.name
					{
						Ok(declared) =>
						{
							let parameter_name = declared.name.clone();
							let location_of_declaration =
								declared.location.clone();
							if can_hint_missing_address(&argument, &a, p)
							{
								return Err(Error::ArgumentMissingAddress {
									parameter_name,
									argument_type: a,
									parameter_type: p.clone(),
									location: argument.location().clone(),
									location_of_declaration,
								});
							}
							return Err(Error::ArgumentTypeMismatch {
								parameter_name,
								argument_type: a,
								parameter_type: p.clone(),
								location: argument.location().clone(),
								location_of_declaration,
							});
						}
						Err(_poison) => (),
					},
					(_, _) => (),
				}
			}
			Ok(())
		}
	}
}

fn can_hint_missing_address(
	argument: &Expression,
	argument_type: &ValueType,
	parameter_type: &ValueType,
) -> bool
{
	match parameter_type
	{
		ValueType::Pointer { deref_type }
			if deref_type.as_ref() == argument_type =>
		{
			match argument
			{
				Expression::Deref {
					reference: _,
					deref_type: _,
				} => true,
				_ => false,
			}
		}
		_ => false,
	}
}

fn declare(declaration: &Declaration, analyzer: &mut Analyzer)
{
	match declaration
	{
		Declaration::Constant { .. } => (),
		Declaration::Function {
			name,
			parameters,
			body: _,
			return_type: _,
			flags: _,
		} => analyzer.declare_function(name, parameters),
		Declaration::FunctionHead {
			name,
			parameters,
			return_type: _,
			flags: _,
		} => analyzer.declare_function(name, parameters),
		Declaration::Structure { .. } => (),
		Declaration::PreprocessorDirective { .. } => unreachable!(),
		Declaration::Poison(Poison::Error {
			error: _,
			partial: Some(declaration),
		}) => declare(declaration, analyzer),
		Declaration::Poison(Poison::Error {
			error: _,
			partial: None,
		}) => (),
		Declaration::Poison(Poison::Poisoned) => (),
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
		analyzer.is_immediate_function_argument = false;
		match self
		{
			Declaration::Constant {
				name,
				value,
				value_type,
				flags,
			} =>
			{
				analyzer.is_const_evaluated = true;
				let value = value.analyze(analyzer);
				analyzer.is_const_evaluated = false;

				let value_type = match value_type
				{
					Ok(vt) if !vt.can_be_constant() => Err(Poison::Error {
						error: Error::IllegalConstantType {
							value_type: vt.clone(),
							location: name.location.clone(),
						},
						partial: Some(vt),
					}),
					_ => value_type,
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
				body,
				return_type,
				flags,
			} =>
			{
				let parameters = parameters
					.into_iter()
					.map(|x| x.analyze(analyzer))
					.collect();
				let body = match body
				{
					Ok(body) => Ok(body.analyze(analyzer)),
					Err(poison) => Err(poison),
				};
				Declaration::Function {
					name,
					parameters,
					body,
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
				let parameters = parameters
					.into_iter()
					.map(|x| x.analyze(analyzer))
					.collect();
				Declaration::FunctionHead {
					name,
					parameters,
					return_type,
					flags,
				}
			}
			Declaration::Structure {
				name: _,
				members: _,
				structural_type: _,
				flags: _,
				depth: _,
			} => self,
			Declaration::PreprocessorDirective { .. } => unreachable!(),
			Declaration::Poison(_) => self,
		}
	}
}

impl Analyzable for Parameter
{
	fn analyze(self, analyzer: &mut Analyzer) -> Self
	{
		analyzer.is_immediate_function_argument = false;
		let value_type = match &self.value_type
		{
			Ok(vt) if !vt.can_be_parameter() => match &self.name
			{
				Ok(name) => Err(Poison::Error {
					error: Error::IllegalParameterType {
						value_type: vt.clone(),
						location: name.location.clone(),
					},
					partial: Some(vt.clone()),
				}),
				Err(_poison) => self.value_type,
			},
			_ => self.value_type,
		};
		Parameter {
			name: self.name,
			value_type,
		}
	}
}

impl Analyzable for FunctionBody
{
	fn analyze(self, analyzer: &mut Analyzer) -> Self
	{
		analyzer.is_immediate_function_argument = false;
		let statements = self
			.statements
			.into_iter()
			.map(|x| x.analyze(analyzer))
			.collect();
		let return_value = self.return_value.map(|x| x.analyze(analyzer));
		FunctionBody {
			statements,
			return_value,
			return_value_identifier: self.return_value_identifier,
		}
	}
}

impl Analyzable for Block
{
	fn analyze(self, analyzer: &mut Analyzer) -> Self
	{
		analyzer.is_immediate_function_argument = false;
		let statements = self
			.statements
			.into_iter()
			.map(|x| x.analyze(analyzer))
			.collect();
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
		analyzer.is_immediate_function_argument = false;
		match self
		{
			Statement::Declaration {
				name,
				value,
				value_type,
				location,
			} =>
			{
				let value_type = match value_type
				{
					Some(Ok(vt)) if !vt.can_be_variable() =>
					{
						Some(Err(Error::IllegalVariableType {
							value_type: vt,
							location: name.location.clone(),
						}
						.into()))
					}
					_ => value_type,
				};
				let value = value.map(|x| x.analyze(analyzer));
				Statement::Declaration {
					name,
					value,
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
				let reference = reference.analyze(analyzer);
				let value = value.analyze(analyzer);
				Statement::Assignment {
					reference,
					value,
					location,
				}
			}
			Statement::MethodCall { name, arguments } =>
			{
				if analyzer.is_const_evaluated
				{
					return Statement::Poison(Poison::Error {
						error: Error::FunctionInConstContext {
							location: name.location.clone(),
						},
						partial: Some(Box::new(Statement::MethodCall {
							name,
							arguments,
						})),
					});
				}

				let recoverable_error =
					analyzer.use_function(&name, &arguments);
				let arguments = arguments
					.into_iter()
					.map(|argument| {
						analyzer.is_immediate_function_argument = true;
						argument.analyze(analyzer)
					})
					.collect();
				analyzer.is_immediate_function_argument = false;

				match recoverable_error
				{
					Ok(()) => Statement::MethodCall { name, arguments },
					Err(error) => Statement::Poison(Poison::Error {
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
				let then_branch = Box::new(then_branch.analyze(analyzer));
				let else_branch = else_branch.map(|x| {
					let branch = x.branch.analyze(analyzer);
					Else {
						branch: Box::new(branch),
						location_of_else: x.location_of_else,
					}
				});
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
		analyzer.is_immediate_function_argument = false;
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
		analyzer.is_immediate_function_argument = false;
		let elements = self
			.elements
			.into_iter()
			.map(|x| x.analyze(analyzer))
			.collect();
		Array {
			elements,
			location: self.location,
			resolution_id: self.resolution_id,
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
				if analyzer.is_const_evaluated
				{
					return Expression::Poison(Poison::Error {
						error: Error::UnsupportedInConstContext {
							location: location.clone(),
						},
						partial: Some(Box::new(Expression::Binary {
							op,
							left,
							right,
							location,
							location_of_op,
						})),
					});
				}

				analyzer.is_immediate_function_argument = false;
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
				if analyzer.is_const_evaluated
				{
					return Expression::Poison(Poison::Error {
						error: Error::UnsupportedInConstContext {
							location: location.clone(),
						},
						partial: Some(Box::new(Expression::Unary {
							op,
							expression,
							location,
							location_of_op,
						})),
					});
				}

				analyzer.is_immediate_function_argument = false;
				let expression = expression.analyze(analyzer);
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
				analyzer.is_immediate_function_argument = false;
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
				let members = members
					.into_iter()
					.map(|member| {
						let expression = member.expression.analyze(analyzer);
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
			Expression::Deref {
				reference,
				deref_type,
			} =>
			{
				if analyzer.is_const_evaluated
				{
					return Expression::Poison(Poison::Error {
						error: Error::UnsupportedInConstContext {
							location: reference.location.clone(),
						},
						partial: Some(Box::new(Expression::Deref {
							reference,
							deref_type,
						})),
					});
				}

				let deref_type = match deref_type
				{
					Some(Ok(vt @ ValueType::Array { .. }))
					| Some(Ok(vt @ ValueType::EndlessArray { .. })) =>
					{
						if !analyzer.is_immediate_function_argument
						{
							Some(Err(Poison::Error {
								error: Error::CannotCopyArray {
									location: reference.location.clone(),
								},
								partial: Some(vt),
							}))
						}
						else
						{
							Some(Ok(vt))
						}
					}
					Some(Ok(vt @ ValueType::Slice { .. }))
					| Some(Ok(vt @ ValueType::Arraylike { .. })) =>
					{
						if !analyzer.is_immediate_function_argument
						{
							Some(Err(Poison::Error {
								error: Error::CannotCopySlice {
									location: reference.location.clone(),
								},
								partial: Some(vt),
							}))
						}
						else
						{
							Some(Ok(vt))
						}
					}
					Some(Ok(vt @ ValueType::Struct { .. })) =>
					{
						if !analyzer.is_immediate_function_argument
						{
							Some(Err(Poison::Error {
								error: Error::CannotCopyStruct {
									location: reference.location.clone(),
								},
								partial: Some(vt),
							}))
						}
						else
						{
							Some(Ok(vt))
						}
					}
					_ => deref_type,
				};
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
				if analyzer.is_const_evaluated
				{
					return Expression::Poison(Poison::Error {
						error: Error::UnsupportedInConstContext {
							location: reference.location.clone(),
						},
						partial: Some(Box::new(Expression::LengthOfArray {
							reference,
						})),
					});
				}

				let reference = reference.analyze(analyzer);
				Expression::LengthOfArray { reference }
			}
			Expression::FunctionCall {
				name,
				arguments,
				return_type,
			} =>
			{
				if analyzer.is_const_evaluated
				{
					return Expression::Poison(Poison::Error {
						error: Error::FunctionInConstContext {
							location: name.location.clone(),
						},
						partial: Some(Box::new(Expression::FunctionCall {
							name,
							arguments,
							return_type,
						})),
					});
				}

				let recoverable_error =
					analyzer.use_function(&name, &arguments);
				let arguments = arguments
					.into_iter()
					.map(|argument| {
						analyzer.is_immediate_function_argument = true;
						argument.analyze(analyzer)
					})
					.collect();
				analyzer.is_immediate_function_argument = false;

				match recoverable_error
				{
					Ok(()) => Expression::FunctionCall {
						name,
						arguments,
						return_type,
					},
					Err(error) => Expression::Poison(Poison::Error {
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
				analyzer.is_immediate_function_argument = false;
				let argument = argument.analyze(analyzer);

				let argument = match argument.value_type()
				{
					Some(Ok(ValueType::Usize)) => argument,
					Some(Ok(other_type)) => Expression::Poison(Poison::Error {
						error: Error::IndexTypeMismatch {
							argument_type: other_type,
							index_type: ValueType::Usize,
							location: argument.location().clone(),
						},
						partial: Some(Box::new(argument)),
					}),
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
	fn analyze(self, analyzer: &mut Analyzer) -> Self
	{
		let steps = self
			.steps
			.into_iter()
			.map(|x| x.analyze(analyzer))
			.collect();
		Reference {
			base: self.base,
			steps,
			address_depth: self.address_depth,
			location: self.location,
		}
	}
}
