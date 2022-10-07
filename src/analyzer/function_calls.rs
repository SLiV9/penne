//
// Part of penne
// Copyright (c) 2020 Sander in 't Veld
// License: MIT
//

use crate::common::*;
use crate::typer::Typed;

use anyhow::anyhow;
use anyhow::Context;

pub fn analyze(program: &Vec<Declaration>) -> Result<(), anyhow::Error>
{
	let mut analyzer = Analyzer {
		function_list: Vec::new(),
		is_const_evaluated: false,
		is_immediate_function_argument: false,
	};
	for declaration in program
	{
		declare(declaration, &mut analyzer)?;
	}
	for declaration in program
	{
		declaration.analyze(&mut analyzer)?;
	}
	Ok(())
}

struct Analyzer
{
	function_list: Vec<(Identifier, Vec<Parameter>)>,
	is_const_evaluated: bool,
	is_immediate_function_argument: bool,
}

impl Analyzer
{
	fn declare_function(
		&mut self,
		identifier: &Identifier,
		parameters: &Vec<Parameter>,
	) -> Result<(), anyhow::Error>
	{
		if let Some((previous_identifier, _)) = self
			.function_list
			.iter()
			.find(|(x, _)| x.resolution_id == identifier.resolution_id)
		{
			return Err(anyhow!(
				"previous declaration {}",
				previous_identifier.location.format()
			)
			.context(identifier.location.format())
			.context(format!(
				"a function named '{}' is already defined",
				identifier.name
			)));
		}

		self.function_list
			.push((identifier.clone(), parameters.to_vec()));
		Ok(())
	}

	fn use_function(
		&self,
		identifier: &Identifier,
		arguments: Vec<Option<Poisonable<ValueType>>>,
	) -> Result<(), anyhow::Error>
	{
		if let Some((declaration_identifier, parameters)) = self
			.function_list
			.iter()
			.find(|(x, _)| x.resolution_id == identifier.resolution_id)
		{
			if arguments.len() < parameters.len()
			{
				Err(anyhow!("too few arguments")
					.context(format!(
						"function was declared {}",
						declaration_identifier.location.format()
					))
					.context(identifier.location.format())
					.context(format!(
						"too few arguments to function '{}'",
						identifier.name
					)))
			}
			else if arguments.len() > parameters.len()
			{
				Err(anyhow!("too many arguments")
					.context(format!(
						"function was declared {}",
						declaration_identifier.location.format()
					))
					.context(identifier.location.format())
					.context(format!(
						"too many arguments to function '{}'",
						identifier.name
					)))
			}
			else
			{
				for (parameter, argument) in
					parameters.iter().zip(arguments.iter())
				{
					match (&parameter.value_type, argument)
					{
						(Ok(p), Some(Ok(a))) if p != a =>
						{
							return Err(anyhow!("got {:?}", argument)
								.context(format!(
									"expected {:?}",
									parameter.value_type
								))
								.context(format!(
									"function was declared {}",
									declaration_identifier.location.format()
								))
								.context(identifier.location.format())
								.context(format!(
									"type mismatch of parameter '{}' of function '{}'",
									parameter.name.name, identifier.name,
								)))
						}
						(_, _) => (),
					}
				}
				Ok(())
			}
		}
		else
		{
			Err(anyhow!("undefined reference")
				.context(identifier.location.format())
				.context(format!(
					"reference to undefined function named '{}'",
					identifier.name
				)))
		}
	}
}

fn declare(
	declaration: &Declaration,
	analyzer: &mut Analyzer,
) -> Result<(), anyhow::Error>
{
	match declaration
	{
		Declaration::Constant { .. } => Ok(()),
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
		Declaration::PreprocessorDirective { .. } => unreachable!(),
		Declaration::Poison(Poison::Error {
			error: _,
			partial: Some(declaration),
		}) => declare(declaration, analyzer),
		Declaration::Poison(Poison::Error {
			error: _,
			partial: None,
		}) => Ok(()),
		Declaration::Poison(Poison::Poisoned) => Ok(()),
	}
}

trait Analyzable
{
	fn analyze(&self, analyzer: &mut Analyzer) -> Result<(), anyhow::Error>;
}

impl Analyzable for Declaration
{
	fn analyze(&self, analyzer: &mut Analyzer) -> Result<(), anyhow::Error>
	{
		analyzer.is_immediate_function_argument = false;
		match self
		{
			Declaration::Constant {
				name,
				value,
				value_type: _,
				flags: _,
			} =>
			{
				analyzer.is_const_evaluated = true;
				value
					.analyze(analyzer)
					.with_context(|| name.location.format())?;
				analyzer.is_const_evaluated = false;
				Ok(())
			}
			Declaration::Function {
				name: _,
				parameters,
				body,
				return_type: _,
				flags: _,
			} =>
			{
				for parameter in parameters
				{
					parameter.analyze(analyzer)?;
				}
				match body
				{
					Ok(body) => body.analyze(analyzer)?,
					Err(_poison) => (),
				}
				Ok(())
			}
			Declaration::FunctionHead {
				name: _,
				parameters,
				return_type: _,
				flags: _,
			} =>
			{
				for parameter in parameters
				{
					parameter.analyze(analyzer)?;
				}
				Ok(())
			}
			Declaration::PreprocessorDirective { .. } => unreachable!(),
			Declaration::Poison(Poison::Error {
				error: _,
				partial: Some(declaration),
			}) => declaration.analyze(analyzer),
			Declaration::Poison(Poison::Error {
				error: _,
				partial: None,
			}) => Ok(()),
			Declaration::Poison(Poison::Poisoned) => Ok(()),
		}
	}
}

impl Analyzable for Parameter
{
	fn analyze(&self, analyzer: &mut Analyzer) -> Result<(), anyhow::Error>
	{
		analyzer.is_immediate_function_argument = false;
		match &self.value_type
		{
			Ok(ValueType::Array { .. }) =>
			{
				Err(anyhow!("non-slice array parameter")
					.context(self.name.location.format())
					.context(format!(
						"parameter '{}' is not a slice",
						self.name.name
					)))
			}
			_ => Ok(()),
		}
	}
}

impl Analyzable for FunctionBody
{
	fn analyze(&self, analyzer: &mut Analyzer) -> Result<(), anyhow::Error>
	{
		analyzer.is_immediate_function_argument = false;
		for statement in &self.statements
		{
			statement.analyze(analyzer)?;
		}
		if let Some(return_value) = &self.return_value
		{
			return_value.analyze(analyzer)?;
		}
		Ok(())
	}
}

impl Analyzable for Block
{
	fn analyze(&self, analyzer: &mut Analyzer) -> Result<(), anyhow::Error>
	{
		analyzer.is_immediate_function_argument = false;
		for statement in &self.statements
		{
			statement.analyze(analyzer)?;
		}
		Ok(())
	}
}

impl Analyzable for Statement
{
	fn analyze(&self, analyzer: &mut Analyzer) -> Result<(), anyhow::Error>
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
				match value_type
				{
					Some(Ok(ValueType::Array { .. })) => (),
					Some(Ok(ValueType::Slice { .. })) =>
					{
						return Err(anyhow!("slice variable")
							.context(location.format())
							.context(format!(
								"variable '{}' may not be a slice",
								name.name
							)));
					}
					Some(Ok(ValueType::ExtArray { .. })) =>
					{
						return Err(anyhow!("extarray variable")
							.context(location.format())
							.context(format!(
								"variable '{}' may not be an external array",
								name.name
							)));
					}
					_ => (),
				}
				if let Some(value) = value
				{
					value
						.analyze(analyzer)
						.with_context(|| location.format())?;
				}
				Ok(())
			}
			Statement::Assignment {
				reference,
				value,
				location,
			} =>
			{
				reference
					.analyze(analyzer)
					.with_context(|| location.format())?;
				value.analyze(analyzer).with_context(|| location.format())?;
				Ok(())
			}
			Statement::MethodCall { name, arguments } =>
			{
				if analyzer.is_const_evaluated
				{
					let error = anyhow!("const evaluation")
						.context(name.location.format())
						.context("cannot call methods in const context");
					return Err(error);
				}

				let argument_types =
					arguments.iter().map(|x| x.value_type()).collect();
				analyzer.use_function(name, argument_types)?;
				for argument in arguments
				{
					analyzer.is_immediate_function_argument = true;
					argument.analyze(analyzer)?;
				}
				analyzer.is_immediate_function_argument = false;

				Ok(())
			}
			Statement::Loop { location: _ } => Ok(()),
			Statement::Goto {
				label: _,
				location: _,
			} => Ok(()),
			Statement::Label {
				label: _,
				location: _,
			} => Ok(()),
			Statement::If {
				condition,
				then_branch,
				else_branch,
				location,
			} =>
			{
				condition
					.analyze(analyzer)
					.with_context(|| location.format())?;
				then_branch.analyze(analyzer)?;
				if let Some(else_branch) = else_branch
				{
					else_branch.branch.analyze(analyzer)?;
				}
				Ok(())
			}
			Statement::Block(block) => block.analyze(analyzer),
			Statement::Poison(Poison::Error {
				error: _,
				partial: Some(statement),
			}) => statement.analyze(analyzer),
			Statement::Poison(Poison::Error {
				error: _,
				partial: None,
			}) => Ok(()),
			Statement::Poison(Poison::Poisoned) => Ok(()),
		}
	}
}

impl Analyzable for Comparison
{
	fn analyze(&self, analyzer: &mut Analyzer) -> Result<(), anyhow::Error>
	{
		analyzer.is_immediate_function_argument = false;
		self.left
			.analyze(analyzer)
			.with_context(|| self.location.format())?;
		self.right
			.analyze(analyzer)
			.with_context(|| self.location.format())?;
		Ok(())
	}
}

impl Analyzable for Array
{
	fn analyze(&self, analyzer: &mut Analyzer) -> Result<(), anyhow::Error>
	{
		analyzer.is_immediate_function_argument = false;
		for element in &self.elements
		{
			element.analyze(analyzer)?;
		}
		Ok(())
	}
}

impl Analyzable for Expression
{
	fn analyze(&self, analyzer: &mut Analyzer) -> Result<(), anyhow::Error>
	{
		match self
		{
			Expression::Binary {
				op: _,
				left,
				right,
				location,
				location_of_op: _,
			} =>
			{
				analyzer.is_immediate_function_argument = false;
				left.analyze(analyzer).with_context(|| location.format())?;
				right.analyze(analyzer).with_context(|| location.format())?;
				Ok(())
			}
			Expression::Unary {
				op: _,
				expression,
				location,
				location_of_op: _,
			} =>
			{
				analyzer.is_immediate_function_argument = false;
				expression
					.analyze(analyzer)
					.with_context(|| location.format())?;
				Ok(())
			}
			Expression::PrimitiveLiteral { .. } => Ok(()),
			Expression::NakedIntegerLiteral { .. } => Ok(()),
			Expression::BitIntegerLiteral { .. } => Ok(()),
			Expression::ArrayLiteral {
				array,
				element_type: _,
			} =>
			{
				analyzer.is_immediate_function_argument = false;
				array.analyze(analyzer)
			}
			Expression::StringLiteral { .. } => Ok(()),
			Expression::Deref {
				reference,
				deref_type,
			} =>
			{
				match deref_type
				{
					Some(Ok(ValueType::Array { .. })) =>
					{
						if !analyzer.is_immediate_function_argument
						{
							let error = anyhow!("cannot move from array")
								.context(reference.location.format())
								.context("this variable cannot be moved from");
							return Err(error);
						}
					}
					Some(Ok(ValueType::Slice { .. }))
					| Some(Ok(ValueType::ExtArray { .. })) =>
					{
						if !analyzer.is_immediate_function_argument
						{
							let error = anyhow!("cannot move from slice")
								.context(reference.location.format())
								.context("this variable cannot be moved from");
							return Err(error);
						}
					}
					_ => (),
				}
				reference.analyze(analyzer)
			}
			Expression::Autocoerce {
				expression,
				coerced_type: _,
			} => expression.analyze(analyzer),
			Expression::PrimitiveCast {
				expression,
				coerced_type: _,
				location: _,
				location_of_type: _,
			} => expression.analyze(analyzer),
			Expression::LengthOfArray { reference } =>
			{
				reference.analyze(analyzer)
			}
			Expression::FunctionCall {
				name,
				arguments,
				return_type: _,
			} =>
			{
				if analyzer.is_const_evaluated
				{
					let error = anyhow!("const evaluation")
						.context(name.location.format())
						.context("cannot call functions in const context");
					return Err(error);
				}

				let argument_types =
					arguments.iter().map(|x| x.value_type()).collect();
				analyzer.use_function(name, argument_types)?;
				for argument in arguments
				{
					analyzer.is_immediate_function_argument = true;
					argument.analyze(analyzer)?;
				}
				analyzer.is_immediate_function_argument = false;

				Ok(())
			}
			Expression::Poison(_) => Ok(()),
		}
	}
}

impl Analyzable for Reference
{
	fn analyze(&self, analyzer: &mut Analyzer) -> Result<(), anyhow::Error>
	{
		for step in self.steps.iter()
		{
			match step
			{
				ReferenceStep::Element { argument } =>
				{
					let argument_type = argument.value_type();
					match argument_type
					{
						Some(Ok(ValueType::Usize)) => (),
						Some(Ok(other_type)) =>
						{
							return Err(anyhow!("got {:?}", other_type)
								.context(format!("expected usize"))
								.context(self.location.format())
								.context(format!(
									"type mismatch of index into array '{}'",
									self.base.name,
								)));
						}
						Some(Err(_poison)) =>
						{
							// The analyze below will raise the error if needed.
						}
						None =>
						{
							return Err(anyhow!("failed to determine type")
								.context(format!("expected usize"))
								.context(self.location.format())
								.context(format!(
									"type mismatch of index into array '{}'",
									self.base.name,
								)));
						}
					}

					analyzer.is_immediate_function_argument = false;
					argument.analyze(analyzer)?;
				}
				ReferenceStep::Member { .. } => (),
				ReferenceStep::Autodeslice { .. } => (),
				ReferenceStep::Autoderef => (),
				ReferenceStep::Autoview => (),
			}
		}
		Ok(())
	}
}
