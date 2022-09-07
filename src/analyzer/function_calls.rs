/**/

use crate::common::*;
use crate::typer::Typed;

use anyhow::anyhow;
use anyhow::Context;

pub fn analyze(program: &Vec<Declaration>) -> Result<(), anyhow::Error>
{
	let mut analyzer = Analyzer {
		function_list: Vec::new(),
		array_list: Vec::new(),
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
	array_list: Vec<Identifier>,
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
		arguments: Vec<Option<ValueType>>,
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
			else if let Some((parameter, argument)) =
				parameters.iter().zip(arguments.iter()).find(|(p, a)| {
					!does_argument_match_parameter(*a, &p.value_type)
				})
			{
				Err(anyhow!("got {:?}", argument)
					.context(format!("expected {:?}", parameter.value_type))
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
			else
			{
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

	fn declare_array(
		&mut self,
		identifier: &Identifier,
	) -> Result<(), anyhow::Error>
	{
		// Array names have been resolved during scoping.
		self.array_list.push(identifier.clone());
		Ok(())
	}

	fn use_array(
		&self,
		identifier: &Identifier,
		argument_type: Option<ValueType>,
	) -> Result<(), anyhow::Error>
	{
		if let Some(declaration_identifier) = self
			.array_list
			.iter()
			.find(|x| x.resolution_id == identifier.resolution_id)
		{
			match argument_type
			{
				Some(ValueType::Usize) => Ok(()),
				Some(other_type) => Err(anyhow!("got {:?}", other_type)
					.context(format!("expected usize"))
					.context(format!(
						"array was declared {}",
						declaration_identifier.location.format()
					))
					.context(identifier.location.format())
					.context(format!(
						"type mismatch of index into array '{}'",
						identifier.name,
					))),
				None => Err(anyhow!("failed to determine type")
					.context(format!("expected usize"))
					.context(format!(
						"array was declared {}",
						declaration_identifier.location.format()
					))
					.context(identifier.location.format())
					.context(format!(
						"type mismatch of index into array '{}'",
						identifier.name,
					))),
			}
		}
		else
		{
			Err(anyhow!("undefined reference")
				.context(identifier.location.format())
				.context(format!(
					"reference to undefined array named '{}'",
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
				body.analyze(analyzer)?;
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
		}
	}
}

impl Analyzable for Parameter
{
	fn analyze(&self, analyzer: &mut Analyzer) -> Result<(), anyhow::Error>
	{
		analyzer.is_immediate_function_argument = false;
		match self.value_type
		{
			Some(ValueType::Array { .. }) =>
			{
				Err(anyhow!("non-slice array parameter")
					.context(self.name.location.format())
					.context(format!(
						"parameter '{}' is not a slice",
						self.name.name
					)))
			}
			Some(ValueType::Slice { .. }) => analyzer.declare_array(&self.name),
			Some(ValueType::ExtArray { .. }) =>
			{
				analyzer.declare_array(&self.name)
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
				value: Some(value),
				value_type,
				location,
			} =>
			{
				match value_type
				{
					Some(ValueType::Array { .. }) =>
					{
						analyzer.declare_array(name)?;
					}
					Some(ValueType::Slice { .. }) =>
					{
						return Err(anyhow!("slice variable")
							.context(location.format())
							.context(format!(
								"variable '{}' may not be a slice",
								name.name
							)));
					}
					Some(ValueType::ExtArray { .. }) =>
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
				value.analyze(analyzer).with_context(|| location.format())?;
				Ok(())
			}
			Statement::Declaration {
				name,
				value: None,
				value_type,
				location,
			} =>
			{
				match value_type
				{
					Some(ValueType::Array { .. }) =>
					{
						analyzer.declare_array(name)?;
					}
					Some(ValueType::Slice { .. }) =>
					{
						return Err(anyhow!("slice variable")
							.context(location.format())
							.context(format!(
								"variable '{}' may not be a slice",
								name.name
							)));
					}
					Some(ValueType::ExtArray { .. }) =>
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
				Ok(())
			}
			Statement::Assignment {
				reference,
				value,
				location,
			} =>
			{
				match reference
				{
					Reference::Identifier(..) => (),
					Reference::ArrayElement { name: _, argument } =>
					{
						argument
							.analyze(analyzer)
							.with_context(|| location.format())?;
					}
				}
				value.analyze(analyzer).with_context(|| location.format())?;
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
					else_branch.analyze(analyzer)?;
				}
				Ok(())
			}
			Statement::Block(block) => block.analyze(analyzer),
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
			} =>
			{
				analyzer.is_immediate_function_argument = false;
				left.analyze(analyzer).with_context(|| location.format())?;
				right.analyze(analyzer).with_context(|| location.format())?;
				Ok(())
			}
			Expression::PrimitiveLiteral(_lit) => Ok(()),
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
			Expression::StringLiteral(_lit) => Ok(()),
			Expression::Deref {
				reference,
				value_type,
			} =>
			{
				if !analyzer.is_immediate_function_argument
				{
					match value_type
					{
						Some(ValueType::Array { .. })
						| Some(ValueType::Slice { .. })
						| Some(ValueType::ExtArray { .. }) =>
						{
							let error = anyhow!("cannot move from array")
								.context(reference.location().format())
								.context("this variable cannot be moved from");
							return Err(error);
						}
						_ => (),
					}
				}
				analyzer.is_immediate_function_argument = false;
				match reference
				{
					Reference::Identifier(_) => (),
					Reference::ArrayElement { name, argument } =>
					{
						let argument_type = argument.value_type();
						analyzer.use_array(name, argument_type)?;
						argument.analyze(analyzer)?;
					}
				}
				Ok(())
			}
			Expression::LengthOfArray { reference } => match reference
			{
				Reference::Identifier(_) => Ok(()),
				Reference::ArrayElement { name, argument } =>
				{
					analyzer.is_immediate_function_argument = false;
					let argument_type = argument.value_type();
					analyzer.use_array(name, argument_type)?;
					argument.analyze(analyzer)?;
					Ok(())
				}
			},
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
		}
	}
}

fn does_argument_match_parameter(
	argument: &Option<ValueType>,
	parameter: &Option<ValueType>,
) -> bool
{
	match (argument, parameter)
	{
		(
			Some(ValueType::Array {
				element_type: x,
				length: _,
			}),
			Some(ValueType::Slice { element_type: y }),
		) => x == y,
		(
			Some(ValueType::Array {
				element_type: x,
				length: _,
			}),
			Some(ValueType::View {
				element_type: viewed_type,
			}),
		) => match viewed_type.as_ref()
		{
			ValueType::ExtArray { element_type: y } => x == y,
			_ => false,
		},
		(Some(a), Some(p)) => a == p,
		(_, _) => true,
	}
}
