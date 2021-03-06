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
	};
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
			.find(|(x, _)| x.name == identifier.name)
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
			.find(|(x, _)| x.name == identifier.name)
		{
			if parameters.len() < arguments.len()
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
			else if parameters.len() > arguments.len()
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
					p.value_type.is_some() && a.is_some() && p.value_type != **a
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
				Some(ValueType::Int32) => Ok(()),
				Some(other_type) => Err(anyhow!("got {:?}", other_type)
					.context(format!("expected integer type"))
					.context(format!(
						"function was declared {}",
						declaration_identifier.location.format()
					))
					.context(identifier.location.format())
					.context(format!(
						"type mismatch of index into array '{}'",
						identifier.name,
					))),
				None => Err(anyhow!("failed to determine type")
					.context(format!("expected integer type"))
					.context(format!(
						"function was declared {}",
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

trait Analyzable
{
	fn analyze(&self, analyzer: &mut Analyzer) -> Result<(), anyhow::Error>;
}

impl Analyzable for Declaration
{
	fn analyze(&self, analyzer: &mut Analyzer) -> Result<(), anyhow::Error>
	{
		match self
		{
			Declaration::Function {
				name,
				parameters,
				body,
				return_type: _,
			} =>
			{
				analyzer.declare_function(name, parameters)?;
				for parameter in parameters
				{
					parameter.analyze(analyzer)?;
				}
				body.analyze(analyzer)?;
				Ok(())
			}
		}
	}
}

impl Analyzable for Parameter
{
	fn analyze(&self, _analyzer: &mut Analyzer) -> Result<(), anyhow::Error>
	{
		Ok(())
	}
}

impl Analyzable for FunctionBody
{
	fn analyze(&self, analyzer: &mut Analyzer) -> Result<(), anyhow::Error>
	{
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
					_ => (),
				}
				value.analyze(analyzer).with_context(|| location.format())?;
				Ok(())
			}
			Statement::Declaration {
				name,
				value: None,
				value_type,
				location: _,
			} =>
			{
				match value_type
				{
					Some(ValueType::Array { .. }) =>
					{
						analyzer.declare_array(name)?;
					}
					_ => (),
				}
				Ok(())
			}
			Statement::Assignment {
				name: _,
				value,
				location,
			} =>
			{
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
				left.analyze(analyzer).with_context(|| location.format())?;
				right.analyze(analyzer).with_context(|| location.format())?;
				Ok(())
			}
			Expression::PrimitiveLiteral(_lit) => Ok(()),
			Expression::ArrayLiteral {
				array,
				element_type: _,
			} => array.analyze(analyzer),
			Expression::StringLiteral(_lit) => Ok(()),
			Expression::Variable { name, value_type } =>
			{
				match value_type
				{
					Some(ValueType::Array { .. }) =>
					{
						return Err(anyhow!("cannot move from array")
							.context(name.location.format())
							.context(format!(
								"the variable '{}' cannot be moved from",
								name.name
							)));
					}
					_ => (),
				}
				Ok(())
			}
			Expression::ArrayAccess {
				name,
				argument,
				element_type: _,
			} =>
			{
				let argument_type = argument.value_type();
				analyzer.use_array(name, argument_type)?;
				argument.analyze(analyzer)?;
				Ok(())
			}
			Expression::FunctionCall {
				name,
				arguments,
				return_type: _,
			} =>
			{
				let argument_types =
					arguments.iter().map(|x| x.value_type()).collect();
				analyzer.use_function(name, argument_types)?;
				for argument in arguments
				{
					argument.analyze(analyzer)?;
				}
				Ok(())
			}
		}
	}
}
