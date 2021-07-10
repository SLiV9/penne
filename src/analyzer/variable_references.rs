/**/

use crate::common::*;
use crate::typer::Typed;

use anyhow::anyhow;
use anyhow::Context;

pub fn analyze(program: &Vec<Declaration>) -> Result<(), anyhow::Error>
{
	let mut analyzer = Analyzer {
		function_list: Vec::new(),
		variable_stack: Vec::new(),
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
	variable_stack: Vec<Vec<(Identifier, bool)>>,
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

	fn declare_variable(
		&mut self,
		identifier: &Identifier,
		is_mutable: bool,
	) -> Result<(), anyhow::Error>
	{
		for scope in &self.variable_stack
		{
			if let Some((previous_identifier, _)) =
				scope.iter().find(|(x, _)| x.name == identifier.name)
			{
				return Err(anyhow!(
					"previous declaration {}",
					previous_identifier.location.format()
				)
				.context(identifier.location.format())
				.context(format!(
					"a variables named '{}' is already defined in this scope",
					identifier.name
				)));
			}
		}

		if let Some(scope) = self.variable_stack.last_mut()
		{
			scope.push((identifier.clone(), is_mutable));
		}
		else
		{
			self.variable_stack
				.push(vec![(identifier.clone(), is_mutable)]);
		}
		Ok(())
	}

	fn use_variable(
		&self,
		identifier: &Identifier,
		is_mutated: bool,
	) -> Result<(), anyhow::Error>
	{
		for scope in &self.variable_stack
		{
			if let Some((previous_identifier, is_mutable)) =
				scope.iter().find(|(x, _)| x.name == identifier.name)
			{
				if is_mutated && !is_mutable
				{
					return Err(anyhow!(
						"previous declaration {}",
						previous_identifier.location.format()
					)
					.context(identifier.location.format())
					.context(format!(
						"the variable '{}' is not mutable",
						identifier.name
					)));
				}
				return Ok(());
			}
		}

		Err(anyhow!("undefined reference")
			.context(identifier.location.format())
			.context(format!(
				"reference to undefined variable named '{}'",
				identifier.name
			)))
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
				analyzer.push_scope();
				for parameter in parameters
				{
					parameter.analyze(analyzer)?;
				}
				body.analyze(analyzer)?;
				analyzer.pop_scope();
				Ok(())
			}
		}
	}
}

impl Analyzable for Parameter
{
	fn analyze(&self, analyzer: &mut Analyzer) -> Result<(), anyhow::Error>
	{
		analyzer.declare_variable(&self.name, self.is_mutable)?;
		Ok(())
	}
}

impl Analyzable for FunctionBody
{
	fn analyze(&self, analyzer: &mut Analyzer) -> Result<(), anyhow::Error>
	{
		analyzer.push_scope();
		for statement in &self.statements
		{
			statement.analyze(analyzer)?;
		}
		if let Some(return_value) = &self.return_value
		{
			return_value.analyze(analyzer)?;
		}
		analyzer.pop_scope();
		Ok(())
	}
}

impl Analyzable for Block
{
	fn analyze(&self, analyzer: &mut Analyzer) -> Result<(), anyhow::Error>
	{
		analyzer.push_scope();
		for statement in &self.statements
		{
			statement.analyze(analyzer)?;
		}
		analyzer.pop_scope();
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
				value_type: _,
				location,
			} =>
			{
				value.analyze(analyzer).with_context(|| location.format())?;
				analyzer.declare_variable(name, true)?;
				Ok(())
			}
			Statement::Declaration {
				name,
				value: None,
				value_type: _,
				location: _,
			} =>
			{
				analyzer.declare_variable(name, true)?;
				Ok(())
			}
			Statement::Assignment {
				name,
				value,
				location,
			} =>
			{
				value.analyze(analyzer).with_context(|| location.format())?;
				analyzer.use_variable(name, true)?;
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
		analyzer.push_scope();
		for element in &self.elements
		{
			element.analyze(analyzer)?;
		}
		analyzer.pop_scope();
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
			Expression::Variable {
				name,
				value_type: _,
			} => analyzer.use_variable(name, false),
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
