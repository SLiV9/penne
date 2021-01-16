/**/

use crate::typer::{Block, Declaration, FunctionBody, Statement};
use crate::typer::{Comparison, Expression, Identifier};

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
	function_list: Vec<Identifier>,
	variable_stack: Vec<Vec<Identifier>>,
}

impl Analyzer
{
	fn declare_function(
		&mut self,
		identifier: &Identifier,
	) -> Result<(), anyhow::Error>
	{
		if let Some(previous_identifier) = self
			.function_list
			.iter()
			.find(|x| x.name == identifier.name)
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

		self.function_list.push(identifier.clone());
		Ok(())
	}

	fn use_function(&self, identifier: &Identifier)
		-> Result<(), anyhow::Error>
	{
		if self.function_list.iter().any(|x| x.name == identifier.name)
		{
			return Ok(());
		}

		Err(anyhow!("undefined reference")
			.context(identifier.location.format())
			.context(format!(
				"reference to undefined function named '{}'",
				identifier.name
			)))
	}

	fn declare_variable(
		&mut self,
		identifier: &Identifier,
	) -> Result<(), anyhow::Error>
	{
		for scope in &self.variable_stack
		{
			if let Some(previous_identifier) =
				scope.iter().find(|x| x.name == identifier.name)
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
			scope.push(identifier.clone());
		}
		else
		{
			self.variable_stack.push(vec![identifier.clone()]);
		}
		Ok(())
	}

	fn use_variable(&self, identifier: &Identifier)
		-> Result<(), anyhow::Error>
	{
		for scope in &self.variable_stack
		{
			if scope.iter().any(|x| x.name == identifier.name)
			{
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
				body,
				return_type: _,
			} =>
			{
				analyzer.declare_function(name)?;
				analyzer.push_scope();
				// parameters in this scope
				body.analyze(analyzer)?;
				analyzer.pop_scope();
				Ok(())
			}
		}
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
				analyzer.declare_variable(name)?;
				Ok(())
			}
			Statement::Declaration {
				name,
				value: None,
				value_type: _,
				location: _,
			} =>
			{
				analyzer.declare_variable(name)?;
				Ok(())
			}
			Statement::Assignment {
				name,
				value,
				location,
			} =>
			{
				value.analyze(analyzer).with_context(|| location.format())?;
				analyzer.use_variable(name)?;
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
			Expression::Literal(_lit) => Ok(()),
			Expression::Variable {
				name,
				value_type: _,
			} => analyzer.use_variable(name),
			Expression::FunctionCall {
				name,
				arguments,
				return_type: _,
			} =>
			{
				analyzer.use_function(name)?;
				for argument in arguments
				{
					argument.analyze(analyzer)?;
				}
				Ok(())
			}
		}
	}
}
