/**/

use crate::typer::{Block, Declaration, FunctionBody, Statement};
use crate::typer::{Comparison, Expression};

use anyhow::anyhow;

pub fn analyze(program: &Vec<Declaration>) -> Result<(), anyhow::Error>
{
	let mut analyzer = Analyzer {
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
	variable_stack: Vec<Vec<String>>,
}

impl Analyzer
{
	fn declare_variable(&mut self, name: &str) -> Result<(), anyhow::Error>
	{
		for scope in &self.variable_stack
		{
			if scope.iter().any(|x| x == name)
			{
				return Err(anyhow!(
					"Multiple variables named '{}' with overlapping scope",
					name
				));
			}
		}

		if let Some(scope) = self.variable_stack.last_mut()
		{
			scope.push(name.to_string());
		}
		else
		{
			self.variable_stack.push(vec![name.to_string()]);
		}
		Ok(())
	}

	fn use_variable(&self, name: &str) -> Result<(), anyhow::Error>
	{
		for scope in &self.variable_stack
		{
			if scope.iter().any(|x| x == name)
			{
				return Ok(());
			}
		}

		Err(anyhow!("Reference to undefined variable named '{}'", name))
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
				name: _,
				body,
				return_type: _,
			} =>
			{
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
			} =>
			{
				value.analyze(analyzer)?;
				analyzer.declare_variable(name)?;
				Ok(())
			}
			Statement::Declaration {
				name,
				value: None,
				value_type: _,
			} =>
			{
				analyzer.declare_variable(name)?;
				Ok(())
			}
			Statement::Assignment { name, value } =>
			{
				value.analyze(analyzer)?;
				analyzer.use_variable(name)?;
				Ok(())
			}
			Statement::Loop => Ok(()),
			Statement::Goto { label: _ } => Ok(()),
			Statement::Label { label: _ } => Ok(()),
			Statement::If {
				condition,
				then_branch,
				else_branch,
			} =>
			{
				condition.analyze(analyzer)?;
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
		self.left.analyze(analyzer)?;
		self.right.analyze(analyzer)?;
		Ok(())
	}
}

impl Analyzable for Expression
{
	fn analyze(&self, analyzer: &mut Analyzer) -> Result<(), anyhow::Error>
	{
		match self
		{
			Expression::Binary { op: _, left, right } =>
			{
				left.analyze(analyzer)?;
				right.analyze(analyzer)?;
				Ok(())
			}
			Expression::Literal(_lit) => Ok(()),
			Expression::Variable {
				name,
				value_type: _,
			} => analyzer.use_variable(name),
		}
	}
}
