/**/

use crate::typer::{Block, Declaration, FunctionBody, Statement};

use anyhow::anyhow;

pub fn analyze(program: &Vec<Declaration>) -> Result<(), anyhow::Error>
{
	let mut analyzer = Analyzer {
		label_stack: Vec::new(),
	};
	for declaration in program
	{
		declaration.analyze(&mut analyzer)?;
	}
	Ok(())
}

struct Analyzer
{
	label_stack: Vec<Vec<String>>,
}

impl Analyzer
{
	fn declare_label(&mut self, name: &str) -> Result<(), anyhow::Error>
	{
		for scope in &self.label_stack
		{
			if scope.iter().any(|x| x == name)
			{
				return Err(anyhow!(
					"Multiple labels '{}' with overlapping scope",
					name
				));
			}
		}

		if let Some(scope) = self.label_stack.last_mut()
		{
			scope.push(name.to_string());
		}
		else
		{
			self.label_stack.push(vec![name.to_string()]);
		}
		Ok(())
	}

	fn use_label(&self, name: &str) -> Result<(), anyhow::Error>
	{
		for scope in &self.label_stack
		{
			if scope.iter().any(|x| x == name)
			{
				return Ok(());
			}
		}

		Err(anyhow!("Reference to undefined label '{}'", name))
	}

	fn push_scope(&mut self)
	{
		self.label_stack.push(Vec::new());
	}

	fn pop_scope(&mut self)
	{
		self.label_stack.pop();
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
		for statement in self.statements.iter().rev()
		{
			statement.analyze(analyzer)?;
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
		for statement in self.statements.iter().rev()
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
			Statement::Declaration { .. } => Ok(()),
			Statement::Assignment { .. } => Ok(()),
			Statement::Loop => Ok(()),
			Statement::Goto { label } => analyzer.use_label(label),
			Statement::Label { label } => analyzer.declare_label(label),
			Statement::If {
				condition: _,
				then_branch,
				else_branch,
			} =>
			{
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
