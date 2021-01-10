/**/

use crate::typer::{Block, Declaration, FunctionBody, Identifier, Statement};

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
	label_stack: Vec<Vec<Identifier>>,
}

impl Analyzer
{
	fn declare_label(
		&mut self,
		identifier: &Identifier,
	) -> Result<(), anyhow::Error>
	{
		for scope in &self.label_stack
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
					"a label named '{}' is already defined in this scope",
					identifier.name
				)));
			}
		}

		if let Some(scope) = self.label_stack.last_mut()
		{
			scope.push(identifier.clone());
		}
		else
		{
			self.label_stack.push(vec![identifier.clone()]);
		}
		Ok(())
	}

	fn use_label(&self, identifier: &Identifier) -> Result<(), anyhow::Error>
	{
		for scope in &self.label_stack
		{
			if scope.iter().any(|x| x.name == identifier.name)
			{
				return Ok(());
			}
		}

		Err(anyhow!("undefined label '{}'", identifier.name)
			.context(identifier.location.format())
			.context(format!(
				"reference to undefined label '{}'",
				identifier.name
			)))
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
			Statement::Loop { location: _ } => Ok(()),
			Statement::Goto { label, location: _ } => analyzer.use_label(label),
			Statement::Label { label, location: _ } =>
			{
				analyzer.declare_label(label)
			}
			Statement::If {
				condition: _,
				then_branch,
				else_branch,
				location: _,
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
