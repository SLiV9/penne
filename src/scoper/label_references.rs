/**/

use crate::common::*;

use anyhow::anyhow;

pub fn analyze(
	program: Vec<Declaration>,
) -> Result<Vec<Declaration>, anyhow::Error>
{
	let mut analyzer = Analyzer {
		label_stack: Vec::new(),
		resolution_id: 1,
	};
	program.iter().map(|x| x.analyze(&mut analyzer)).collect()
}

struct Analyzer
{
	label_stack: Vec<Vec<Identifier>>,
	resolution_id: u32,
}

impl Analyzer
{
	fn declare_label(
		&mut self,
		identifier: &Identifier,
	) -> Result<Identifier, anyhow::Error>
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

		let identifier = Identifier {
			name: identifier.name.clone(),
			location: identifier.location.clone(),
			resolution_id: self.resolution_id,
		};
		self.resolution_id += 1;

		if let Some(scope) = self.label_stack.last_mut()
		{
			scope.push(identifier.clone());
		}
		else
		{
			self.label_stack.push(vec![identifier.clone()]);
		}
		Ok(identifier)
	}

	fn use_label(
		&self,
		identifier: &Identifier,
	) -> Result<Identifier, anyhow::Error>
	{
		for scope in &self.label_stack
		{
			if let Some(previous_identifier) =
				scope.iter().find(|x| x.name == identifier.name)
			{
				return Ok(Identifier {
					resolution_id: previous_identifier.resolution_id,
					..identifier.clone()
				});
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
	type Item;

	fn analyze(
		&self,
		analyzer: &mut Analyzer,
	) -> Result<Self::Item, anyhow::Error>;
}

impl Analyzable for Declaration
{
	type Item = Declaration;

	fn analyze(
		&self,
		analyzer: &mut Analyzer,
	) -> Result<Declaration, anyhow::Error>
	{
		match self
		{
			Declaration::Function {
				name,
				parameters,
				body,
				return_type,
				flags,
			} =>
			{
				let body = body.analyze(analyzer)?;

				let function = Declaration::Function {
					name: name.clone(),
					parameters: parameters.clone(),
					body,
					return_type: return_type.clone(),
					flags: *flags,
				};
				Ok(function)
			}
			Declaration::FunctionHead { .. } => Ok(self.clone()),
		}
	}
}

impl Analyzable for FunctionBody
{
	type Item = FunctionBody;

	fn analyze(
		&self,
		analyzer: &mut Analyzer,
	) -> Result<FunctionBody, anyhow::Error>
	{
		analyzer.push_scope();
		let statements: Result<Vec<Statement>, anyhow::Error> = self
			.statements
			.iter()
			.rev()
			.map(|x| x.analyze(analyzer))
			.collect();
		let mut statements = statements?;
		statements.reverse();
		analyzer.pop_scope();

		Ok(FunctionBody {
			statements,
			return_value: self.return_value.clone(),
			return_value_identifier: self.return_value_identifier.clone(),
		})
	}
}

impl Analyzable for Block
{
	type Item = Block;

	fn analyze(
		&self,
		analyzer: &mut Analyzer,
	) -> Result<Self::Item, anyhow::Error>
	{
		analyzer.push_scope();
		let statements: Result<Vec<Statement>, anyhow::Error> = self
			.statements
			.iter()
			.rev()
			.map(|x| x.analyze(analyzer))
			.collect();
		let mut statements = statements?;
		statements.reverse();
		analyzer.pop_scope();

		Ok(Block {
			statements,
			location: self.location.clone(),
		})
	}
}

impl Analyzable for Statement
{
	type Item = Statement;

	fn analyze(
		&self,
		analyzer: &mut Analyzer,
	) -> Result<Statement, anyhow::Error>
	{
		match self
		{
			Statement::Declaration { .. } => Ok(self.clone()),
			Statement::Assignment { .. } => Ok(self.clone()),
			Statement::Loop { location: _ } => Ok(self.clone()),
			Statement::Goto { label, location } =>
			{
				let label = analyzer.use_label(label)?;
				Ok(Statement::Goto {
					label,
					location: location.clone(),
				})
			}
			Statement::Label { label, location } =>
			{
				let label = analyzer.declare_label(label)?;
				Ok(Statement::Label {
					label,
					location: location.clone(),
				})
			}
			Statement::If {
				condition,
				then_branch,
				else_branch,
				location,
			} =>
			{
				let then_branch = {
					let branch = then_branch.analyze(analyzer)?;
					Box::new(branch)
				};
				let else_branch = match else_branch
				{
					Some(else_branch) =>
					{
						let branch = else_branch.analyze(analyzer)?;
						Some(Box::new(branch))
					}
					None => None,
				};
				Ok(Statement::If {
					condition: condition.clone(),
					then_branch,
					else_branch,
					location: location.clone(),
				})
			}
			Statement::Block(block) =>
			{
				let block = block.analyze(analyzer)?;
				Ok(Statement::Block(block))
			}
		}
	}
}
