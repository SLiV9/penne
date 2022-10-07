//
// Part of penne
// Copyright (c) 2020 Sander in 't Veld
// License: MIT
//

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
	program
		.into_iter()
		.map(|x| x.analyze(&mut analyzer))
		.collect()
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
		identifier: Identifier,
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
			resolution_id: self.resolution_id,
			..identifier
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
		identifier: Identifier,
	) -> Result<Identifier, anyhow::Error>
	{
		for scope in &self.label_stack
		{
			if let Some(previous_identifier) =
				scope.iter().find(|x| x.name == identifier.name)
			{
				return Ok(Identifier {
					resolution_id: previous_identifier.resolution_id,
					..identifier
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
		self,
		analyzer: &mut Analyzer,
	) -> Result<Self::Item, anyhow::Error>;
}

impl Analyzable for Declaration
{
	type Item = Declaration;

	fn analyze(
		self,
		analyzer: &mut Analyzer,
	) -> Result<Declaration, anyhow::Error>
	{
		match self
		{
			Declaration::Constant { .. } => Ok(self),
			Declaration::Function {
				name,
				parameters,
				body,
				return_type,
				flags,
			} =>
			{
				let body = match body
				{
					Ok(body) => Ok(body.analyze(analyzer)?),
					Err(poison) => Err(poison),
				};
				let function = Declaration::Function {
					name,
					parameters,
					body,
					return_type,
					flags,
				};
				Ok(function)
			}
			Declaration::FunctionHead { .. } => Ok(self),
			Declaration::PreprocessorDirective { .. } => unreachable!(),
			Declaration::Poison(Poison::Error {
				error,
				partial: Some(declaration),
			}) =>
			{
				let declaration = declaration.analyze(analyzer)?;
				Ok(Declaration::Poison(Poison::Error {
					error,
					partial: Some(Box::new(declaration)),
				}))
			}
			Declaration::Poison(Poison::Error {
				error: _,
				partial: None,
			}) => Ok(self),
			Declaration::Poison(Poison::Poisoned) => Ok(self),
		}
	}
}

impl Analyzable for FunctionBody
{
	type Item = FunctionBody;

	fn analyze(
		self,
		analyzer: &mut Analyzer,
	) -> Result<FunctionBody, anyhow::Error>
	{
		analyzer.push_scope();
		let statements: Result<Vec<Statement>, anyhow::Error> = self
			.statements
			.into_iter()
			.rev()
			.map(|x| x.analyze(analyzer))
			.collect();
		let mut statements = statements?;
		statements.reverse();
		analyzer.pop_scope();

		Ok(FunctionBody {
			statements,
			return_value: self.return_value,
			return_value_identifier: self.return_value_identifier,
		})
	}
}

impl Analyzable for Block
{
	type Item = Block;

	fn analyze(
		self,
		analyzer: &mut Analyzer,
	) -> Result<Self::Item, anyhow::Error>
	{
		analyzer.push_scope();
		let statements: Result<Vec<Statement>, anyhow::Error> = self
			.statements
			.into_iter()
			.rev()
			.map(|x| x.analyze(analyzer))
			.collect();
		let mut statements = statements?;
		statements.reverse();
		analyzer.pop_scope();

		Ok(Block {
			statements,
			location: self.location,
		})
	}
}

impl Analyzable for Statement
{
	type Item = Statement;

	fn analyze(
		self,
		analyzer: &mut Analyzer,
	) -> Result<Statement, anyhow::Error>
	{
		match self
		{
			Statement::Declaration { .. } => Ok(self),
			Statement::Assignment { .. } => Ok(self),
			Statement::MethodCall { .. } => Ok(self),
			Statement::Loop { location: _ } => Ok(self),
			Statement::Goto { label, location } =>
			{
				let label = analyzer.use_label(label)?;
				Ok(Statement::Goto { label, location })
			}
			Statement::Label { label, location } =>
			{
				let label = analyzer.declare_label(label)?;
				Ok(Statement::Label { label, location })
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
						let branch = else_branch.branch.analyze(analyzer)?;
						Some(Else {
							branch: Box::new(branch),
							location_of_else: else_branch.location_of_else,
						})
					}
					None => None,
				};
				Ok(Statement::If {
					condition,
					then_branch,
					else_branch,
					location,
				})
			}
			Statement::Block(block) =>
			{
				let block = block.analyze(analyzer)?;
				Ok(Statement::Block(block))
			}
			Statement::Poison(Poison::Error {
				error,
				partial: Some(statement),
			}) =>
			{
				let statement = statement.analyze(analyzer)?;
				Ok(Statement::Poison(Poison::Error {
					error,
					partial: Some(Box::new(statement)),
				}))
			}
			Statement::Poison(Poison::Error {
				error: _,
				partial: None,
			}) => Ok(self),
			Statement::Poison(Poison::Poisoned) => Ok(self),
		}
	}
}
