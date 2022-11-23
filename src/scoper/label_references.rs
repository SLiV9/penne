//
// Part of penne
// Copyright (c) 2020 Sander in 't Veld
// License: MIT
//

use crate::common::*;
use crate::error::Error;

pub fn analyze(program: Vec<Declaration>) -> Vec<Declaration>
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
	) -> Result<Identifier, Error>
	{
		let mut recoverable_error = None;
		for scope in &self.label_stack
		{
			if let Some(previous_identifier) =
				scope.iter().find(|x| x.name == identifier.name)
			{
				// Because we analyze labels backwards, the previous label is
				// the one that should be marked as a duplicate.
				recoverable_error = Some(Error::DuplicateDeclarationLabel {
					name: identifier.name.clone(),
					location: previous_identifier.location.clone(),
					previous: identifier.location.clone(),
				});
				break;
			}
		}

		let identifier = Identifier {
			resolution_id: self.resolution_id,
			is_authoritative: true,
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

		if let Some(error) = recoverable_error
		{
			Err(error)
		}
		else
		{
			Ok(identifier)
		}
	}

	fn use_label(&self, identifier: Identifier) -> Result<Identifier, Error>
	{
		for scope in &self.label_stack
		{
			if let Some(previous_identifier) =
				scope.iter().find(|x| x.name == identifier.name)
			{
				return Ok(Identifier {
					resolution_id: previous_identifier.resolution_id,
					is_authoritative: false,
					..identifier
				});
			}
		}

		Err(Error::UndefinedLabel {
			name: identifier.name.clone(),
			location: identifier.location.clone(),
		})
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
	fn analyze(self, analyzer: &mut Analyzer) -> Self;
}

impl Analyzable for Declaration
{
	fn analyze(self, analyzer: &mut Analyzer) -> Self
	{
		match self
		{
			Declaration::Constant { .. } => self,
			Declaration::Function {
				name,
				parameters,
				body,
				return_type,
				flags,
				location_of_declaration,
				location_of_return_type,
			} =>
			{
				let body = match body
				{
					Ok(body) => Ok(body.analyze(analyzer)),
					Err(poison) => Err(poison),
				};
				Declaration::Function {
					name,
					parameters,
					body,
					return_type,
					flags,
					location_of_declaration,
					location_of_return_type,
				}
			}
			Declaration::FunctionHead { .. } => self,
			Declaration::Structure { .. } => self,
			Declaration::PreprocessorDirective { .. } => unreachable!(),
			Declaration::Poison(_) => self,
		}
	}
}

impl Analyzable for FunctionBody
{
	fn analyze(self, analyzer: &mut Analyzer) -> Self
	{
		analyzer.push_scope();
		let mut statements: Vec<Statement> = self
			.statements
			.into_iter()
			.rev()
			.map(|x| x.analyze(analyzer))
			.collect();
		// We need to collect the statements before reversing them again,
		// because .rev().rev() will just iterate in normal order.
		statements.reverse();
		analyzer.pop_scope();

		FunctionBody {
			statements,
			return_value: self.return_value,
			return_value_identifier: self.return_value_identifier,
		}
	}
}

impl Analyzable for Block
{
	fn analyze(self, analyzer: &mut Analyzer) -> Self
	{
		analyzer.push_scope();
		let mut statements: Vec<Statement> = self
			.statements
			.into_iter()
			.rev()
			.map(|x| x.analyze(analyzer))
			.collect();
		// We need to collect the statements before reversing them again,
		// because .rev().rev() will just iterate in normal order.
		statements.reverse();
		analyzer.pop_scope();

		Block {
			statements,
			location: self.location,
		}
	}
}

impl Analyzable for Statement
{
	fn analyze(self, analyzer: &mut Analyzer) -> Self
	{
		match self
		{
			Statement::Declaration { .. } => self,
			Statement::Assignment { .. } => self,
			Statement::MethodCall { .. } => self,
			Statement::Loop { location: _ } => self,
			Statement::Goto { label, location } =>
			{
				match analyzer.use_label(label)
				{
					Ok(label) => Statement::Goto { label, location },
					Err(error) => Statement::Poison(Poison::Error(error)),
				}
			}
			Statement::Label { label, location } =>
			{
				match analyzer.declare_label(label)
				{
					Ok(label) => Statement::Label { label, location },
					Err(error) => Statement::Poison(Poison::Error(error)),
				}
			}
			Statement::If {
				condition,
				then_branch,
				else_branch,
				location,
			} =>
			{
				let then_branch = {
					let branch = then_branch.analyze(analyzer);
					Box::new(branch)
				};
				let else_branch = match else_branch
				{
					Some(else_branch) =>
					{
						let branch = else_branch.branch.analyze(analyzer);
						Some(Else {
							branch: Box::new(branch),
							location_of_else: else_branch.location_of_else,
						})
					}
					None => None,
				};
				Statement::If {
					condition,
					then_branch,
					else_branch,
					location,
				}
			}
			Statement::Block(block) =>
			{
				let block = block.analyze(analyzer);
				Statement::Block(block)
			}
			Statement::Poison(_) => self,
		}
	}
}
