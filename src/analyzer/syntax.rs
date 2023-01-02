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
		discovered_labels: std::collections::HashMap::new(),
		unresolved_gotos: std::collections::HashMap::new(),
		is_naked_then_branch: false,
		is_naked_else_branch: false,
		is_in_block: false,
	};
	program
		.into_iter()
		.map(|x| x.analyze(&mut analyzer))
		.collect()
}

struct Analyzer
{
	discovered_labels: std::collections::HashMap<u32, Identifier>,
	unresolved_gotos: std::collections::HashMap<u32, Identifier>,
	is_naked_then_branch: bool,
	is_naked_else_branch: bool,
	is_in_block: bool,
}

impl Analyzer
{
	fn discover_label(
		&mut self,
		label: &Identifier,
		location_for_context: &Location,
	)
	{
		let context = Identifier {
			location: location_for_context.clone(),
			..label.clone()
		};
		self.discovered_labels.insert(label.resolution_id, context);
	}

	fn add_goto(&mut self, label: &Identifier, location_for_context: &Location)
	{
		let context = Identifier {
			location: location_for_context.clone(),
			..label.clone()
		};
		self.unresolved_gotos.insert(label.resolution_id, context);
	}

	fn resolve_goto(&mut self, label: &Identifier)
	{
		self.unresolved_gotos.remove(&label.resolution_id);
	}

	fn get_first_unresolved_goto(&self) -> Option<UnresolvedGoto>
	{
		if let Some((rid, goto)) = self.unresolved_gotos.iter().next()
		{
			let goto = goto.clone();
			let label = match self.discovered_labels.get(rid)
			{
				Some(label) => Ok(label.clone()),
				None => Err(()),
			};
			Some(UnresolvedGoto { goto, label })
		}
		else
		{
			None
		}
	}
}

struct UnresolvedGoto
{
	goto: Identifier,
	label: Result<Identifier, ()>,
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
			Declaration::Constant {
				name: _,
				value: _,
				value_type: _,
				flags: _,
				location_of_declaration: _,
				location_of_type: _,
			} => self,
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
			Declaration::FunctionHead {
				name: _,
				parameters: _,
				return_type: _,
				flags: _,
				location_of_declaration: _,
				location_of_return_type: _,
			} => self,
			Declaration::Structure {
				name: _,
				members: _,
				structural_type: _,
				flags: _,
				depth: _,
				location_of_declaration: _,
			} => self,
			Declaration::Import { .. } => self,
			Declaration::Poison(_) => self,
		}
	}
}

impl Analyzable for FunctionBody
{
	fn analyze(self, analyzer: &mut Analyzer) -> Self
	{
		discover_labels(&self.statements, analyzer);

		// We are a function body, not a block.
		analyzer.is_in_block = false;

		let statements = self
			.statements
			.into_iter()
			.map(|x| x.analyze(analyzer))
			.collect();

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
		discover_labels(&self.statements, analyzer);

		let (others, last) = {
			let mut statements = self.statements;
			let last = statements.pop();
			let others = statements;
			(others, last)
		};
		let statements = match last
		{
			Some(last) =>
			{
				let mut statements: Vec<Statement> = others
					.into_iter()
					.map(|statement| {
						analyzer.is_in_block = true;
						let statement = statement.analyze(analyzer);
						match statement
						{
							Statement::Loop { location } => Statement::Poison(
								Poison::Error(Error::NonFinalLoopStatement {
									location,
									location_of_block: self.location.clone(),
								}),
							),
							Statement::Poison(_) => statement,
							_ => statement,
						}
					})
					.collect();
				analyzer.is_in_block = true;
				let last = last.analyze(analyzer);
				analyzer.is_in_block = false;
				statements.push(last);
				statements
			}
			None => Vec::new(),
		};

		Block {
			statements,
			location: self.location,
		}
	}
}

fn discover_labels(statements: &[Statement], analyzer: &mut Analyzer)
{
	// Look ahead into the top-level statements of this function body or block
	// for labels, so that any goto-related error knows where the corresponding
	// label is.
	for statement in statements
	{
		match statement
		{
			Statement::Label { label, location } =>
			{
				analyzer.discover_label(label, location);
			}
			Statement::Block(_) =>
			{
				// Do not look into blocks because they have their own
				// label scope.
			}
			_ => (),
		}
	}
}

impl Analyzable for Statement
{
	fn analyze(self, analyzer: &mut Analyzer) -> Self
	{
		if analyzer.is_naked_then_branch || analyzer.is_naked_else_branch
		{
			match &self
			{
				Statement::Goto { .. } => (),
				Statement::Block(..) => (),
				Statement::If { .. } if analyzer.is_naked_else_branch => (),
				Statement::Poison(_poison) => (),
				statement =>
				{
					return Statement::Poison(Poison::Error(
						Error::MissingBraces {
							location: statement.location().clone(),
						},
					));
				}
			}
		}

		match self
		{
			Statement::Declaration {
				name,
				value,
				value_type,
				location,
			} => match analyzer.get_first_unresolved_goto()
			{
				Some(UnresolvedGoto {
					goto,
					label: Ok(label),
				}) => Statement::Poison(Poison::Error(
					Error::VariableDeclarationMayBeSkipped {
						label: label.name.clone(),
						location,
						location_of_goto: goto.location,
						location_of_label: label.location,
					},
				)),
				Some(UnresolvedGoto {
					goto: _,
					label: Err(()),
				}) => Statement::Poison(Poison::Poisoned),
				None => Statement::Declaration {
					name,
					value,
					value_type,
					location,
				},
			},
			Statement::Assignment { .. } => self,
			Statement::MethodCall { .. } => self,
			Statement::Loop { location } =>
			{
				if analyzer.is_in_block
				{
					Statement::Loop { location }
				}
				else
				{
					Statement::Poison(Poison::Error(
						Error::MisplacedLoopStatement { location },
					))
				}
			}
			Statement::Goto { label, location } =>
			{
				analyzer.add_goto(&label, &location);
				Statement::Goto { label, location }
			}
			Statement::Label { label, location } =>
			{
				analyzer.resolve_goto(&label);
				Statement::Label { label, location }
			}
			Statement::If {
				condition,
				then_branch,
				else_branch,
				location,
			} =>
			{
				analyzer.is_in_block = false;

				analyzer.is_naked_then_branch = true;
				let then_branch = Box::new(then_branch.analyze(analyzer));
				analyzer.is_naked_then_branch = false;

				let else_branch = else_branch.map(|x| {
					analyzer.is_naked_else_branch = true;
					let branch = x.branch.analyze(analyzer);
					analyzer.is_naked_else_branch = false;

					Else {
						branch: Box::new(branch),
						location_of_else: x.location_of_else,
					}
				});

				Statement::If {
					condition,
					then_branch,
					else_branch,
					location,
				}
			}
			Statement::Block(block) =>
			{
				analyzer.is_naked_then_branch = false;
				analyzer.is_naked_else_branch = false;
				let block = block.analyze(analyzer);
				Statement::Block(block)
			}
			Statement::Poison(_) => self,
		}
	}
}
