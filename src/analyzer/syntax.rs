/**/

use crate::common::*;

use anyhow::anyhow;

pub fn analyze(program: &Vec<Declaration>) -> Result<(), anyhow::Error>
{
	let mut analyzer = Analyzer {
		is_naked_then_branch: false,
		is_naked_else_branch: false,
		is_final_statement_in_block: false,
	};
	for declaration in program
	{
		declaration.analyze(&mut analyzer)?;
	}
	Ok(())
}

struct Analyzer
{
	is_naked_then_branch: bool,
	is_naked_else_branch: bool,
	is_final_statement_in_block: bool,
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
				parameters: _,
				body,
				return_type: _,
				flags: _,
			} => body.analyze(analyzer),
			Declaration::FunctionHead {
				name: _,
				parameters: _,
				return_type: _,
				flags: _,
			} => Ok(()),
		}
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
		Ok(())
	}
}

impl Analyzable for Block
{
	fn analyze(&self, analyzer: &mut Analyzer) -> Result<(), anyhow::Error>
	{
		match self.statements.split_last()
		{
			Some((last, others)) =>
			{
				for statement in others
				{
					statement.analyze(analyzer)?;
				}
				analyzer.is_final_statement_in_block = true;
				last.analyze(analyzer)?;
				analyzer.is_final_statement_in_block = false;
				Ok(())
			}
			None => Ok(()),
		}
	}
}

impl Analyzable for Statement
{
	fn analyze(&self, analyzer: &mut Analyzer) -> Result<(), anyhow::Error>
	{
		if analyzer.is_naked_then_branch || analyzer.is_naked_else_branch
		{
			match self
			{
				Statement::Goto { .. } => (),
				Statement::Block(..) => (),
				Statement::If { .. } if analyzer.is_naked_else_branch => (),
				statement =>
				{
					return Err(anyhow!("missing braces")
						.context(statement.location().format())
						.context(
							"braces around conditional branches \
							can only be omitted for goto statements",
						))
				}
			}
		}

		match self
		{
			Statement::Declaration { .. } => Ok(()),
			Statement::Assignment { .. } => Ok(()),
			Statement::Loop { location } =>
			{
				if analyzer.is_final_statement_in_block
				{
					Ok(())
				}
				else
				{
					Err(anyhow!("misplaced loop statement")
						.context(location.format())
						.context(
							"loop statement must be final statement \
							in code block",
						))
				}
			}
			Statement::Goto { .. } => Ok(()),
			Statement::Label { .. } => Ok(()),
			Statement::If {
				condition: _,
				then_branch,
				else_branch,
				location: _,
			} =>
			{
				analyzer.is_final_statement_in_block = false;

				analyzer.is_naked_then_branch = true;
				then_branch.analyze(analyzer)?;
				analyzer.is_naked_then_branch = false;

				if let Some(else_branch) = else_branch
				{
					analyzer.is_naked_else_branch = true;
					else_branch.analyze(analyzer)?;
					analyzer.is_naked_else_branch = false;
				}

				Ok(())
			}
			Statement::Block(block) =>
			{
				analyzer.is_naked_then_branch = false;
				analyzer.is_naked_else_branch = false;
				block.analyze(analyzer)
			}
		}
	}
}
