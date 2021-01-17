/**/

use crate::common::*;

use anyhow::anyhow;

pub fn lint(program: &Vec<Declaration>)
{
	let mut linter = Linter {
		is_naked_branch: false,
		is_first_statement_of_branch: false,
	};
	for declaration in program
	{
		declaration.lint(&mut linter);
	}
}

struct Linter
{
	is_naked_branch: bool,
	is_first_statement_of_branch: bool,
}

trait Lintable
{
	fn lint(&self, linter: &mut Linter);
}

impl Lintable for Declaration
{
	fn lint(&self, linter: &mut Linter)
	{
		match self
		{
			Declaration::Function {
				name: _,
				parameters: _,
				body,
				return_type: _,
			} => body.lint(linter),
		}
	}
}

impl Lintable for FunctionBody
{
	fn lint(&self, linter: &mut Linter)
	{
		for statement in &self.statements
		{
			statement.lint(linter);
		}
	}
}

impl Lintable for Block
{
	fn lint(&self, linter: &mut Linter)
	{
		match self.statements.split_first()
		{
			Some((first, others)) =>
			{
				linter.is_first_statement_of_branch = linter.is_naked_branch;
				linter.is_naked_branch = false;
				first.lint(linter);
				linter.is_first_statement_of_branch = false;
				for statement in others
				{
					statement.lint(linter);
				}
			}
			None => (),
		}
	}
}

impl Lintable for Statement
{
	fn lint(&self, linter: &mut Linter)
	{
		match self
		{
			Statement::Declaration { .. } => (),
			Statement::Assignment { .. } => (),
			Statement::Loop { location } =>
			{
				if linter.is_first_statement_of_branch
				{
					println!(
						"Warning: {:?}",
						anyhow!("loop statement")
							.context(location.format())
							.context(
								"loop statement inside conditional code block \
								causes infinite loop if condition is met; \
								use conditional goto to break out of loop, \
								or add label to surpress this warning."
							)
					);
				}
			}
			Statement::Goto { .. } => (),
			Statement::Label { .. } => (),
			Statement::If {
				condition: _,
				then_branch,
				else_branch,
				location: _,
			} =>
			{
				linter.is_first_statement_of_branch = false;

				linter.is_naked_branch = true;
				then_branch.lint(linter);

				if let Some(else_branch) = else_branch
				{
					linter.is_naked_branch = true;
					else_branch.lint(linter);
				}
				linter.is_naked_branch = false;
			}
			Statement::Block(block) => block.lint(linter),
		}
	}
}
