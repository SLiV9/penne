/**/

use crate::common::*;

use anyhow::anyhow;

pub fn lint(program: &Vec<Declaration>) -> Vec<anyhow::Error>
{
	let mut linter = Linter {
		lints: Vec::new(),
		is_naked_branch: false,
		is_first_statement_of_branch: false,
	};
	for declaration in program
	{
		declaration.lint(&mut linter);
	}
	linter.lints
}

struct Linter
{
	lints: Vec<anyhow::Error>,
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
			Declaration::Constant {
				name: _,
				value: _,
				value_type: _,
				flags: _,
			} => (),
			Declaration::Function {
				name: _,
				parameters: _,
				body,
				return_type: _,
				flags: _,
			} => body.lint(linter),
			Declaration::FunctionHead {
				name: _,
				parameters: _,
				return_type: _,
				flags: _,
			} => (),
			Declaration::PreprocessorDirective { .. } => unreachable!(),
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
			Statement::MethodCall { .. } => (),
			Statement::Loop { location } =>
			{
				if linter.is_first_statement_of_branch
				{
					let lint = anyhow!("loop statement")
						.context(location.format())
						.context(
							"loop statement inside conditional code block \
								causes infinite loop if condition is met; \
								use conditional goto to break out of loop, \
								or add label to surpress this warning.",
						);
					linter.lints.push(lint);
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
