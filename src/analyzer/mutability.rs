/**/

use crate::common::*;

use anyhow::anyhow;
use anyhow::Context;

pub fn analyze(program: &Vec<Declaration>) -> Result<(), anyhow::Error>
{
	let mut analyzer = Analyzer {
		variables: std::collections::HashMap::new(),
	};
	for declaration in program
	{
		declaration.analyze(&mut analyzer)?;
	}
	Ok(())
}

struct Analyzer
{
	variables: std::collections::HashMap<u32, (Identifier, bool)>,
}

impl Analyzer
{
	fn declare_variable(
		&mut self,
		identifier: &Identifier,
		is_mutable: bool,
	) -> Result<(), anyhow::Error>
	{
		let old_value = self
			.variables
			.insert(identifier.resolution_id, (identifier.clone(), is_mutable));
		match old_value
		{
			Some((_, was_mutable)) if was_mutable == is_mutable => Ok(()),
			Some((old_identifier, was_mutable)) => Err(anyhow!(
				"first occurrence {}",
				old_identifier.location.format()
			)
			.context(identifier.location.format())
			.context(format!(
				"conflicting mutabilities for '{}', {:?} and {:?}",
				identifier.name, was_mutable, is_mutable
			))),
			None => Ok(()),
		}
	}

	fn use_variable(
		&self,
		identifier: &Identifier,
		is_mutated: bool,
	) -> Result<(), anyhow::Error>
	{
		if let Some((previous_identifier, is_mutable)) =
			self.variables.get(&identifier.resolution_id)
		{
			if is_mutated && !is_mutable
			{
				return Err(anyhow!(
					"previous declaration {}",
					previous_identifier.location.format()
				)
				.context(identifier.location.format())
				.context(format!(
					"the variable '{}' is not mutable",
					identifier.name
				)));
			}
			else
			{
				Ok(())
			}
		}
		else
		{
			Err(anyhow!("undefined reference")
				.context(identifier.location.format())
				.context(format!(
					"reference to undefined variable named '{}'",
					identifier.name
				)))
		}
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
				parameters,
				body,
				return_type: _,
			} =>
			{
				for parameter in parameters
				{
					parameter.analyze(analyzer)?;
				}
				body.analyze(analyzer)?;
				Ok(())
			}
		}
	}
}

impl Analyzable for Parameter
{
	fn analyze(&self, analyzer: &mut Analyzer) -> Result<(), anyhow::Error>
	{
		analyzer.declare_variable(&self.name, false)?;
		Ok(())
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
		if let Some(return_value) = &self.return_value
		{
			return_value.analyze(analyzer)?;
		}
		Ok(())
	}
}

impl Analyzable for Block
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
				location,
			} =>
			{
				value.analyze(analyzer).with_context(|| location.format())?;
				analyzer.declare_variable(name, true)?;
				Ok(())
			}
			Statement::Declaration {
				name,
				value: None,
				value_type: _,
				location: _,
			} =>
			{
				analyzer.declare_variable(name, true)?;
				Ok(())
			}
			Statement::Assignment {
				reference,
				value,
				location,
			} =>
			{
				value.analyze(analyzer).with_context(|| location.format())?;
				match reference
				{
					Reference::Identifier(name) =>
					{
						analyzer.use_variable(name, true)
					}
					Reference::ArrayElement { name, argument } =>
					{
						analyzer.use_variable(name, true)?;
						argument.analyze(analyzer)?;
						Ok(())
					}
				}
			}
			Statement::Loop { location: _ } => Ok(()),
			Statement::Goto {
				label: _,
				location: _,
			} => Ok(()),
			Statement::Label {
				label: _,
				location: _,
			} => Ok(()),
			Statement::If {
				condition,
				then_branch,
				else_branch,
				location,
			} =>
			{
				condition
					.analyze(analyzer)
					.with_context(|| location.format())?;
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
		self.left
			.analyze(analyzer)
			.with_context(|| self.location.format())?;
		self.right
			.analyze(analyzer)
			.with_context(|| self.location.format())?;
		Ok(())
	}
}

impl Analyzable for Array
{
	fn analyze(&self, analyzer: &mut Analyzer) -> Result<(), anyhow::Error>
	{
		for element in &self.elements
		{
			element.analyze(analyzer)?;
		}
		Ok(())
	}
}

impl Analyzable for Expression
{
	fn analyze(&self, analyzer: &mut Analyzer) -> Result<(), anyhow::Error>
	{
		match self
		{
			Expression::Binary {
				op: _,
				left,
				right,
				location,
			} =>
			{
				left.analyze(analyzer).with_context(|| location.format())?;
				right.analyze(analyzer).with_context(|| location.format())?;
				Ok(())
			}
			Expression::PrimitiveLiteral(_lit) => Ok(()),
			Expression::ArrayLiteral {
				array,
				element_type: _,
			} => array.analyze(analyzer),
			Expression::StringLiteral(_lit) => Ok(()),
			Expression::Deref {
				reference,
				value_type: _,
			} => match reference
			{
				Reference::Identifier(name) =>
				{
					analyzer.use_variable(name, false)
				}
				Reference::ArrayElement { name, argument } =>
				{
					analyzer.use_variable(name, false)?;
					argument.analyze(analyzer)?;
					Ok(())
				}
			},
			Expression::FunctionCall {
				name: _,
				arguments,
				return_type: _,
			} =>
			{
				for argument in arguments
				{
					argument.analyze(analyzer)?;
				}
				Ok(())
			}
		}
	}
}
