/**/

use crate::common::*;

use anyhow::anyhow;
use anyhow::Context;
use enumset::{EnumSet, EnumSetType};

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

#[derive(EnumSetType, Debug)]
enum Flag
{
	Mutable,
	Addressable,
}

struct Analyzer
{
	variables: std::collections::HashMap<u32, (Identifier, EnumSet<Flag>)>,
}

impl Analyzer
{
	fn declare_variable(
		&mut self,
		identifier: &Identifier,
		flags: EnumSet<Flag>,
	) -> Result<(), anyhow::Error>
	{
		let old_value = self
			.variables
			.insert(identifier.resolution_id, (identifier.clone(), flags));
		match old_value
		{
			Some((_, old_flags)) if old_flags == flags => Ok(()),
			Some((old_identifier, old_flags)) => Err(anyhow!(
				"first occurrence {}",
				old_identifier.location.format()
			)
			.context(identifier.location.format())
			.context(format!(
				"conflicting mutabilities for '{}', {:?} and {:?}",
				identifier.name, old_flags, flags
			))),
			None => Ok(()),
		}
	}

	fn use_variable(
		&self,
		identifier: &Identifier,
		flags: EnumSet<Flag>,
	) -> Result<(), anyhow::Error>
	{
		if let Some((previous_identifier, declared_flags)) =
			self.variables.get(&identifier.resolution_id)
		{
			let missing = flags.difference(*declared_flags);
			if missing.contains(Flag::Mutable)
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
			else if missing.contains(Flag::Addressable)
			{
				return Err(anyhow!(
					"previous declaration {}",
					previous_identifier.location.format()
				)
				.context(identifier.location.format())
				.context(format!(
					"the variable '{}' is not addressable",
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
			Declaration::Constant {
				name,
				value,
				value_type: _,
				flags: _,
			} =>
			{
				analyzer.declare_variable(name, EnumSet::empty())?;
				value.analyze(analyzer)?;
				Ok(())
			}
			Declaration::Function {
				name: _,
				parameters,
				body,
				return_type: _,
				flags: _,
			} =>
			{
				for parameter in parameters
				{
					parameter.analyze(analyzer)?;
				}
				body.analyze(analyzer)?;
				Ok(())
			}
			Declaration::FunctionHead {
				name: _,
				parameters,
				return_type: _,
				flags: _,
			} =>
			{
				for parameter in parameters
				{
					parameter.analyze(analyzer)?;
				}
				Ok(())
			}
		}
	}
}

impl Analyzable for Parameter
{
	fn analyze(&self, analyzer: &mut Analyzer) -> Result<(), anyhow::Error>
	{
		analyzer.declare_variable(&self.name, EnumSet::empty())?;
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
				analyzer.declare_variable(
					name,
					Flag::Mutable | Flag::Addressable,
				)?;
				Ok(())
			}
			Statement::Declaration {
				name,
				value: None,
				value_type: _,
				location: _,
			} =>
			{
				analyzer.declare_variable(
					name,
					Flag::Mutable | Flag::Addressable,
				)?;
				Ok(())
			}
			Statement::Assignment {
				reference,
				value,
				location,
			} =>
			{
				value.analyze(analyzer).with_context(|| location.format())?;

				// TODO if this is a pointer, ignore inner mutation
				analyzer.use_variable(&reference.base, true)?;
				for step in reference.steps.iter()
				{
					match step
					{
						ReferenceStep::Element { argument } =>
						{
							argument.analyze(analyzer)?;
						}
						ReferenceStep::Member { member: _ } => unimplemented!(),
						ReferenceStep::Autoderef => (),
						ReferenceStep::Autodeslice => (),
					}
				}
				Ok(())
			}
			Statement::MethodCall { name: _, arguments } =>
			{
				for argument in arguments
				{
					argument.analyze(analyzer)?;
				}
				Ok(())
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
			Expression::NakedIntegerLiteral { .. } => Ok(()),
			Expression::BitIntegerLiteral { .. } => Ok(()),
			Expression::ArrayLiteral {
				array,
				element_type: _,
			} => array.analyze(analyzer),
			Expression::StringLiteral(_lit) => Ok(()),
			Expression::Autocoerce {
				expression,
				coerced_type: _,
			} => expression.analyze(analyzer),
			Expression::Deref {
				reference,
				deref_type: _,
			}
			| Expression::LengthOfArray { reference } =>
			{
				// TODO if this is a pointer, we might mutate
				analyzer.use_variable(&reference.base, false)?;
				for step in reference.steps.iter()
				{
					match step
					{
						ReferenceStep::Element { argument } =>
						{
							argument.analyze(analyzer)?;
						}
						ReferenceStep::Member { member: _ } => unimplemented!(),
						ReferenceStep::Autoderef => (),
						ReferenceStep::Autodeslice => (),
					}
				}
				Ok(())
			}
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
