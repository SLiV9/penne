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
			Declaration::Constant {
				name,
				value,
				value_type: _,
				flags: _,
			} =>
			{
				analyzer.declare_variable(name, false)?;
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
			Declaration::PreprocessorDirective { .. } => unreachable!(),
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
				value_type,
				location,
			} =>
			{
				value.analyze(analyzer).with_context(|| location.format())?;
				let is_mutable = match value_type
				{
					Some(ValueType::View { .. }) => false,
					Some(_) => true,
					None => false,
				};
				analyzer.declare_variable(name, is_mutable)?;
				Ok(())
			}
			Statement::Declaration {
				name,
				value: None,
				value_type,
				location: _,
			} =>
			{
				let is_mutable = match value_type
				{
					Some(ValueType::View { .. }) => false,
					Some(_) => true,
					None => false,
				};
				analyzer.declare_variable(name, is_mutable)?;
				Ok(())
			}
			Statement::Assignment {
				reference,
				value,
				location,
			} =>
			{
				value.analyze(analyzer).with_context(|| location.format())?;

				let mut needs_outer_mutability = true;
				for step in reference.steps.iter()
				{
					match step
					{
						ReferenceStep::Element { argument } =>
						{
							argument.analyze(analyzer)?;
						}
						ReferenceStep::Member { .. } => (),
						ReferenceStep::Autodeslice { .. } => (),
						ReferenceStep::Autoderef =>
						{
							needs_outer_mutability = false;
						}
						ReferenceStep::Autoview => (),
					}
				}

				analyzer
					.use_variable(&reference.base, needs_outer_mutability)?;
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
			Expression::Unary {
				op: _,
				expression,
				location,
			} =>
			{
				expression
					.analyze(analyzer)
					.with_context(|| location.format())?;
				Ok(())
			}
			Expression::PrimitiveLiteral(_lit) => Ok(()),
			Expression::NakedIntegerLiteral { .. } => Ok(()),
			Expression::BitIntegerLiteral { .. } => Ok(()),
			Expression::ArrayLiteral {
				array,
				element_type: _,
			} => array.analyze(analyzer),
			Expression::StringLiteral { .. } => Ok(()),
			Expression::Autocoerce {
				expression,
				coerced_type: _,
			} => expression.analyze(analyzer),
			Expression::PrimitiveCast {
				expression,
				coerced_type: _,
				location: _,
			} => expression.analyze(analyzer),
			Expression::Deref {
				reference,
				deref_type: _,
			} =>
			{
				let is_addressed = match reference.address_depth
				{
					0 => false,
					1 => true,
					_ =>
					{
						return Err(anyhow!("address depth too high")
							.context(reference.location.format())
							.context(format!(
							"cannot take address of temporary address of '{}'",
							reference.base.name
						)));
					}
				};
				analyzer.use_variable(&reference.base, is_addressed)?;
				for step in reference.steps.iter()
				{
					match step
					{
						ReferenceStep::Element { argument } =>
						{
							argument.analyze(analyzer)?;
						}
						ReferenceStep::Member { .. } => (),
						ReferenceStep::Autodeslice { .. } => (),
						ReferenceStep::Autoderef => (),
						ReferenceStep::Autoview => (),
					}
				}
				Ok(())
			}
			Expression::LengthOfArray { reference } =>
			{
				analyzer.use_variable(&reference.base, false)?;
				for step in reference.steps.iter()
				{
					match step
					{
						ReferenceStep::Element { argument } =>
						{
							argument.analyze(analyzer)?;
						}
						ReferenceStep::Member { .. } => (),
						ReferenceStep::Autodeslice { .. } => (),
						ReferenceStep::Autoderef => (),
						ReferenceStep::Autoview => (),
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
