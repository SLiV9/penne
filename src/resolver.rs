/**/

use crate::common::*;
use crate::typer::Typed;

use anyhow::anyhow;
use anyhow::Context;

pub fn analyze(
	program: Vec<Declaration>,
) -> Result<Vec<Declaration>, anyhow::Error>
{
	let mut analyzer = Analyzer {};
	program.iter().map(|x| x.analyze(&mut analyzer)).collect()
}

struct Analyzer {}

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
	) -> Result<Self::Item, anyhow::Error>
	{
		match self
		{
			Declaration::Constant {
				name,
				value,
				value_type,
				flags,
			} =>
			{
				let value = value.analyze(analyzer)?;
				let declaration = Declaration::Constant {
					name: name.clone(),
					value,
					value_type: value_type.clone(),
					flags: *flags,
				};
				Ok(declaration)
			}
			Declaration::Function {
				name,
				parameters,
				body,
				return_type,
				flags,
			} =>
			{
				let parameters: Result<Vec<Parameter>, anyhow::Error> =
					parameters.iter().map(|x| x.analyze(analyzer)).collect();
				let parameters = parameters?;
				let body = body.analyze(analyzer)?;

				let function = Declaration::Function {
					name: name.clone(),
					parameters,
					body,
					return_type: return_type.clone(),
					flags: *flags,
				};
				Ok(function)
			}
			Declaration::FunctionHead {
				name,
				parameters,
				return_type,
				flags,
			} =>
			{
				let parameters: Result<Vec<Parameter>, anyhow::Error> =
					parameters.iter().map(|x| x.analyze(analyzer)).collect();
				let parameters = parameters?;

				let function = Declaration::FunctionHead {
					name: name.clone(),
					parameters,
					return_type: return_type.clone(),
					flags: *flags,
				};
				Ok(function)
			}
			Declaration::PreprocessorDirective { .. } => unreachable!(),
		}
	}
}

impl Analyzable for Parameter
{
	type Item = Parameter;

	fn analyze(
		&self,
		_analyzer: &mut Analyzer,
	) -> Result<Self::Item, anyhow::Error>
	{
		if self.value_type.is_some()
		{
			Ok(Parameter {
				name: self.name.clone(),
				value_type: self.value_type.clone(),
			})
		}
		else
		{
			Err(anyhow!("failed to infer type")
				.context(self.name.location.format())
				.context(format!(
					"failed to infer type for '{}'",
					self.name.name
				)))
		}
	}
}

impl Analyzable for FunctionBody
{
	type Item = FunctionBody;

	fn analyze(
		&self,
		analyzer: &mut Analyzer,
	) -> Result<Self::Item, anyhow::Error>
	{
		let statements: Result<Vec<Statement>, anyhow::Error> = self
			.statements
			.iter()
			.map(|x| x.analyze(analyzer))
			.collect();
		let statements = statements?;
		let return_value = self
			.return_value
			.as_ref()
			.map(|v| v.analyze(analyzer))
			.transpose()?;

		Ok(FunctionBody {
			statements,
			return_value,
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
		let statements: Result<Vec<Statement>, anyhow::Error> = self
			.statements
			.iter()
			.map(|x| x.analyze(analyzer))
			.collect();
		let statements = statements?;

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
			Statement::Declaration {
				name,
				value: Some(value),
				value_type: Some(vt),
				location,
			} =>
			{
				let value = value
					.analyze(analyzer)
					.with_context(|| location.format())?;
				Ok(Statement::Declaration {
					name: name.clone(),
					value: Some(value),
					value_type: Some(vt.clone()),
					location: location.clone(),
				})
			}
			Statement::Declaration {
				name,
				value: None,
				value_type: Some(vt),
				location,
			} => Ok(Statement::Declaration {
				name: name.clone(),
				value: None,
				value_type: Some(vt.clone()),
				location: location.clone(),
			}),
			Statement::Declaration {
				name,
				value: _,
				value_type: None,
				location,
			} => Err(anyhow!("failed to infer type")
				.context(location.format())
				.context(format!("failed to infer type for '{}'", name.name))),
			Statement::Assignment {
				reference,
				value,
				location,
			} =>
			{
				let value = value
					.analyze(analyzer)
					.with_context(|| location.format())?;
				let reference = reference
					.analyze(analyzer)
					.with_context(|| location.format())?;

				if value.value_type().is_some()
				{
					Ok(Statement::Assignment {
						reference,
						value,
						location: location.clone(),
					})
				}
				else
				{
					Err(anyhow!("failed to infer type")
						.context(reference.location.format())
						.context(format!(
							"failed to infer type for '{}'",
							reference.base.name
						)))
				}
			}
			Statement::MethodCall { name, arguments } =>
			{
				let arguments: Result<Vec<Expression>, anyhow::Error> =
					arguments.iter().map(|a| a.analyze(analyzer)).collect();
				let arguments = arguments?;

				if name.resolution_id > 0
				{
					Ok(Statement::MethodCall {
						name: name.clone(),
						arguments,
					})
				}
				else
				{
					Err(anyhow!("failed to resolve method call")
						.context(name.location.format())
						.context(format!("failed to resolve '{}'", name.name)))
				}
			}
			Statement::Loop { .. } => Ok(self.clone()),
			Statement::Goto { label, location } =>
			{
				if label.resolution_id > 0
				{
					Ok(self.clone())
				}
				else
				{
					Err(anyhow!("failed to resolve method call")
						.context(location.format())
						.context(format!("failed to resolve '{}'", label.name)))
				}
			}
			Statement::Label { .. } => Ok(self.clone()),
			Statement::If {
				condition,
				then_branch,
				else_branch,
				location,
			} =>
			{
				let condition = condition
					.analyze(analyzer)
					.with_context(|| location.format())?;
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
					condition,
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

impl Analyzable for Comparison
{
	type Item = Comparison;

	fn analyze(
		&self,
		analyzer: &mut Analyzer,
	) -> Result<Comparison, anyhow::Error>
	{
		let left = self
			.left
			.analyze(analyzer)
			.with_context(|| self.location.format())?;
		let right = self
			.right
			.analyze(analyzer)
			.with_context(|| self.location.format())?;
		let lvt = left.value_type();
		let rvt = right.value_type();
		if lvt.is_some() && lvt == rvt
		{
			Ok(Comparison {
				op: self.op,
				left,
				right,
				location: self.location.clone(),
			})
		}
		else
		{
			Err(anyhow!("failed to infer type")
				.context(format!("got {:?} and {:?}", lvt, rvt))
				.context(self.location.format())
				.context(format!(
					"failed to infer operand types for comparison operator",
				)))
		}
	}
}

impl Analyzable for Array
{
	type Item = Array;

	fn analyze(&self, analyzer: &mut Analyzer) -> Result<Array, anyhow::Error>
	{
		let elements: Result<Vec<Expression>, anyhow::Error> =
			self.elements.iter().map(|x| x.analyze(analyzer)).collect();
		let elements = elements?;

		Ok(Array {
			elements,
			location: self.location.clone(),
			resolution_id: self.resolution_id,
		})
	}
}

impl Analyzable for Expression
{
	type Item = Expression;

	fn analyze(
		&self,
		analyzer: &mut Analyzer,
	) -> Result<Expression, anyhow::Error>
	{
		match self
		{
			Expression::Binary {
				op,
				left,
				right,
				location,
			} =>
			{
				let left = left
					.analyze(analyzer)
					.with_context(|| location.format())?;
				let right = right
					.analyze(analyzer)
					.with_context(|| location.format())?;
				let lvt = left.value_type();
				let rvt = right.value_type();
				if lvt.is_some() && lvt == rvt
				{
					Ok(Expression::Binary {
						op: *op,
						left: Box::new(left),
						right: Box::new(right),
						location: location.clone(),
					})
				}
				else
				{
					Err(anyhow!("failed to infer type")
						.context(format!("got {:?} and {:?}", lvt, rvt))
						.context(location.format())
						.context(format!(
							"failed to infer operand types for binary operator",
						)))
				}
			}
			Expression::Unary {
				op,
				expression,
				location,
			} =>
			{
				let expr = expression
					.analyze(analyzer)
					.with_context(|| location.format())?;
				Ok(Expression::Unary {
					op: *op,
					expression: Box::new(expr),
					location: location.clone(),
				})
			}
			Expression::PrimitiveLiteral(_lit) => Ok(self.clone()),
			Expression::NakedIntegerLiteral {
				value: _,
				value_type: Some(_),
				location: _,
			} => Ok(self.clone()),
			Expression::NakedIntegerLiteral {
				value: _,
				value_type: None,
				location,
			} => Err(anyhow!("failed to infer type")
				.context(location.format())
				.context(format!("failed to infer integer literal type"))),
			Expression::BitIntegerLiteral {
				value: _,
				value_type: Some(_),
				location: _,
			} => Ok(self.clone()),
			Expression::BitIntegerLiteral {
				value: _,
				value_type: None,
				location,
			} => Err(anyhow!("failed to infer type")
				.context(location.format())
				.context(format!("failed to infer integer literal type"))),
			Expression::ArrayLiteral {
				array,
				element_type,
			} =>
			{
				let array = array.analyze(analyzer)?;
				if element_type.is_some()
				{
					Ok(Expression::ArrayLiteral {
						array,
						element_type: element_type.clone(),
					})
				}
				else
				{
					Err(anyhow!("failed to infer type")
						.context(array.location.format())
						.context(format!(
							"failed to infer array literal element type"
						)))
				}
			}
			Expression::StringLiteral {
				bytes: _,
				value_type: Some(ValueType::String),
				location: _,
			} => Ok(self.clone()),
			Expression::StringLiteral {
				bytes: _,
				value_type: Some(ValueType::Slice { .. }),
				location: _,
			} => Ok(self.clone()),
			Expression::StringLiteral {
				bytes: _,
				value_type: _,
				location,
			} => Err(anyhow!("failed to infer type")
				.context(location.format())
				.context(format!("failed to infer string literal type"))),
			Expression::Deref {
				reference,
				deref_type,
			} =>
			{
				let reference = reference.analyze(analyzer)?;
				if deref_type.is_some()
				{
					Ok(Expression::Deref {
						reference,
						deref_type: deref_type.clone(),
					})
				}
				else
				{
					Err(anyhow!("failed to infer type")
						.context(reference.location.format())
						.context(format!("failed to infer type of reference")))
				}
			}
			Expression::Autocoerce {
				expression,
				coerced_type,
			} =>
			{
				let expression = expression.analyze(analyzer)?;
				if expression.value_type().is_some()
				{
					Ok(Expression::Autocoerce {
						expression: Box::new(expression),
						coerced_type: coerced_type.clone(),
					})
				}
				else
				{
					Err(anyhow!("failed to infer type in coercion")
						.context(format!("failed to infer type in coercion")))
				}
			}
			Expression::LengthOfArray { reference } =>
			{
				let reference = reference.analyze(analyzer)?;
				Ok(Expression::LengthOfArray { reference })
			}
			Expression::FunctionCall {
				name,
				arguments,
				return_type,
			} =>
			{
				let arguments: Result<Vec<Expression>, anyhow::Error> =
					arguments.iter().map(|a| a.analyze(analyzer)).collect();
				let arguments = arguments?;

				if name.resolution_id > 0
				{
					Ok(Expression::FunctionCall {
						name: name.clone(),
						arguments,
						return_type: return_type.clone(),
					})
				}
				else
				{
					Err(anyhow!("failed to resolve function call")
						.context(name.location.format())
						.context(format!("failed to resolve '{}'", name.name)))
				}
			}
		}
	}
}

impl Analyzable for Reference
{
	type Item = Reference;

	fn analyze(
		&self,
		analyzer: &mut Analyzer,
	) -> Result<Self::Item, anyhow::Error>
	{
		let steps: Result<Vec<ReferenceStep>, anyhow::Error> = self
			.steps
			.iter()
			.map(|step| step.analyze(analyzer))
			.collect();
		let steps = steps?;

		if self.base.resolution_id > 0
		{
			Ok(Reference {
				base: self.base.clone(),
				steps,
				address_depth: self.address_depth,
				location: self.location.clone(),
			})
		}
		else
		{
			Err(anyhow!("failed to resolve reference")
				.context(self.location.format())
				.context(format!("failed to resolve '{}'", self.base.name)))
		}
	}
}

impl Analyzable for ReferenceStep
{
	type Item = ReferenceStep;

	fn analyze(
		&self,
		analyzer: &mut Analyzer,
	) -> Result<Self::Item, anyhow::Error>
	{
		match &self
		{
			ReferenceStep::Element { argument } =>
			{
				let argument = argument.analyze(analyzer)?;
				Ok(ReferenceStep::Element {
					argument: Box::new(argument),
				})
			}
			ReferenceStep::Member { member: _ } => Ok(self.clone()),
			ReferenceStep::Autodeslice { .. } => Ok(self.clone()),
			ReferenceStep::Autoderef => Ok(ReferenceStep::Autoderef),
			ReferenceStep::Autoview => Ok(ReferenceStep::Autoview),
		}
	}
}
