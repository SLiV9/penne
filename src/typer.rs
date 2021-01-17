/**/

use crate::common::*;

use anyhow::anyhow;

pub fn analyze(
	program: Vec<Declaration>,
) -> Result<Vec<Declaration>, anyhow::Error>
{
	let mut typer = Typer {
		symbols: std::collections::HashMap::new(),
	};
	program.iter().map(|x| x.analyze(&mut typer)).collect()
}

struct Typer
{
	symbols: std::collections::HashMap<String, (Identifier, ValueType)>,
}

impl Typer
{
	fn put_symbol(
		&mut self,
		identifier: &Identifier,
		value_type: Option<ValueType>,
	) -> Result<(), anyhow::Error>
	{
		if let Some(vt) = value_type
		{
			let old_value = self
				.symbols
				.insert(identifier.name.to_string(), (identifier.clone(), vt));
			match old_value
			{
				Some((_, ot)) if ot == vt => Ok(()),
				Some((old_identifier, old_type)) => Err(anyhow!(
					"first occurrence {}",
					old_identifier.location.format()
				)
				.context(identifier.location.format())
				.context(format!(
					"conflicting types for '{}', {:?} and {:?}",
					identifier.name, old_type, vt
				))),
				None => Ok(()),
			}
		}
		else
		{
			Ok(())
		}
	}

	fn get_symbol(&self, name: &Identifier) -> Option<ValueType>
	{
		match self.symbols.get(&name.name)
		{
			Some((_, vt)) => Some(*vt),
			None => None,
		}
	}
}

pub trait Typed
{
	fn value_type(&self) -> Option<ValueType>;
}

impl Typed for Literal
{
	fn value_type(&self) -> Option<ValueType>
	{
		match self
		{
			Literal::Int32(_) => Some(ValueType::Int32),
			Literal::Bool(_) => Some(ValueType::Bool),
		}
	}
}

trait Analyzable
{
	type Item;

	fn analyze(&self, typer: &mut Typer) -> Result<Self::Item, anyhow::Error>;
}

impl Typed for Declaration
{
	fn value_type(&self) -> Option<ValueType>
	{
		match self
		{
			Declaration::Function { return_type, .. } => *return_type,
		}
	}
}

impl Analyzable for Declaration
{
	type Item = Declaration;

	fn analyze(&self, typer: &mut Typer) -> Result<Self::Item, anyhow::Error>
	{
		match self
		{
			Declaration::Function {
				name,
				parameters,
				body,
				return_type,
			} =>
			{
				typer.put_symbol(name, *return_type)?;

				// Pre-analyze the function body because it might contain
				// untyped declarations, e.g. "var x;", whose types won't be
				// determined in the first pass.
				body.analyze(typer)?;

				let parameters: Result<Vec<Parameter>, anyhow::Error> =
					parameters.iter().map(|x| x.analyze(typer)).collect();
				let parameters = parameters?;
				let body = body.analyze(typer)?;
				let return_type = body.value_type();
				typer.put_symbol(name, return_type)?;
				let function = Declaration::Function {
					name: name.clone(),
					parameters,
					body,
					return_type,
				};
				Ok(function)
			}
		}
	}
}

impl Analyzable for Parameter
{
	type Item = Parameter;

	fn analyze(&self, typer: &mut Typer) -> Result<Self::Item, anyhow::Error>
	{
		typer.put_symbol(&self.name, self.value_type)?;
		let value_type = typer.get_symbol(&self.name);
		Ok(Parameter {
			name: self.name.clone(),
			value_type,
		})
	}
}

impl Typed for FunctionBody
{
	fn value_type(&self) -> Option<ValueType>
	{
		match &self.return_value
		{
			Some(value) => value.value_type(),
			None => None,
		}
	}
}

impl Analyzable for FunctionBody
{
	type Item = FunctionBody;

	fn analyze(&self, typer: &mut Typer) -> Result<Self::Item, anyhow::Error>
	{
		let statements: Result<Vec<Statement>, anyhow::Error> =
			self.statements.iter().map(|x| x.analyze(typer)).collect();
		let statements = statements?;
		let return_value = match &self.return_value
		{
			Some(value) =>
			{
				let value = value.analyze(typer)?;
				Some(value)
			}
			None => None,
		};
		Ok(FunctionBody {
			statements,
			return_value,
		})
	}
}

impl Analyzable for Block
{
	type Item = Block;

	fn analyze(&self, typer: &mut Typer) -> Result<Self::Item, anyhow::Error>
	{
		let statements: Result<Vec<Statement>, anyhow::Error> =
			self.statements.iter().map(|x| x.analyze(typer)).collect();
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

	fn analyze(&self, typer: &mut Typer) -> Result<Self::Item, anyhow::Error>
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
				typer.put_symbol(name, *value_type)?;
				let value = value.analyze(typer)?;
				let value_type = value.value_type();
				typer.put_symbol(name, value_type)?;
				let stmt = Statement::Declaration {
					name: name.clone(),
					value: Some(value),
					value_type,
					location: location.clone(),
				};
				Ok(stmt)
			}
			Statement::Declaration {
				name,
				value: None,
				value_type: Some(value_type),
				location,
			} =>
			{
				typer.put_symbol(name, Some(*value_type))?;
				let stmt = Statement::Declaration {
					name: name.clone(),
					value: None,
					value_type: Some(*value_type),
					location: location.clone(),
				};
				Ok(stmt)
			}
			Statement::Declaration {
				name,
				value: None,
				value_type: None,
				location,
			} =>
			{
				let value_type = typer.get_symbol(name);
				let stmt = Statement::Declaration {
					name: name.clone(),
					value: None,
					value_type,
					location: location.clone(),
				};
				Ok(stmt)
			}
			Statement::Assignment {
				name,
				value,
				location,
			} =>
			{
				let value = value.analyze(typer)?;
				let value_type = value.value_type();
				typer.put_symbol(name, value_type)?;
				let stmt = Statement::Assignment {
					name: name.clone(),
					value,
					location: location.clone(),
				};
				Ok(stmt)
			}
			Statement::Loop { location } => Ok(Statement::Loop {
				location: location.clone(),
			}),
			Statement::Goto { label, location } => Ok(Statement::Goto {
				label: label.clone(),
				location: location.clone(),
			}),
			Statement::Label { label, location } => Ok(Statement::Label {
				label: label.clone(),
				location: location.clone(),
			}),
			Statement::If {
				condition,
				then_branch,
				else_branch,
				location,
			} =>
			{
				let condition = condition.analyze(typer)?;
				let then_branch = {
					let branch = then_branch.analyze(typer)?;
					Box::new(branch)
				};
				let else_branch = if let Some(else_branch) = else_branch
				{
					let branch = else_branch.analyze(typer)?;
					Some(Box::new(branch))
				}
				else
				{
					None
				};
				let stmt = Statement::If {
					condition,
					then_branch,
					else_branch,
					location: location.clone(),
				};
				Ok(stmt)
			}
			Statement::Block(block) =>
			{
				let block = block.analyze(typer)?;
				Ok(Statement::Block(block))
			}
		}
	}
}

impl Analyzable for Comparison
{
	type Item = Comparison;

	fn analyze(&self, typer: &mut Typer) -> Result<Self::Item, anyhow::Error>
	{
		let left = self.left.analyze(typer)?;
		let right = self.right.analyze(typer)?;
		let expr = Comparison {
			op: self.op,
			left,
			right,
			location: self.location.clone(),
		};
		Ok(expr)
	}
}

impl Typed for Expression
{
	fn value_type(&self) -> Option<ValueType>
	{
		match self
		{
			Expression::Binary { left, .. } => left.value_type(),
			Expression::Literal(literal) => literal.value_type(),
			Expression::Variable { value_type, .. } => *value_type,
			Expression::FunctionCall { return_type, .. } => *return_type,
		}
	}
}

impl Analyzable for Expression
{
	type Item = Expression;

	fn analyze(&self, typer: &mut Typer) -> Result<Self::Item, anyhow::Error>
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
				let left = left.analyze(typer)?;
				let right = right.analyze(typer)?;
				let expr = Expression::Binary {
					op: *op,
					left: Box::new(left),
					right: Box::new(right),
					location: location.clone(),
				};
				Ok(expr)
			}
			Expression::Literal(lit) => Ok(Expression::Literal(lit.clone())),
			Expression::Variable {
				name,
				value_type: Some(value_type),
			} =>
			{
				typer.put_symbol(name, Some(*value_type))?;
				let expr = Expression::Variable {
					name: name.clone(),
					value_type: Some(*value_type),
				};
				Ok(expr)
			}
			Expression::Variable {
				name,
				value_type: None,
			} =>
			{
				let value_type = typer.get_symbol(name);
				let expr = Expression::Variable {
					name: name.clone(),
					value_type,
				};
				Ok(expr)
			}
			Expression::FunctionCall {
				name,
				arguments,
				return_type,
			} =>
			{
				typer.put_symbol(name, *return_type)?;
				let return_type = typer.get_symbol(name);
				let arguments: Result<Vec<Expression>, anyhow::Error> =
					arguments.iter().map(|a| a.analyze(typer)).collect();
				let arguments = arguments?;
				let expr = Expression::FunctionCall {
					name: name.clone(),
					arguments,
					return_type,
				};
				Ok(expr)
			}
		}
	}
}
