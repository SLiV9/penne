/**/

pub use crate::parser::{BinaryOp, ComparisonOp, Literal};

use crate::parser;

use anyhow::anyhow;

pub fn analyze(
	program: Vec<parser::Declaration>,
) -> Result<Vec<Declaration>, anyhow::Error>
{
	let mut typer = Typer {
		symbols: std::collections::HashMap::new(),
	};
	program.iter().map(|x| x.analyze(&mut typer)).collect()
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ValueType
{
	Int32,
	Bool,
}

struct Typer
{
	symbols: std::collections::HashMap<String, ValueType>,
}

impl Typer
{
	fn put_symbol(
		&mut self,
		name: &str,
		value_type: Option<ValueType>,
	) -> Result<(), anyhow::Error>
	{
		if let Some(vt) = value_type
		{
			let old_type = self.symbols.insert(name.to_string(), vt);
			match old_type
			{
				Some(ot) if ot == vt => Ok(()),
				Some(ot) => Err(anyhow!(
					"conflicting types for '{}', {:?} and {:?}",
					name,
					ot,
					vt
				)),
				None => Ok(()),
			}
		}
		else
		{
			Ok(())
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

#[derive(Debug, PartialEq, Eq)]
pub enum Declaration
{
	Function
	{
		name: String,
		//parameters: Vec<Parameter>,
		body: FunctionBody,
		return_type: Option<ValueType>,
	},
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

impl Analyzable for parser::Declaration
{
	type Item = Declaration;

	fn analyze(&self, typer: &mut Typer) -> Result<Self::Item, anyhow::Error>
	{
		match self
		{
			parser::Declaration::Function { name, body } =>
			{
				// Pre-analyze the function body because it might contain
				// untyped declarations, e.g. "var x;", whose types won't be
				// determined in the first pass.
				body.analyze(typer)?;
				let body = body.analyze(typer)?;
				let return_type = body.value_type();
				let function = Declaration::Function {
					name: name.clone(),
					body,
					return_type,
				};
				Ok(function)
			}
		}
	}
}

#[derive(Debug, PartialEq, Eq)]
pub struct FunctionBody
{
	pub statements: Vec<Statement>,
	pub return_value: Option<Expression>,
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

impl Analyzable for parser::FunctionBody
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

#[derive(Debug, PartialEq, Eq)]
pub struct Block
{
	pub statements: Vec<Statement>,
}

impl Analyzable for parser::Block
{
	type Item = Block;

	fn analyze(&self, typer: &mut Typer) -> Result<Self::Item, anyhow::Error>
	{
		let statements: Result<Vec<Statement>, anyhow::Error> =
			self.statements.iter().map(|x| x.analyze(typer)).collect();
		let statements = statements?;
		Ok(Block { statements })
	}
}

#[derive(Debug, PartialEq, Eq)]
pub enum Statement
{
	Declaration
	{
		name: String,
		value: Option<Expression>,
		value_type: Option<ValueType>,
	},
	Assignment
	{
		name: String,
		value: Expression,
	},
	Loop,
	Goto
	{
		label: String,
	},
	Label
	{
		label: String,
	},
	If
	{
		condition: Comparison,
		then_branch: Box<Statement>,
		else_branch: Option<Box<Statement>>,
	},
	Block(Block),
}

impl Analyzable for parser::Statement
{
	type Item = Statement;

	fn analyze(&self, typer: &mut Typer) -> Result<Self::Item, anyhow::Error>
	{
		match self
		{
			parser::Statement::Declaration {
				name,
				value: Some(value),
			} =>
			{
				let value = value.analyze(typer)?;
				let value_type = value.value_type();
				typer.put_symbol(name, value_type)?;
				let stmt = Statement::Declaration {
					name: name.clone(),
					value: Some(value),
					value_type,
				};
				Ok(stmt)
			}
			parser::Statement::Declaration { name, value: None } =>
			{
				let value_type = typer.symbols.get(name).cloned();
				let stmt = Statement::Declaration {
					name: name.clone(),
					value: None,
					value_type,
				};
				Ok(stmt)
			}
			parser::Statement::Assignment { name, value } =>
			{
				let value = value.analyze(typer)?;
				let value_type = value.value_type();
				typer.put_symbol(name, value_type)?;
				let stmt = Statement::Assignment {
					name: name.clone(),
					value,
				};
				Ok(stmt)
			}
			parser::Statement::Loop => Ok(Statement::Loop),
			parser::Statement::Goto { label } => Ok(Statement::Goto {
				label: label.clone(),
			}),
			parser::Statement::Label { label } => Ok(Statement::Label {
				label: label.clone(),
			}),
			parser::Statement::If {
				condition,
				then_branch,
				else_branch,
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
				};
				Ok(stmt)
			}
			parser::Statement::Block(block) =>
			{
				let block = block.analyze(typer)?;
				Ok(Statement::Block(block))
			}
		}
	}
}

#[derive(Debug, PartialEq, Eq)]
pub struct Comparison
{
	pub op: ComparisonOp,
	pub left: Expression,
	pub right: Expression,
}

impl Analyzable for parser::Comparison
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
		};
		Ok(expr)
	}
}

#[derive(Debug, PartialEq, Eq)]
pub enum Expression
{
	Binary
	{
		op: BinaryOp,
		left: Box<Expression>,
		right: Box<Expression>,
	},
	Literal(Literal),
	Variable
	{
		name: String,
		value_type: Option<ValueType>,
	},
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
		}
	}
}

impl Analyzable for parser::Expression
{
	type Item = Expression;

	fn analyze(&self, typer: &mut Typer) -> Result<Self::Item, anyhow::Error>
	{
		match self
		{
			parser::Expression::Binary { op, left, right } =>
			{
				let left = left.analyze(typer)?;
				let right = right.analyze(typer)?;
				let expr = Expression::Binary {
					op: *op,
					left: Box::new(left),
					right: Box::new(right),
				};
				Ok(expr)
			}
			parser::Expression::Literal(lit) =>
			{
				Ok(Expression::Literal(lit.clone()))
			}
			parser::Expression::Variable(name) =>
			{
				let value_type = typer.symbols.get(name).cloned();
				let expr = Expression::Variable {
					name: name.clone(),
					value_type,
				};
				Ok(expr)
			}
		}
	}
}
