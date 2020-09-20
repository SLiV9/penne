/**/

pub use crate::parser::{BinaryOp, ComparisonOp, Literal};

use crate::parser;

use anyhow::anyhow;

pub fn analyze(
	program: Vec<parser::Declaration>,
) -> Result<Vec<Declaration>, anyhow::Error>
{
	let mut analyzer = Analyzer {
		symbols: std::collections::HashMap::new(),
	};
	program.iter().map(|x| x.analyze(&mut analyzer)).collect()
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ValueType
{
	Int32,
	Bool,
}

struct Analyzer
{
	symbols: std::collections::HashMap<String, ValueType>,
}

impl Analyzer
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

	fn analyze(
		&self,
		analyzer: &mut Analyzer,
	) -> Result<Self::Item, anyhow::Error>;
}

#[derive(Debug, PartialEq, Eq)]
pub enum Declaration
{
	Function
	{
		name: String,
		//parameters: Vec<Parameter>,
		body: Block,
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

	fn analyze(
		&self,
		analyzer: &mut Analyzer,
	) -> Result<Self::Item, anyhow::Error>
	{
		match self
		{
			parser::Declaration::Function { name, body } =>
			{
				// Pre-analyze the function body because it might contain
				// untyped declarations, e.g. "var x;", whose types won't be
				// determined in the first pass.
				body.analyze(analyzer)?;
				let body = body.analyze(analyzer)?;
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
pub struct Block
{
	pub statements: Vec<Statement>,
	pub value: Expression,
}

impl Typed for Block
{
	fn value_type(&self) -> Option<ValueType>
	{
		self.value.value_type()
	}
}

impl Analyzable for parser::Block
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
		let value = self.value.analyze(analyzer)?;
		Ok(Block { statements, value })
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

impl Typed for Statement
{
	fn value_type(&self) -> Option<ValueType>
	{
		None
	}
}

impl Analyzable for parser::Statement
{
	type Item = Statement;

	fn analyze(
		&self,
		analyzer: &mut Analyzer,
	) -> Result<Self::Item, anyhow::Error>
	{
		match self
		{
			parser::Statement::Declaration {
				name,
				value: Some(value),
			} =>
			{
				let value = value.analyze(analyzer)?;
				let value_type = value.value_type();
				analyzer.put_symbol(name, value_type)?;
				let stmt = Statement::Declaration {
					name: name.clone(),
					value: Some(value),
					value_type,
				};
				Ok(stmt)
			}
			parser::Statement::Declaration { name, value: None } =>
			{
				let value_type = analyzer.symbols.get(name).cloned();
				let stmt = Statement::Declaration {
					name: name.clone(),
					value: None,
					value_type,
				};
				Ok(stmt)
			}
			parser::Statement::Assignment { name, value } =>
			{
				let value = value.analyze(analyzer)?;
				let value_type = value.value_type();
				analyzer.put_symbol(name, value_type)?;
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
				let condition = condition.analyze(analyzer)?;
				let then_branch = {
					let branch = then_branch.analyze(analyzer)?;
					Box::new(branch)
				};
				let else_branch = if let Some(else_branch) = else_branch
				{
					let branch = else_branch.analyze(analyzer)?;
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
				let block = block.analyze(analyzer)?;
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

	fn analyze(
		&self,
		analyzer: &mut Analyzer,
	) -> Result<Self::Item, anyhow::Error>
	{
		let left = self.left.analyze(analyzer)?;
		let right = self.right.analyze(analyzer)?;
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
	Void,
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
			Expression::Void => None,
		}
	}
}

impl Analyzable for parser::Expression
{
	type Item = Expression;

	fn analyze(
		&self,
		analyzer: &mut Analyzer,
	) -> Result<Self::Item, anyhow::Error>
	{
		match self
		{
			parser::Expression::Binary { op, left, right } =>
			{
				let left = left.analyze(analyzer)?;
				let right = right.analyze(analyzer)?;
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
				let value_type = analyzer.symbols.get(name).cloned();
				let expr = Expression::Variable {
					name: name.clone(),
					value_type,
				};
				Ok(expr)
			}
			parser::Expression::Void => Ok(Expression::Void),
		}
	}
}
