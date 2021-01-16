/**/

pub use crate::lexer::Location;
pub use crate::parser::{BinaryOp, ComparisonOp, Identifier, Literal};

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

#[derive(Debug)]
pub enum Declaration
{
	Function
	{
		name: Identifier,
		parameters: Vec<Parameter>,
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
			parser::Declaration::Function {
				name,
				parameters,
				body,
			} =>
			{
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

#[derive(Debug)]
pub struct Parameter
{
	pub name: Identifier,
	pub value_type: Option<ValueType>,
}

impl Analyzable for parser::Parameter
{
	type Item = Parameter;

	fn analyze(&self, typer: &mut Typer) -> Result<Self::Item, anyhow::Error>
	{
		let value_type = typer.get_symbol(&self.name);
		Ok(Parameter {
			name: self.name.clone(),
			value_type,
		})
	}
}

#[derive(Debug)]
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

#[derive(Debug)]
pub struct Block
{
	pub statements: Vec<Statement>,
	pub location: Location,
}

impl Analyzable for parser::Block
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

#[derive(Debug)]
pub enum Statement
{
	Declaration
	{
		name: Identifier,
		value: Option<Expression>,
		value_type: Option<ValueType>,
		location: Location,
	},
	Assignment
	{
		name: Identifier,
		value: Expression,
		location: Location,
	},
	Loop
	{
		location: Location,
	},
	Goto
	{
		label: Identifier,
		location: Location,
	},
	Label
	{
		label: Identifier,
		location: Location,
	},
	If
	{
		condition: Comparison,
		then_branch: Box<Statement>,
		else_branch: Option<Box<Statement>>,
		location: Location,
	},
	Block(Block),
}

impl Statement
{
	pub fn location(&self) -> &Location
	{
		match self
		{
			Statement::Declaration { location, .. } => location,
			Statement::Assignment { location, .. } => location,
			Statement::Loop { location } => location,
			Statement::Goto { location, .. } => location,
			Statement::Label { location, .. } => location,
			Statement::If { location, .. } => location,
			Statement::Block(block) => &block.location,
		}
	}
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
				location,
			} =>
			{
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
			parser::Statement::Declaration {
				name,
				value: None,
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
			parser::Statement::Assignment {
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
			parser::Statement::Loop { location } => Ok(Statement::Loop {
				location: location.clone(),
			}),
			parser::Statement::Goto { label, location } =>
			{
				Ok(Statement::Goto {
					label: label.clone(),
					location: location.clone(),
				})
			}
			parser::Statement::Label { label, location } =>
			{
				Ok(Statement::Label {
					label: label.clone(),
					location: location.clone(),
				})
			}
			parser::Statement::If {
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
			parser::Statement::Block(block) =>
			{
				let block = block.analyze(typer)?;
				Ok(Statement::Block(block))
			}
		}
	}
}

#[derive(Debug)]
pub struct Comparison
{
	pub op: ComparisonOp,
	pub left: Expression,
	pub right: Expression,
	pub location: Location,
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
			location: self.location.clone(),
		};
		Ok(expr)
	}
}

#[derive(Debug)]
pub enum Expression
{
	Binary
	{
		op: BinaryOp,
		left: Box<Expression>,
		right: Box<Expression>,
		location: Location,
	},
	Literal(Literal),
	Variable
	{
		name: Identifier,
		value_type: Option<ValueType>,
	},
	FunctionCall
	{
		name: Identifier,
		arguments: Vec<Expression>,
		return_type: Option<ValueType>,
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
			Expression::FunctionCall { return_type, .. } => *return_type,
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
			parser::Expression::Binary {
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
			parser::Expression::Literal(lit) =>
			{
				Ok(Expression::Literal(lit.clone()))
			}
			parser::Expression::Variable(name) =>
			{
				let value_type = typer.get_symbol(name);
				let expr = Expression::Variable {
					name: name.clone(),
					value_type,
				};
				Ok(expr)
			}
			parser::Expression::FunctionCall { name, arguments } =>
			{
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
