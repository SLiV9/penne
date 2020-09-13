/**/

use crate::parser::{BinaryOp, Comparison, ComparisonOp, Expression, Literal};
use crate::parser::{Block, Declaration, Statement};

use std::fmt::Write;

pub fn rebuild(
	program: Vec<Declaration>,
	indentation: &Indentation,
) -> Result<String, anyhow::Error>
{
	let mut buffer = String::new();
	for declaration in program
	{
		write!(
			&mut buffer,
			"{}\n{}",
			indentation,
			declaration.rebuild(indentation)?
		)?;
	}
	Ok(buffer)
}

pub struct Indentation
{
	pub value: &'static str,
	pub amount: usize,
}

impl Indentation
{
	fn increased(&self) -> Indentation
	{
		Indentation {
			value: self.value,
			amount: self.amount + 1,
		}
	}
}

impl std::fmt::Display for Indentation
{
	fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result
	{
		write!(f, "{}", self.value.repeat(self.amount))
	}
}

trait Rebuildable
{
	fn rebuild(
		&self,
		indentation: &Indentation,
	) -> Result<String, anyhow::Error>;
}

impl Rebuildable for Declaration
{
	fn rebuild(
		&self,
		indentation: &Indentation,
	) -> Result<String, anyhow::Error>
	{
		match self
		{
			Declaration::Function { name, body } => Ok(format!(
				"{}fn {}()\n{}",
				indentation,
				name,
				body.rebuild(indentation)?
			)),
		}
	}
}

impl Rebuildable for Block
{
	fn rebuild(
		&self,
		indentation: &Indentation,
	) -> Result<String, anyhow::Error>
	{
		let mut buffer = String::new();
		write!(&mut buffer, "{}{{\n", indentation)?;
		for statement in &self.statements
		{
			write!(
				&mut buffer,
				"{}",
				statement.rebuild(&indentation.increased())?
			)?;
		}
		if self.value != Expression::Void
		{
			write!(
				&mut buffer,
				"{}{}\n",
				indentation.increased(),
				self.value.rebuild(&indentation.increased())?
			)?;
		}
		write!(&mut buffer, "{}}}\n", indentation)?;
		Ok(buffer)
	}
}

impl Rebuildable for Statement
{
	fn rebuild(
		&self,
		indentation: &Indentation,
	) -> Result<String, anyhow::Error>
	{
		match self
		{
			Statement::Declaration {
				name,
				value: Some(value),
			} => Ok(format!(
				"{}var {} = {};\n",
				indentation,
				name,
				value.rebuild(&indentation.increased())?
			)),
			Statement::Declaration { name, value: None } =>
			{
				Ok(format!("{}var {};\n", indentation, name))
			}
			Statement::Assignment { name, value } => Ok(format!(
				"{}{} = {};\n",
				indentation,
				name,
				value.rebuild(&indentation.increased())?
			)),
			Statement::Loop => Ok(format!("{}loop;\n", indentation)),
			Statement::Goto { label } =>
			{
				Ok(format!("{}goto {};\n", indentation, label))
			}
			Statement::Label { label } =>
			{
				Ok(format!("{}{}:\n", indentation, label))
			}
			Statement::If {
				condition,
				then_branch,
				else_branch: Some(else_branch),
			} => Ok(format!(
				"{}if {}\n{}{}else\n{}",
				indentation,
				condition.rebuild(&indentation.increased())?,
				then_branch.rebuild(&indentation.increased())?,
				indentation,
				else_branch.rebuild(&indentation.increased())?
			)),
			Statement::If {
				condition,
				then_branch,
				else_branch: None,
			} => Ok(format!(
				"{}if {}\n{}",
				indentation,
				condition.rebuild(&indentation.increased())?,
				then_branch.rebuild(&indentation.increased())?,
			)),
			Statement::Block(block) => block.rebuild(indentation),
		}
	}
}

impl Rebuildable for Comparison
{
	fn rebuild(
		&self,
		indentation: &Indentation,
	) -> Result<String, anyhow::Error>
	{
		match self.op
		{
			ComparisonOp::Equals => Ok(format!(
				"{} == {}",
				self.left.rebuild(&indentation.increased())?,
				self.right.rebuild(&indentation.increased())?
			)),
		}
	}
}

impl Rebuildable for Expression
{
	fn rebuild(
		&self,
		indentation: &Indentation,
	) -> Result<String, anyhow::Error>
	{
		match self
		{
			Expression::Binary { op, left, right } => match op
			{
				BinaryOp::Add => Ok(format!(
					"{} + {}",
					left.rebuild(&indentation.increased())?,
					right.rebuild(&indentation.increased())?
				)),
				BinaryOp::Subtract => Ok(format!(
					"{} - {}",
					left.rebuild(&indentation.increased())?,
					right.rebuild(&indentation.increased())?
				)),
			},
			Expression::Literal(literal) => literal.rebuild(indentation),
			Expression::Variable(var) => Ok(var.to_string()),
			Expression::Void => Ok("".to_string()),
		}
	}
}

impl Rebuildable for Literal
{
	fn rebuild(
		&self,
		_indentation: &Indentation,
	) -> Result<String, anyhow::Error>
	{
		match self
		{
			Literal::Int32(value) => Ok(format!("{}", value)),
			Literal::Bool(true) => Ok("true".to_string()),
			Literal::Bool(false) => Ok("false".to_string()),
		}
	}
}