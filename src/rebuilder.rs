/**/

use crate::common::*;

use std::fmt::Write;

pub fn rebuild(
	program: &Vec<Declaration>,
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
			Declaration::Function {
				name,
				parameters,
				body,
				return_type,
			} =>
			{
				let mut buffer = String::new();
				write!(&mut buffer, "{}fn {}(", indentation, name.name)?;
				match parameters.split_first()
				{
					Some((first, others)) =>
					{
						write!(
							&mut buffer,
							"{}",
							first.rebuild(&indentation.increased())?
						)?;
						for parameter in others
						{
							write!(
								&mut buffer,
								", {}",
								parameter.rebuild(&indentation.increased())?
							)?;
						}
					}
					None => (),
				}
				write!(&mut buffer, ")")?;
				if let Some(value_type) = return_type
				{
					write!(
						&mut buffer,
						" -> {}",
						value_type.rebuild(&indentation.increased())?
					)?;
				}
				write!(&mut buffer, "\n{}", body.rebuild(indentation)?)?;
				Ok(buffer)
			}
		}
	}
}

impl Rebuildable for Parameter
{
	fn rebuild(
		&self,
		indentation: &Indentation,
	) -> Result<String, anyhow::Error>
	{
		let mut buffer = self.name.name.to_string();
		if let Some(value_type) = self.value_type
		{
			write!(
				&mut buffer,
				": {}",
				value_type.rebuild(&indentation.increased())?
			)?;
		}
		Ok(buffer)
	}
}

impl Rebuildable for FunctionBody
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
		if let Some(value) = &self.return_value
		{
			write!(
				&mut buffer,
				"{}{}\n",
				indentation.increased(),
				value.rebuild(&indentation.increased())?
			)?;
		}
		write!(&mut buffer, "{}}}\n", indentation)?;
		Ok(buffer)
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
				value_type: Some(value_type),
				location: _,
			} => Ok(format!(
				"{}var {}: {} = {};\n",
				indentation,
				name.name,
				value_type.rebuild(&indentation.increased())?,
				value.rebuild(&indentation.increased())?
			)),
			Statement::Declaration {
				name,
				value: None,
				value_type: Some(value_type),
				location: _,
			} => Ok(format!(
				"{}var {}: {};\n",
				indentation,
				name.name,
				value_type.rebuild(&indentation.increased())?,
			)),
			Statement::Declaration {
				name,
				value: Some(value),
				value_type: None,
				location: _,
			} => Ok(format!(
				"{}var {} = {};\n",
				indentation,
				name.name,
				value.rebuild(&indentation.increased())?
			)),
			Statement::Declaration {
				name,
				value: None,
				value_type: None,
				location: _,
			} => Ok(format!("{}var {};\n", indentation, name.name)),
			Statement::Assignment {
				name,
				value,
				location: _,
			} => Ok(format!(
				"{}{} = {};\n",
				indentation,
				name.name,
				value.rebuild(&indentation.increased())?
			)),
			Statement::Loop { location: _ } =>
			{
				Ok(format!("{}loop;\n", indentation))
			}
			Statement::Goto { label, location: _ } =>
			{
				Ok(format!("{}goto {};\n", indentation, label.name))
			}
			Statement::Label { label, location: _ } =>
			{
				Ok(format!("{}{}:\n", indentation, label.name))
			}
			Statement::If {
				condition,
				then_branch,
				else_branch: Some(else_branch),
				location: _,
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
				location: _,
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

impl Rebuildable for Array
{
	fn rebuild(
		&self,
		indentation: &Indentation,
	) -> Result<String, anyhow::Error>
	{
		let mut buffer = String::new();
		write!(&mut buffer, "{}[\n", indentation)?;
		for element in &self.elements
		{
			write!(
				&mut buffer,
				"{}",
				element.rebuild(&indentation.increased())?
			)?;
		}
		write!(&mut buffer, "{}]\n", indentation)?;
		Ok(buffer)
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
			Expression::Binary {
				op,
				left,
				right,
				location: _,
			} => match op
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
			Expression::PrimitiveLiteral(literal) =>
			{
				literal.rebuild(indentation)
			}
			Expression::ArrayLiteral {
				array,
				element_type: _,
			} => array.rebuild(indentation),
			Expression::StringLiteral(value) =>
			{
				// TODO escape \xNN and \uNNNNN properly instead of \u{NNNNN}
				Ok(format!("\"{}\"", value.escape_default()))
			}
			Expression::Variable {
				name: var,
				value_type: _,
			} => Ok(var.name.to_string()),
			Expression::FunctionCall {
				name,
				arguments,
				return_type: _,
			} =>
			{
				let mut buffer = String::new();
				write!(&mut buffer, "{}(", name.name)?;
				for argument in arguments
				{
					write!(
						&mut buffer,
						"{}",
						argument.rebuild(&indentation.increased())?
					)?;
				}
				write!(&mut buffer, ")")?;
				Ok(buffer)
			}
		}
	}
}

impl Rebuildable for PrimitiveLiteral
{
	fn rebuild(
		&self,
		_indentation: &Indentation,
	) -> Result<String, anyhow::Error>
	{
		match self
		{
			PrimitiveLiteral::Int32(value) => Ok(format!("{}", value)),
			PrimitiveLiteral::Bool(true) => Ok("true".to_string()),
			PrimitiveLiteral::Bool(false) => Ok("false".to_string()),
		}
	}
}

impl Rebuildable for ValueType
{
	fn rebuild(
		&self,
		_indentation: &Indentation,
	) -> Result<String, anyhow::Error>
	{
		match self
		{
			ValueType::Int32 => Ok("i32".to_string()),
			ValueType::Bool => Ok("bool".to_string()),
		}
	}
}
