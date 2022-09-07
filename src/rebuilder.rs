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
			Declaration::Constant {
				name,
				value,
				value_type,
				flags,
			} =>
			{
				let mut buffer = String::new();
				write!(&mut buffer, "{}", indentation)?;
				if flags.contains(DeclarationFlag::Public)
				{
					write!(&mut buffer, "pub ")?;
				}
				if flags.contains(DeclarationFlag::External)
				{
					write!(&mut buffer, "extern ")?;
				}
				write!(
					&mut buffer,
					"const {}: {} = {};",
					identify(name),
					value_type.rebuild(&indentation.increased())?,
					value.rebuild(&indentation.increased())?
				)?;
				Ok(buffer)
			}
			Declaration::Function {
				name,
				parameters,
				body,
				return_type,
				flags,
			} =>
			{
				let mut buffer = String::new();
				write!(&mut buffer, "{}", indentation)?;
				if flags.contains(DeclarationFlag::Public)
				{
					write!(&mut buffer, "pub ")?;
				}
				if flags.contains(DeclarationFlag::External)
				{
					write!(&mut buffer, "extern ")?;
				}
				write!(&mut buffer, "fn {}(", identify(name))?;
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
			Declaration::FunctionHead {
				name,
				parameters,
				return_type,
				flags,
			} =>
			{
				let mut buffer = String::new();
				write!(&mut buffer, "{}", indentation)?;
				if flags.contains(DeclarationFlag::External)
				{
					write!(&mut buffer, "extern ")?;
				}
				write!(&mut buffer, "fn {}(", identify(name))?;
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
		let mut buffer = identify(&self.name);
		if let Some(value_type) = &self.value_type
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
				identify(name),
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
				identify(name),
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
				identify(name),
				value.rebuild(&indentation.increased())?
			)),
			Statement::Declaration {
				name,
				value: None,
				value_type: None,
				location: _,
			} => Ok(format!("{}var {};\n", indentation, identify(name))),
			Statement::Assignment {
				reference,
				value,
				location: _,
			} => Ok(format!(
				"{}{} = {};\n",
				indentation,
				reference.rebuild(&indentation.increased())?,
				value.rebuild(&indentation.increased())?
			)),
			Statement::Loop { location: _ } =>
			{
				Ok(format!("{}loop;\n", indentation))
			}
			Statement::Goto { label, location: _ } =>
			{
				Ok(format!("{}goto {};\n", indentation, identify(label)))
			}
			Statement::Label { label, location: _ } =>
			{
				Ok(format!("{}{}:\n", indentation, identify(label)))
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
		write!(&mut buffer, "[\n")?;
		for element in &self.elements
		{
			write!(
				&mut buffer,
				"{}{},\n",
				indentation.increased(),
				element.rebuild(&indentation.increased())?
			)?;
		}
		write!(&mut buffer, "{}]", indentation)?;
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
			Expression::NakedIntegerLiteral {
				value,
				value_type: _,
				location: _,
			} => Ok(format!("{}", value)),
			Expression::ArrayLiteral {
				array,
				element_type: _,
			} => array.rebuild(indentation),
			Expression::StringLiteral(value) =>
			{
				Ok(format!("\"{}\"", value.escape_default()))
			}
			Expression::Deref {
				reference,
				value_type: _,
			} => reference.rebuild(indentation),
			Expression::LengthOfArray { reference } => Ok(format!(
				"|{}|",
				reference.rebuild(&indentation.increased())?
			)),
			Expression::FunctionCall {
				name,
				arguments,
				return_type: _,
			} =>
			{
				let mut buffer = String::new();
				write!(&mut buffer, "{}(", identify(name))?;
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
			PrimitiveLiteral::Int8(value) => Ok(format!("{}i8", value)),
			PrimitiveLiteral::Int16(value) => Ok(format!("{}i16", value)),
			PrimitiveLiteral::Int32(value) => Ok(format!("{}i32", value)),
			PrimitiveLiteral::Int64(value) => Ok(format!("{}i64", value)),
			PrimitiveLiteral::Int128(value) => Ok(format!("{}i128", value)),
			PrimitiveLiteral::Uint8(value) => Ok(format!("{}u8", value)),
			PrimitiveLiteral::Uint16(value) => Ok(format!("{}u16", value)),
			PrimitiveLiteral::Uint32(value) => Ok(format!("{}u32", value)),
			PrimitiveLiteral::Uint64(value) => Ok(format!("{}u64", value)),
			PrimitiveLiteral::Uint128(value) => Ok(format!("{}u128", value)),
			PrimitiveLiteral::Usize(value) => Ok(format!("{}usize", value)),
			PrimitiveLiteral::Bool(true) => Ok("true".to_string()),
			PrimitiveLiteral::Bool(false) => Ok("false".to_string()),
		}
	}
}

impl Rebuildable for ValueType
{
	fn rebuild(
		&self,
		indentation: &Indentation,
	) -> Result<String, anyhow::Error>
	{
		match self
		{
			ValueType::Int8 => Ok("i8".to_string()),
			ValueType::Int16 => Ok("i16".to_string()),
			ValueType::Int32 => Ok("i32".to_string()),
			ValueType::Int64 => Ok("i64".to_string()),
			ValueType::Int128 => Ok("i128".to_string()),
			ValueType::Uint8 => Ok("u8".to_string()),
			ValueType::Uint16 => Ok("u16".to_string()),
			ValueType::Uint32 => Ok("u32".to_string()),
			ValueType::Uint64 => Ok("u64".to_string()),
			ValueType::Uint128 => Ok("u128".to_string()),
			ValueType::Usize => Ok("usize".to_string()),
			ValueType::Bool => Ok("bool".to_string()),
			ValueType::Array {
				element_type,
				length,
			} => Ok(format!(
				"[{}]{}",
				length,
				element_type.rebuild(indentation)?
			)),
			ValueType::Slice { element_type } =>
			{
				Ok(format!("[]{}", element_type.rebuild(indentation)?))
			}
		}
	}
}

impl Rebuildable for Reference
{
	fn rebuild(
		&self,
		indentation: &Indentation,
	) -> Result<String, anyhow::Error>
	{
		match self
		{
			Reference::Identifier(identifier) => Ok(identify(identifier)),
			Reference::ArrayElement { name, argument } => Ok(format!(
				"{}[{}]",
				identify(name),
				argument.rebuild(&indentation.increased())?
			)),
		}
	}
}

fn identify(identifier: &Identifier) -> String
{
	if identifier.resolution_id > 0
	{
		format!(
			"{}#{}",
			identifier.name.to_string(),
			identifier.resolution_id
		)
	}
	else
	{
		identifier.name.to_string()
	}
}
