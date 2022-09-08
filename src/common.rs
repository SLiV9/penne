/**/

use enumset::{EnumSet, EnumSetType};

pub use crate::lexer::Location;

#[must_use]
#[derive(Debug, Clone)]
pub enum Declaration
{
	Constant
	{
		name: Identifier,
		value: Expression,
		value_type: ValueType,
		flags: EnumSet<DeclarationFlag>,
	},
	Function
	{
		name: Identifier,
		parameters: Vec<Parameter>,
		body: FunctionBody,
		return_type: Option<ValueType>,
		flags: EnumSet<DeclarationFlag>,
	},
	FunctionHead
	{
		name: Identifier,
		parameters: Vec<Parameter>,
		return_type: Option<ValueType>,
		flags: EnumSet<DeclarationFlag>,
	},
}

#[must_use]
#[derive(EnumSetType, Debug)]
pub enum DeclarationFlag
{
	Public,
	External,
}

#[must_use]
#[derive(Debug, Clone)]
pub struct Parameter
{
	pub name: Identifier,
	pub value_type: Option<ValueType>,
}

#[must_use]
#[derive(Debug, Clone)]
pub struct FunctionBody
{
	pub statements: Vec<Statement>,
	pub return_value: Option<Expression>,
	pub return_value_identifier: Identifier,
}

#[must_use]
#[derive(Debug, Clone)]
pub struct Block
{
	pub statements: Vec<Statement>,
	pub location: Location,
}

#[must_use]
#[derive(Debug, Clone)]
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
		reference: Reference,
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

#[must_use]
#[derive(Debug, Clone)]
pub struct Comparison
{
	pub op: ComparisonOp,
	pub left: Expression,
	pub right: Expression,
	pub location: Location,
}

#[must_use]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ComparisonOp
{
	Equals,
}

#[must_use]
#[derive(Debug, Clone)]
pub struct Array
{
	pub elements: Vec<Expression>,
	pub location: Location,
	pub resolution_id: u32,
}

impl Array
{
	pub fn get_identifier(&self) -> Identifier
	{
		Identifier {
			name: "(array)".to_string(),
			location: self.location.clone(),
			resolution_id: self.resolution_id,
		}
	}
}

#[must_use]
#[derive(Debug, Clone)]
pub enum Expression
{
	Binary
	{
		op: BinaryOp,
		left: Box<Expression>,
		right: Box<Expression>,
		location: Location,
	},
	PrimitiveLiteral(PrimitiveLiteral),
	NakedIntegerLiteral
	{
		value: i128,
		value_type: Option<ValueType>,
		location: Location,
	},
	BitIntegerLiteral
	{
		value: u64,
		value_type: Option<ValueType>,
		location: Location,
	},
	StringLiteral(String),
	ArrayLiteral
	{
		array: Array,
		element_type: Option<ValueType>,
	},
	Deref
	{
		reference: Reference,
		ref_type: Option<ValueType>,
		deref_type: Option<ValueType>,
	},
	LengthOfArray
	{
		reference: Reference,
	},
	FunctionCall
	{
		name: Identifier,
		arguments: Vec<Expression>,
		return_type: Option<ValueType>,
	},
}

#[must_use]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BinaryOp
{
	Add,
	Subtract,
}

#[must_use]
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum PrimitiveLiteral
{
	Int8(i8),
	Int16(i16),
	Int32(i32),
	Int64(i64),
	Int128(i128),
	Uint8(u8),
	Uint16(u16),
	Uint32(u32),
	Uint64(u64),
	Uint128(u128),
	Usize(usize),
	Bool(bool),
}

#[must_use]
#[derive(Debug, Clone)]
pub enum Reference
{
	Identifier(Identifier),
	ArrayElement
	{
		name: Identifier,
		argument: Box<Expression>,
	},
}

impl Reference
{
	pub fn location(&self) -> &Location
	{
		match self
		{
			Reference::Identifier(name) => &name.location,
			Reference::ArrayElement { name, .. } => &name.location,
		}
	}
}

#[must_use]
#[derive(Debug, Clone)]
pub struct Identifier
{
	pub name: String,
	pub location: Location,
	pub resolution_id: u32,
}

impl Identifier
{
	pub fn return_value(self) -> Self
	{
		Identifier {
			name: format!("(return value of '{}')", self.name),
			..self
		}
	}
}

#[must_use]
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ValueType
{
	Int8,
	Int16,
	Int32,
	Int64,
	Int128,
	Uint8,
	Uint16,
	Uint32,
	Uint64,
	Uint128,
	Usize,
	Bool,
	Array
	{
		element_type: Box<ValueType>,
		length: usize,
	},
	Slice
	{
		element_type: Box<ValueType>,
	},
	ExtArray
	{
		element_type: Box<ValueType>,
	},
	Pointer
	{
		deref_type: Box<ValueType>,
	},
	View
	{
		deref_type: Box<ValueType>,
	},
}

impl ValueType
{
	pub fn can_autoderef_into(&self, other: &ValueType) -> bool
	{
		match self
		{
			ValueType::Array {
				element_type: a,
				length: _,
			} => match other
			{
				ValueType::Slice { element_type: b } => a == b,
				ValueType::ExtArray { element_type: b } => a == b,
				ValueType::View { deref_type } => match deref_type.as_ref()
				{
					ValueType::ExtArray { element_type: b } => a == b,
					_ => false,
				},
				_ => false,
			},
			ValueType::Slice { element_type: a } => match other
			{
				ValueType::ExtArray { element_type: b } => a == b,
				ValueType::View { deref_type } => match deref_type.as_ref()
				{
					ValueType::ExtArray { element_type: b } => a == b,
					_ => false,
				},
				_ => false,
			},
			ValueType::View { deref_type } =>
			{
				deref_type.as_ref() == other
					|| deref_type.can_autoderef_into(other)
			}
			_ => false,
		}
	}

	pub fn get_element_type(&self) -> Option<ValueType>
	{
		match self
		{
			ValueType::Array {
				element_type,
				length: _,
			} => Some(element_type.as_ref().clone()),
			ValueType::Slice { element_type } =>
			{
				Some(element_type.as_ref().clone())
			}
			ValueType::ExtArray { element_type } =>
			{
				Some(element_type.as_ref().clone())
			}
			ValueType::Pointer { .. } => None,
			ValueType::View { .. } => None,
			ValueType::Int8 => None,
			ValueType::Int16 => None,
			ValueType::Int32 => None,
			ValueType::Int64 => None,
			ValueType::Int128 => None,
			ValueType::Uint8 => None,
			ValueType::Uint16 => None,
			ValueType::Uint32 => None,
			ValueType::Uint64 => None,
			ValueType::Uint128 => None,
			ValueType::Usize => None,
			ValueType::Bool => None,
		}
	}
}
