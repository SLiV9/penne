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
	PreprocessorDirective
	{
		directive: String,
		location: Location,
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
	MethodCall
	{
		name: Identifier,
		arguments: Vec<Expression>,
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
			Statement::MethodCall { name, .. } => &name.location,
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
	DoesNotEqual,
	IsGreater,
	IsGE,
	IsLess,
	IsLE,
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
	Unary
	{
		op: UnaryOp,
		expression: Box<Expression>,
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
	StringLiteral
	{
		bytes: Vec<u8>,
		value_type: Option<ValueType>,
		location: Location,
	},
	ArrayLiteral
	{
		array: Array,
		element_type: Option<ValueType>,
	},
	Deref
	{
		reference: Reference,
		deref_type: Option<ValueType>,
	},
	Autocoerce
	{
		expression: Box<Expression>,
		coerced_type: ValueType,
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
	Multiply,
	Divide,
	Modulo,
	BitwiseAnd,
	BitwiseOr,
	BitwiseXor,
	ShiftLeft,
	ShiftRight,
}

#[must_use]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum UnaryOp
{
	Negative,
	BitwiseComplement,
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
pub enum ReferenceStep
{
	Element
	{
		argument: Box<Expression>,
	},
	Member
	{
		member: Identifier,
	},
	Autodeslice
	{
		offset: u8,
	},
	Autoderef,
	Autoview,
}

#[must_use]
#[derive(Debug, Clone)]
pub struct Reference
{
	pub base: Identifier,
	pub steps: Vec<ReferenceStep>,
	pub address_depth: u8,
	pub location: Location,
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
	Char,
	String,
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
	pub fn for_byte_string() -> ValueType
	{
		ValueType::Slice {
			element_type: Box::new(ValueType::Uint8),
		}
	}

	pub fn can_be_used_as(&self, other: &ValueType) -> bool
	{
		match self
		{
			ValueType::Array {
				element_type: a,
				length: _,
			} => match other
			{
				ValueType::ExtArray { element_type: b } => a.can_be_used_as(b),
				_ => self == other,
			},
			ValueType::Slice { element_type: a } => match other
			{
				ValueType::ExtArray { element_type: b } => a.can_be_used_as(b),
				_ => self == other,
			},
			_ => self == other,
		}
	}

	pub fn can_be_declared_as(&self, other: &ValueType) -> bool
	{
		match self
		{
			ValueType::Array {
				element_type: a,
				length: _,
			} => match other
			{
				ValueType::Slice { element_type: b } => a == b,
				_ => self == other,
			},
			_ => self == other,
		}
	}

	pub fn can_coerce_into(&self, other: &ValueType) -> bool
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
			_ => false,
		}
	}

	pub fn can_autoderef_into(&self, other: &ValueType) -> bool
	{
		match self
		{
			ValueType::Array {
				element_type: a,
				length: _,
			} => match other
			{
				ValueType::Slice { element_type: b } => a.can_be_used_as(b),
				ValueType::ExtArray { element_type: b } => a.can_be_used_as(b),
				ValueType::View { deref_type } => match deref_type.as_ref()
				{
					ValueType::ExtArray { element_type: b } =>
					{
						a.can_be_used_as(b)
					}
					_ => false,
				},
				_ => false,
			},
			ValueType::Slice { element_type: a } => match other
			{
				ValueType::ExtArray { element_type: b } => a.can_be_used_as(b),
				ValueType::View { deref_type } => match deref_type.as_ref()
				{
					ValueType::ExtArray { element_type: b } =>
					{
						a.can_be_used_as(b)
					}
					_ => false,
				},
				_ => false,
			},
			ValueType::View { deref_type } =>
			{
				deref_type.as_ref().can_be_used_as(other)
					|| deref_type.can_autoderef_into(other)
					|| other.get_viewee_type().map_or(false, |t| {
						deref_type.as_ref().can_be_used_as(&t)
							|| deref_type.can_autoderef_into(&t)
					})
			}
			ValueType::Pointer { deref_type } =>
			{
				deref_type.as_ref().can_be_used_as(other)
					|| deref_type.can_autoderef_into(other)
					|| other.get_pointee_type().map_or(false, |t| {
						deref_type.as_ref().can_be_used_as(&t)
							|| deref_type.can_autoderef_into(&t)
					})
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
			_ => None,
		}
	}

	pub fn get_pointee_type(&self) -> Option<ValueType>
	{
		match self
		{
			ValueType::Pointer { deref_type } =>
			{
				Some(deref_type.as_ref().clone())
			}
			_ => None,
		}
	}

	pub fn get_viewee_type(&self) -> Option<ValueType>
	{
		match self
		{
			ValueType::View { deref_type } => Some(deref_type.as_ref().clone()),
			_ => None,
		}
	}

	pub fn fully_dereferenced(&self) -> ValueType
	{
		match self
		{
			ValueType::Pointer { deref_type } =>
			{
				deref_type.fully_dereferenced()
			}
			ValueType::View { deref_type } => deref_type.fully_dereferenced(),
			_ => self.clone(),
		}
	}
}
