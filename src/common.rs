//
// Part of penne
// Copyright (c) 2020 Sander in 't Veld
// License: MIT
//

//! Most compiler stages share a common AST.

use enumset::{EnumSet, EnumSetType};

pub use crate::error::{Poison, Poisonable};
pub use crate::lexer::Location;

use crate::value_type;

pub type ValueType = value_type::ValueType<Identifier>;

#[must_use]
#[derive(Debug, Clone)]
pub enum Declaration
{
	Constant
	{
		name: Identifier,
		value: Expression,
		value_type: Poisonable<ValueType>,
		flags: EnumSet<DeclarationFlag>,
		depth: Option<Poisonable<u32>>,
		location_of_declaration: Location,
		location_of_type: Location,
	},
	Function
	{
		name: Identifier,
		parameters: Vec<Parameter>,
		body: Poisonable<FunctionBody>,
		return_type: Poisonable<ValueType>,
		flags: EnumSet<DeclarationFlag>,
		location_of_declaration: Location,
		location_of_return_type: Location,
	},
	FunctionHead
	{
		name: Identifier,
		parameters: Vec<Parameter>,
		return_type: Poisonable<ValueType>,
		flags: EnumSet<DeclarationFlag>,
		location_of_declaration: Location,
		location_of_return_type: Location,
	},
	Structure
	{
		name: Identifier,
		members: Vec<Member>,
		structural_type: Poisonable<ValueType>,
		flags: EnumSet<DeclarationFlag>,
		depth: Option<Poisonable<u32>>,
		location_of_declaration: Location,
	},
	Import
	{
		filename: String,
		location: Location,
	},
	Poison(Poison),
}

#[must_use]
#[derive(EnumSetType, Debug)]
pub enum DeclarationFlag
{
	Public,
	External,
	Main,
	Forward,
	OpaqueStruct,
}

#[must_use]
#[derive(Debug, Clone)]
pub struct Member
{
	pub name: Poisonable<Identifier>,
	pub value_type: Poisonable<ValueType>,
	pub location_of_type: Location,
}

#[must_use]
#[derive(Debug, Clone)]
pub struct Parameter
{
	pub name: Poisonable<Identifier>,
	pub value_type: Poisonable<ValueType>,
	pub location_of_type: Location,
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
		value_type: Option<Poisonable<ValueType>>,
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
		else_branch: Option<Else>,
		location: Location,
	},
	Block(Block),
	Poison(Poison),
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
			Statement::Poison(Poison::Error { .. }) => unreachable!(),
			Statement::Poison(Poison::Poisoned) => unreachable!(),
		}
	}
}

#[must_use]
#[derive(Debug, Clone)]
pub struct Else
{
	pub branch: Box<Statement>,
	pub location_of_else: Location,
}

#[must_use]
#[derive(Debug, Clone)]
pub struct Comparison
{
	pub op: ComparisonOp,
	pub left: Expression,
	pub right: Expression,
	pub location: Location,
	pub location_of_op: Location,
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
			is_authoritative: true,
		}
	}
}

#[must_use]
#[derive(Debug, Clone)]
pub struct MemberExpression
{
	pub name: Poisonable<Identifier>,
	pub offset: Option<usize>,
	pub expression: Expression,
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
		location_of_op: Location,
	},
	Unary
	{
		op: UnaryOp,
		expression: Box<Expression>,
		location: Location,
		location_of_op: Location,
	},
	BooleanLiteral
	{
		value: bool,
		location: Location,
	},
	SignedIntegerLiteral
	{
		value: i128,
		value_type: Option<Poisonable<ValueType>>,
		location: Location,
	},
	BitIntegerLiteral
	{
		value: u128,
		value_type: Option<Poisonable<ValueType>>,
		location: Location,
	},
	StringLiteral
	{
		bytes: Vec<u8>,
		location: Location,
	},
	ArrayLiteral
	{
		array: Array,
		element_type: Option<Poisonable<ValueType>>,
	},
	Structural
	{
		members: Vec<MemberExpression>,
		structural_type: Poisonable<ValueType>,
		location: Location,
	},
	Parenthesized
	{
		inner: Box<Expression>,
		location: Location,
	},
	Deref
	{
		reference: Reference,
		deref_type: Option<Poisonable<ValueType>>,
	},
	Autocoerce
	{
		expression: Box<Expression>,
		coerced_type: ValueType,
	},
	BitCast
	{
		expression: Box<Expression>,
		coerced_type: Option<Poisonable<ValueType>>,
		location: Location,
		location_of_keyword: Location,
	},
	TypeCast
	{
		expression: Box<Expression>,
		coerced_type: ValueType,
		location: Location,
		location_of_type: Location,
	},
	LengthOfArray
	{
		reference: Reference,
		location: Location,
	},
	SizeOf
	{
		queried_type: ValueType,
		location: Location,
	},
	FunctionCall
	{
		name: Identifier,
		arguments: Vec<Expression>,
		return_type: Option<Poisonable<ValueType>>,
	},
	Poison(Poison),
}

impl Expression
{
	pub fn location(&self) -> &Location
	{
		match self
		{
			Expression::Binary { location, .. } => location,
			Expression::Unary { location, .. } => location,
			Expression::BooleanLiteral { location, .. } => location,
			Expression::SignedIntegerLiteral { location, .. } => location,
			Expression::BitIntegerLiteral { location, .. } => location,
			Expression::StringLiteral { location, .. } => location,
			Expression::ArrayLiteral { array, .. } => &array.location,
			Expression::Structural { location, .. } => location,
			Expression::Parenthesized { location, .. } => location,
			Expression::Deref { reference, .. } => &reference.location,
			Expression::Autocoerce { expression, .. } => expression.location(),
			Expression::BitCast { location, .. } => location,
			Expression::TypeCast { location, .. } => location,
			Expression::LengthOfArray { location, .. } => &location,
			Expression::SizeOf { location, .. } => &location,
			Expression::FunctionCall { name, .. } => &name.location,
			Expression::Poison(Poison::Error { .. }) => unreachable!(),
			Expression::Poison(Poison::Poisoned) => unreachable!(),
		}
	}
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
#[derive(Debug, Clone)]
pub enum DesliceOffset
{
	// 0
	ArrayByView,
	ArrayByPointer,
	// 1
	Length,
}

#[must_use]
#[derive(Debug, Clone)]
pub enum ReferenceStep
{
	Element
	{
		argument: Box<Expression>,
		is_endless: Option<bool>,
	},
	Member
	{
		member: Identifier,
		offset: Option<usize>,
	},
	Autodeslice
	{
		offset: DesliceOffset,
	},
	Autoderef,
	Autoview,
}

impl ReferenceStep
{
	pub fn is_concrete(&self) -> bool
	{
		match self
		{
			ReferenceStep::Element {
				argument: _,
				is_endless,
			} => is_endless.is_some(),
			ReferenceStep::Member { member: _, offset } => offset.is_some(),
			ReferenceStep::Autodeslice { offset: _ } => true,
			ReferenceStep::Autoderef => true,
			ReferenceStep::Autoview => true,
		}
	}

	pub fn get_member(&self) -> Option<Identifier>
	{
		match self
		{
			ReferenceStep::Member { member, offset: _ } => Some(member.clone()),
			_ => None,
		}
	}
}

#[must_use]
#[derive(Debug, Clone)]
pub struct Reference
{
	pub base: Poisonable<Identifier>,
	pub steps: Vec<ReferenceStep>,
	pub address_depth: u8,
	pub location: Location,
	pub location_of_unaddressed: Location,
}

impl Reference
{
	pub fn is_trivial(&self) -> bool
	{
		self.steps.is_empty() && self.address_depth == 0
	}
}

#[must_use]
#[derive(Debug, Clone)]
pub struct Identifier
{
	pub name: String,
	pub location: Location,
	pub resolution_id: u32,
	pub is_authoritative: bool,
}

impl Identifier
{
	pub fn inferred(&self) -> Self
	{
		Identifier {
			is_authoritative: false,
			..self.clone()
		}
	}

	pub fn return_value(&self) -> Self
	{
		Identifier {
			name: format!("(return value of '{}')", self.name),
			is_authoritative: true,
			..self.clone()
		}
	}
}

impl value_type::Identifier for Identifier {}

impl PartialEq for Identifier
{
	fn eq(&self, other: &Identifier) -> bool
	{
		if self.resolution_id > 0
		{
			self.resolution_id == other.resolution_id
		}
		else
		{
			self.location == other.location
		}
	}
}
