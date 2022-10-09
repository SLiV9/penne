//
// Part of penne
// Copyright (c) 2020 Sander in 't Veld
// License: MIT
//

use enumset::{EnumSet, EnumSetType};

pub use crate::error::{Poison, Poisonable};
pub use crate::lexer::Location;
pub use crate::value_type::ValueType;

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
	},
	Function
	{
		name: Identifier,
		parameters: Vec<Parameter>,
		body: Poisonable<FunctionBody>,
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
	Poison(Poison<Box<Declaration>>),
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
	pub name: Poisonable<Identifier>,
	pub value_type: Poisonable<ValueType>,
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
	Poison(Poison<Box<Statement>>),
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
			Statement::Poison(Poison::Error {
				error: _,
				partial: Some(statement),
			}) => statement.location(),
			Statement::Poison(Poison::Error {
				error: _,
				partial: None,
			}) => unreachable!(),
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
		location_of_op: Location,
	},
	Unary
	{
		op: UnaryOp,
		expression: Box<Expression>,
		location: Location,
		location_of_op: Location,
	},
	PrimitiveLiteral
	{
		literal: PrimitiveLiteral,
		location: Location,
	},
	NakedIntegerLiteral
	{
		value: i128,
		value_type: Option<Poisonable<ValueType>>,
		location: Location,
	},
	BitIntegerLiteral
	{
		value: u64,
		value_type: Option<Poisonable<ValueType>>,
		location: Location,
	},
	StringLiteral
	{
		bytes: Vec<u8>,
		value_type: Option<Poisonable<ValueType>>,
		location: Location,
	},
	ArrayLiteral
	{
		array: Array,
		element_type: Option<Poisonable<ValueType>>,
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
	PrimitiveCast
	{
		expression: Box<Expression>,
		coerced_type: ValueType,
		location: Location,
		location_of_type: Location,
	},
	LengthOfArray
	{
		reference: Reference,
	},
	FunctionCall
	{
		name: Identifier,
		arguments: Vec<Expression>,
		return_type: Option<Poisonable<ValueType>>,
	},
	Poison(Poison<Box<Expression>>),
}

impl Expression
{
	pub fn location(&self) -> &Location
	{
		match self
		{
			Expression::Binary { location, .. } => location,
			Expression::Unary { location, .. } => location,
			Expression::PrimitiveLiteral { location, .. } => location,
			Expression::NakedIntegerLiteral { location, .. } => location,
			Expression::BitIntegerLiteral { location, .. } => location,
			Expression::StringLiteral { location, .. } => location,
			Expression::ArrayLiteral { array, .. } => &array.location,
			Expression::Deref { reference, .. } => &reference.location,
			Expression::Autocoerce { expression, .. } => expression.location(),
			Expression::PrimitiveCast { location, .. } => location,
			Expression::LengthOfArray { reference, .. } => &reference.location,
			Expression::FunctionCall { name, .. } => &name.location,
			Expression::Poison(Poison::Error {
				error: _,
				partial: Some(expression),
			}) => expression.location(),
			Expression::Poison(Poison::Error {
				error: _,
				partial: None,
			}) => unreachable!(),
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
	pub base: Poisonable<Identifier>,
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
	pub fn return_value(&self) -> Self
	{
		Identifier {
			name: format!("(return value of '{}')", self.name),
			..self.clone()
		}
	}
}
