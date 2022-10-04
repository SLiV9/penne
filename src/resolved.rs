//
// Part of penne
// Copyright (c) 2020 Sander in 't Veld
// License: MIT
//

pub use crate::common::DeclarationFlag;
pub use crate::common::PrimitiveLiteral;
pub use crate::common::{BinaryOp, ComparisonOp, UnaryOp};
pub use crate::value_type::ValueType;

use enumset::EnumSet;

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
#[derive(Debug, Clone)]
pub struct Parameter
{
	pub name: Identifier,
	pub value_type: ValueType,
}

#[must_use]
#[derive(Debug, Clone)]
pub struct FunctionBody
{
	pub statements: Vec<Statement>,
	pub return_value: Option<Expression>,
}

#[must_use]
#[derive(Debug, Clone)]
pub struct Block
{
	pub statements: Vec<Statement>,
}

#[must_use]
#[derive(Debug, Clone)]
pub enum Statement
{
	Declaration
	{
		name: Identifier,
		value: Option<Expression>,
		value_type: ValueType,
	},
	Assignment
	{
		reference: Reference,
		value: Expression,
	},
	MethodCall
	{
		name: Identifier,
		arguments: Vec<Expression>,
	},
	Loop,
	Goto
	{
		label: Identifier,
	},
	Label
	{
		label: Identifier,
	},
	If
	{
		condition: Comparison,
		then_branch: Box<Statement>,
		else_branch: Option<Box<Statement>>,
	},
	Block(Block),
}

#[must_use]
#[derive(Debug, Clone)]
pub struct Comparison
{
	pub op: ComparisonOp,
	pub left: Expression,
	pub right: Expression,
	pub compared_type: ValueType,
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
		value_type: ValueType,
	},
	Unary
	{
		op: UnaryOp,
		expression: Box<Expression>,
	},
	PrimitiveLiteral(PrimitiveLiteral),
	NakedIntegerLiteral
	{
		value: i128,
		value_type: ValueType,
	},
	BitIntegerLiteral
	{
		value: u64,
		value_type: ValueType,
	},
	StringLiteral
	{
		bytes: Vec<u8>,
	},
	ByteStringLiteral
	{
		bytes: Vec<u8>,
	},
	ArrayLiteral
	{
		elements: Vec<Expression>,
		element_type: ValueType,
	},
	Deref
	{
		reference: Reference,
		deref_type: ValueType,
	},
	Autocoerce
	{
		expression: Box<Expression>,
		coerced_type: ValueType,
	},
	PrimitiveCast
	{
		expression: Box<Expression>,
		expression_type: ValueType,
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
		return_type: ValueType,
	},
}

#[must_use]
#[derive(Debug, Clone)]
pub struct Reference
{
	pub base: Identifier,
	pub steps: Vec<ReferenceStep>,
	pub take_address: bool,
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
pub struct Identifier
{
	pub name: String,
	pub resolution_id: u32,
}
