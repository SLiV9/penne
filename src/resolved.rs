//
// Part of penne
// Copyright (c) 2020 Sander in 't Veld
// License: MIT
//

//! The resolved AST contain no syntax or semantic errors and is ready for
//! IR generation.

pub use crate::builtin;
pub use crate::common::DeclarationFlag;
pub use crate::common::{BinaryOp, ComparisonOp, UnaryOp};

pub(crate) use crate::generator::GeneratorBuiltin;

use crate::value_type;

pub type ValueType = value_type::ValueType<Identifier>;

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
		depth: u32,
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
	Structure
	{
		name: Identifier,
		members: Vec<Member>,
		flags: EnumSet<DeclarationFlag>,
		depth: u32,
	},
}

#[must_use]
#[derive(Debug, Clone)]
pub struct Member
{
	pub name: Identifier,
	pub value_type: ValueType,
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
pub struct MemberExpression
{
	pub name: Identifier,
	pub offset: usize,
	pub expression: Expression,
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
	EvaluateAndDiscard
	{
		value: Expression,
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

/// Helper trait that extracts the resolved type from a value expression.
pub trait Typed
{
	fn value_type(&self) -> ValueType;
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
		value_type: ValueType,
	},
	SignedIntegerLiteral
	{
		value: i128,
		value_type: ValueType,
	},
	BitIntegerLiteral
	{
		value: u128,
		value_type: ValueType,
	},
	StringLiteral
	{
		bytes: Vec<u8>,
	},
	ArrayLiteral
	{
		elements: Vec<Expression>,
		element_type: ValueType,
	},
	Structural
	{
		members: Vec<MemberExpression>,
		structural_type: ValueType,
	},
	Parenthesized
	{
		inner: Box<Expression>,
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
	BitCast
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
	SizeOf
	{
		queried_type: ValueType,
	},
	FunctionCall
	{
		name: Identifier,
		arguments: Vec<Expression>,
		return_type: ValueType,
	},
	InlineBlock
	{
		statements: Vec<Statement>,
		value: Box<Expression>,
	},
	Builtin(GeneratorBuiltin),
}

impl Typed for Expression
{
	fn value_type(&self) -> ValueType
	{
		match self
		{
			Expression::Binary { value_type, .. } => value_type.clone(),
			Expression::Unary { value_type, .. } => value_type.clone(),
			Expression::SignedIntegerLiteral { value_type, .. } =>
			{
				value_type.clone()
			}
			Expression::BitIntegerLiteral { value_type, .. } =>
			{
				value_type.clone()
			}
			Expression::StringLiteral { bytes } =>
			{
				ValueType::for_string_literal(bytes.len())
			}
			Expression::ArrayLiteral {
				elements,
				element_type,
			} => ValueType::Array {
				element_type: Box::new(element_type.clone()),
				length: elements.len(),
			},
			Expression::Structural {
				structural_type, ..
			} => structural_type.clone(),
			Expression::Parenthesized { inner } => inner.value_type(),
			Expression::Deref { deref_type, .. } => deref_type.clone(),
			Expression::Autocoerce { coerced_type, .. } => coerced_type.clone(),
			Expression::BitCast { coerced_type, .. } => coerced_type.clone(),
			Expression::PrimitiveCast { coerced_type, .. } =>
			{
				coerced_type.clone()
			}
			Expression::LengthOfArray { .. } => ValueType::Usize,
			Expression::SizeOf { .. } => ValueType::Usize,
			Expression::FunctionCall { return_type, .. } => return_type.clone(),
			Expression::InlineBlock { value, .. } => value.value_type(),
			Expression::Builtin(builtin) => builtin.value_type(),
		}
	}
}

#[must_use]
#[derive(Debug, Clone)]
pub struct Reference
{
	pub base: Identifier,
	pub steps: Vec<ReferenceStep>,
	pub take_address: bool,
}

impl Reference
{
	pub fn is_trivial(&self) -> bool
	{
		self.steps.is_empty() && !self.take_address
	}
}

#[must_use]
#[derive(Debug, Clone)]
pub enum ReferenceStep
{
	Element
	{
		argument: Box<Expression>,
		is_endless: bool,
	},
	Member
	{
		offset: usize,
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

impl value_type::Identifier for Identifier {}

impl PartialEq for Identifier
{
	fn eq(&self, other: &Identifier) -> bool
	{
		self.resolution_id > 0 && self.resolution_id == other.resolution_id
	}
}
