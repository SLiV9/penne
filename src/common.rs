/**/

pub use crate::lexer::Location;

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

#[derive(Debug, Clone)]
pub struct Parameter
{
	pub name: Identifier,
	pub value_type: Option<ValueType>,
	pub is_mutable: bool,
}

#[derive(Debug)]
pub struct FunctionBody
{
	pub statements: Vec<Statement>,
	pub return_value: Option<Expression>,
}

#[derive(Debug)]
pub struct Block
{
	pub statements: Vec<Statement>,
	pub location: Location,
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

#[derive(Debug)]
pub struct Comparison
{
	pub op: ComparisonOp,
	pub left: Expression,
	pub right: Expression,
	pub location: Location,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ComparisonOp
{
	Equals,
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

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BinaryOp
{
	Add,
	Subtract,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Literal
{
	Int32(i32),
	Bool(bool),
	StringLiteral(String),
}

#[derive(Debug, Clone)]
pub struct Identifier
{
	pub name: String,
	pub location: Location,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ValueType
{
	Int32,
	Bool,
	StringLiteral,
}
