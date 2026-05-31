use enumset::EnumSet;

use crate::alpha::common::BinaryOp;
use crate::alpha::common::ComparisonOp;
use crate::alpha::common::DeclarationFlag;
use crate::alpha::common::UnaryOp;
use crate::delta::lexer::ValueTypeKeyword;

const MAX_NUM_NODES: usize = 1 << 24;

#[must_use]
#[derive(Debug, Clone, Copy)]
pub struct NodeId(pub U24);

#[derive(Debug, Clone, Copy)]
pub struct TokenId(pub U24);

#[derive(Clone, Copy)]
pub struct U24([u8; 3]);

impl U24
{
	pub fn new(value: usize) -> Self
	{
		debug_assert!(value < MAX_NUM_NODES);
		let value = value as u32;
		let [_x3, x2, x1, x0] = value.to_le_bytes();
		Self([x2, x1, x0])
	}
}

impl From<U24> for u32
{
	fn from(value: U24) -> Self
	{
		let [x2, x1, x0] = value.0;
		let bytes = [0, x2, x1, x0];
		u32::from_le_bytes(bytes)
	}
}

impl From<U24> for usize
{
	fn from(value: U24) -> Self
	{
		u32::from(value) as usize
	}
}

impl std::fmt::Debug for U24
{
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result
	{
		let value = u32::from(*self);
		f.debug_tuple("U24").field(&value).finish()
	}
}

#[derive(Debug, Clone, Copy)]
pub enum ParseNode
{
	Padding,
	ConstantDeclaration
	{
		start_of_declaration: TokenId,
		// flags: DeclarationFlags = nodes[-1]
		// identifier: Identifier = nodes[-2]
		// value_type: Item<ValueType> = nodes[-3]
		// expression: Expression = nodes[..-4]
	},
	FunctionDeclaration
	{
		start_of_declaration: TokenId,
		// flags: DeclarationFlags = nodes[-1]
		// identifier: Identifier = nodes[-2]
		// parameters: List<IdentifierAndVT> = nodes[-3]
		// return_type: Item<ValueType> = nodes[-4]
		// statements: List<Statement> = nodes[-5]
		// return_value: Item<Expression>/NoMoreItems = nodes[-6]
	},
	FunctionHeadDeclaration
	{
		start_of_declaration: TokenId,
		// flags: DeclarationFlags = nodes[-1]
		// identifier: Identifier = nodes[-2]
		// parameters: List<IdentifierAndVT> = nodes[-3]
		// return_type: Item<ValueType> = nodes[-4]
		// statements: Padding = nodes[-5]
		// return_value: Padding = nodes[-6]
	},
	StructureDeclaration
	{
		start_of_declaration: TokenId,
		// flags: DeclarationFlags = nodes[-1]
		// identifier: Identifier = nodes[-2]
		// structural_type: StructuralType = nodes[-3]
		// members: List<IdentifierAndVT> = nodes[-4]
	},
	ImportDeclaration
	{
		start_of_declaration: TokenId,
		// flags: DeclarationFlags = nodes[-1]
		// path: StringLiteral = nodes[-2]
	},
	DeclarationFlags(EnumSet<DeclarationFlag>),
	StructuralType
	{
		size_in_bytes_if_word: Option<u8>,
	},
	Identifier
	{
		identifier: TokenId,
	},
	IdentifierAndVT
	{
		identifier: TokenId,
		// value_type: ValueType = nodes[..-1]
	},
	IdentifierAndExpression
	{
		identifier: TokenId,
		// expression: Expression = nodes[..-1]
	},
	VariableDeclaration
	{
		identifier: TokenId,
		// value_type: Item<ValueType>/NoMoreItems = nodes[-1]
		// expression: Item<Expression>/NoMoreItems = nodes[-2]
	},
	Assignment {
		// deref: Item<Deref> = nodes[-1]
		// expression: Expression = nodes[..-2]
	},
	Loop
	{
		token: TokenId,
	},
	Goto
	{
		token: TokenId,
		// label: Identifier = nodes[-1]
	},
	Label
	{
		// label: Identifier = nodes[-1]
		semicolon: TokenId,
	},
	If
	{
		comparison: NodeId,
		// then: Then/ThenElse = nodes[-1]
	},
	Then {
		// then: Statement = nodes[..-1]
	},
	ThenElse
	{
		then: NodeId,
		// else: Statement = nodes[..-1]
	},
	Block
	{
		first: NodeId, // ListItem<Statement>
	},
	MethodCall
	{
		// identifier: Identifier = nodes[-1]
		is_builtin: bool,
		// arguments: List<Expression> = nodes[-2]
	},
	Parenthesized {
		// inner: Expression = nodes[..-1]
	},
	Comparison
	{
		token: TokenId,
		// op: ComparisonOp = nodes[-1],
		// left: Item<Expression> = nodes[-2],
		// right: Expression = nodes[..-3]
	},
	ComparisonOp(ComparisonOp),
	Binary
	{
		token: TokenId,
		// op: BinaryOp = nodes[-1],
		// left: Item<Expression> = nodes[-2],
		// right: Expression = nodes[..-3]
	},
	BinaryOp(BinaryOp),
	Unary
	{
		token: TokenId,
		// op: UnaryOp = nodes[-1],
		// operand: Expression = nodes[..-2],
	},
	UnaryOp(UnaryOp),
	BooleanLiteral
	{
		literal: TokenId,
	},
	CharLiteral
	{
		literal: TokenId,
	},
	UntypedIntegerLiteral
	{
		literal: TokenId,
	},
	TypedIntegerLiteral
	{
		literal: TokenId,
		// value_type: SimpleValueType = nodes[-1]
	},
	SimpleStringLiteral
	{
		literal: TokenId,
	},
	CompositeStringLiteral
	{
		start: TokenId,
		// end: EndOfSpan = nodes[-1]
	},
	ArrayLiteral
	{
		num_elements: U24,
		// items: List<Expression> = nodes[-1]
	},
	Structural
	{
		unresolved_struct_or_word: TokenId,
		// field_initializers: List<IdentifierAndExpression> = nodes[-1]
	},
	Deref
	{
		start_of_reference: TokenId,
		// address_depth: DerefAddressDepth = nodes[-1]
		// base_identifier: Identifier = nodes[-2]
		// steps: List<DerefStep> = nodes[-3]
	},
	DerefAddressDepth
	{
		depth: u8,
	},
	DerefStepElement {
		// argument: Expression = nodes[..-1]
	},
	DerefStepMember
	{
		field_identifier: TokenId,
	},
	BitCast
	{
		cast_keyword: TokenId,
		// expression: Expression = nodes[..-1]
	},
	TypeCast
	{
		start_of_type: TokenId,
		// expression: Item<Expression> = nodes[-1]
		// coerced_type: ValueType = nodes[..-2]
	},
	LengthOf {
		// deref: Deref = nodes[..-1]
	},
	SizeOf {
		// queried_type: ValueType = nodes[..-1]
	},
	FunctionCall
	{
		// identifier: Identifier = nodes[-1]
		is_builtin: bool,
		// arguments: List<Expression> = nodes[-2]
	},
	SimpleValueType(ValueTypeKeyword),
	CompositeValueType
	{
		start: TokenId,
		// end: EndOfSpan = nodes[-1]
		// composite_type: VT = nodes[..-2]
	},
	ArrayVT
	{
		// element_type = nodes[-1]
		fixed_length: TokenId,
	},
	ArrayWithNamedLengthVT
	{
		// element_type = nodes[-1]
		named_length_identifier: TokenId,
	},
	SliceVT {
		// element_type = nodes[-1]
	},
	EndlessArrayVT {
		// element_type = nodes[-1]
	},
	ArraylikeVT {
		// element_type = nodes[-1]
	},
	UnresolvedStructOrWordVT
	{
		identifier: TokenId,
	},
	PointerVT {
		// deref_type = nodes[-1]
	},
	ViewVT {
		// deref_type = nodes[-1]
	},
	EndOfSpan
	{
		end: TokenId,
	},
	Item
	{
		at: NodeId, // ParseNode
	},
	List
	{
		first: NodeId, // ListItem
	},
	ListItem
	{
		// value: ParseNode = nodes[-1]
		next: NodeId,
	},
	UnpatchedListItem,
	NoMoreItems,
	Poison,
}
