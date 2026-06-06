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
	#[inline(always)]
	pub fn new(value: usize) -> Self
	{
		debug_assert!(value < MAX_NUM_NODES);
		let value = value as u32;
		let [x0, x1, x2, _x3] = value.to_le_bytes();
		Self([x0, x1, x2])
	}
}

impl From<U24> for u32
{
	#[inline(always)]
	fn from(value: U24) -> Self
	{
		let [x0, x1, x2] = value.0;
		let bytes = [x0, x1, x2, 0];
		u32::from_le_bytes(bytes)
	}
}

impl From<U24> for usize
{
	#[inline(always)]
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
	NoMoreItems,
	DeclarationFlags(EnumSet<DeclarationFlag>),
	FunctionDeclaration
	{
		start_of_declaration: TokenId,
		// flags: DeclarationFlags = nodes[-1]
		// identifier: Identifier = nodes[-2]
		// parameters: List<IdentifierAndType> = nodes[-3]
		// return_type: Item<ValueType> = nodes[-4]
		// body: FunctionBodyBlock/NoMoreItems = nodes[-5]
	},
	ConstantDeclaration
	{
		start_of_declaration: TokenId,
		// flags: DeclarationFlags = nodes[-1]
		// identifier: Identifier = nodes[-2]
		// value_type: Item<ValueType> = nodes[-3]
		// expression: Expression = nodes[..-4]
	},
	StructureDeclaration
	{
		start_of_declaration: TokenId,
		// flags: DeclarationFlags = nodes[-1]
		// identifier: Identifier = nodes[-2]
		// structural_type: StructuralType = nodes[-3]
		// members: List<IdentifierAndType> = nodes[-4]
	},
	ImportDeclaration
	{
		start_of_declaration: TokenId,
		// flags: DeclarationFlags = nodes[-1]
		// path: StringLiteral = nodes[-2]
	},
	FunctionBody {
		// statements: List<Statement> = nodes[-1]
		// return_value: Item<Expression>/NoMoreItems = nodes[-2]
	},
	StructuralType
	{
		size_in_bytes_if_word: Option<u8>,
	},
	Identifier
	{
		identifier: TokenId,
	},
	IdentifierAndType
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
		colon: TokenId,
	},
	MethodCall
	{
		// identifier: Identifier = nodes[-1]
		is_builtin: bool,
		// arguments: List<Expression> = nodes[-2]
	},
	FunctionCall
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
	UnpatchedListItem,
	Then {
		// then: Statement = nodes[..-1]
	},
	// Nodes that contain NodeId:
	ThenElse
	{
		then: NodeId,
		// else: Statement = nodes[..-1]
	},
	If
	{
		comparison: NodeId,
		// then: Then/ThenElse = nodes[-1]
	},
	Block
	{
		first: NodeId, // ListItem<Statement>
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
	FunctionImpl
	{
		body: NodeId, // FunctionBody
	},
	// Markers for build_header():
	StartPrivateZone
	{
		end: NodeId,
	},
	EndPrivateZone
	{
		start: NodeId,
	},
	EndlessPrivateZone,
}

impl ParseNode
{
	#[inline]
	pub(crate) fn is_declaration(self) -> bool
	{
		use ParseNode::*;
		matches!(
			self,
			ConstantDeclaration { .. }
				| FunctionDeclaration { .. }
				| StructureDeclaration { .. }
				| ImportDeclaration { .. }
		)
	}

	#[inline]
	pub(crate) fn convert_for_head(self, num_skipped_nodes: usize) -> Self
	{
		let adjust = |node_id: NodeId| -> NodeId {
			let i = usize::from(node_id.0);
			debug_assert!(i >= num_skipped_nodes);
			NodeId(U24::new(i - num_skipped_nodes))
		};

		use ParseNode::*;
		match self
		{
			NoMoreItems => self,
			DeclarationFlags(enum_set) => DeclarationFlags(
				enum_set.difference(EnumSet::from(DeclarationFlag::Public)),
			),
			FunctionDeclaration { .. } => self,
			ConstantDeclaration { .. } => self,
			StructureDeclaration { .. } => self,
			ImportDeclaration { .. } => self,
			FunctionBody { .. } => self,
			StructuralType { .. } => self,
			Identifier { .. } => self,
			IdentifierAndType { .. } => self,
			IdentifierAndExpression { .. } => self,
			VariableDeclaration { .. } => self,
			Assignment {} => self,
			Loop { .. } => self,
			Goto { .. } => self,
			Label { .. } => self,
			MethodCall { .. } => self,
			FunctionCall { .. } => self,
			Parenthesized {} => self,
			Comparison { .. } => self,
			ComparisonOp(..) => self,
			Binary { .. } => self,
			BinaryOp(..) => self,
			Unary { .. } => self,
			UnaryOp(..) => self,
			BooleanLiteral { .. } => self,
			CharLiteral { .. } => self,
			UntypedIntegerLiteral { .. } => self,
			TypedIntegerLiteral { .. } => self,
			SimpleStringLiteral { .. } => self,
			CompositeStringLiteral { .. } => self,
			ArrayLiteral { .. } => self,
			Structural { .. } => self,
			Deref { .. } => self,
			DerefAddressDepth { .. } => self,
			DerefStepElement {} => self,
			DerefStepMember { .. } => self,
			BitCast { .. } => self,
			TypeCast { .. } => self,
			LengthOf {} => self,
			SizeOf {} => self,
			SimpleValueType(..) => self,
			CompositeValueType { .. } => self,
			ArrayVT { .. } => self,
			ArrayWithNamedLengthVT { .. } => self,
			SliceVT {} => self,
			EndlessArrayVT {} => self,
			ArraylikeVT {} => self,
			UnresolvedStructOrWordVT { .. } => self,
			PointerVT {} => self,
			ViewVT {} => self,
			EndOfSpan { .. } => self,
			UnpatchedListItem => self,
			Then {} => self,
			ThenElse { then } => ThenElse { then: adjust(then) },
			If { comparison } => If {
				comparison: adjust(comparison),
			},
			Block { first } => Block {
				first: adjust(first),
			},
			Item { at } => Item { at: adjust(at) },
			List { first } => List {
				first: adjust(first),
			},
			ListItem { next } => ListItem { next: adjust(next) },
			FunctionImpl { body: _ } => NoMoreItems,
			StartPrivateZone { .. }
			| EndPrivateZone { .. }
			| EndlessPrivateZone =>
			{
				debug_assert!(false, "unreachable");
				self
			}
		}
	}
}
