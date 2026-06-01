use enumset::EnumSet;

use super::ParseTree;

use crate::alpha::common::DeclarationFlag;
use crate::delta::lexer::tokens::Tokens;
use crate::delta::parser::parse_node::{self, NodeId, ParseNode, U24};
use crate::delta::parser::parse_tree::MAX_PARSE_NODE_CONTEXT;

impl ParseTree
{
	pub fn as_xml(
		&self,
		tokens: &Tokens,
		source: &str,
	) -> impl Iterator<Item = String>
	{
		self.declarations.iter().flat_map(|&node_id| {
			print_xml(&self.nodes, node_id, tokens, source)
		})
	}
}

fn print_xml(
	nodes: &[ParseNode],
	node_id: NodeId,
	tokens: &Tokens,
	source: &str,
) -> Box<dyn Iterator<Item = String>>
{
	use ParseNode::*;
	use std::iter::once;

	let get_source = |token_id: parse_node::TokenId| {
		let location = tokens.get_location(token_id.into());
		&source[location.span]
	};

	let get_span_source = |start: parse_node::TokenId,
	                       end: parse_node::TokenId| {
		let location = tokens.get_location_of_span(start.into()..end.into());
		&source[location.span]
	};

	let get_value_type = |token_id: parse_node::TokenId| {
		let vap = tokens.get_value_type_and_payload(token_id.into());
		vap.value_type()
	};
	let get_integer_value = |token_id: parse_node::TokenId| {
		let vap = tokens.get_value_type_and_payload(token_id.into());
		tokens.get_integer_payload(vap.payload_id()).unwrap()
	};

	let print_flags = |flags: EnumSet<DeclarationFlag>| {
		flags
			.into_iter()
			.map(|flag| format!("{flag:?}"))
			.collect::<Vec<_>>()
			.join("|")
	};

	let print_item =
		|node_id: NodeId| print_xml(nodes, node_id, tokens, source);
	let print_prev = |node_id: usize| print_item(NodeId(U24::new(node_id)));
	let print_list = |node_id: NodeId, meta: &'static str| {
		Box::new(
			once(format!("<List meta=\"{meta}\">"))
				.chain(print_xml(nodes, node_id, tokens, source))
				.chain(once(format!("</List>"))),
		)
	};

	let i = usize::from(node_id.0);
	let node = nodes[i];
	let context: &[ParseNode; MAX_PARSE_NODE_CONTEXT] =
		nodes[..i].last_chunk().expect("padding");
	match (node, *context)
	{
		(Padding, _) => Box::new(once(format!("<Padding />"))),

		(
			ConstantDeclaration { .. },
			[
				_,
				_,
				_expression,
				Item { at: value_type },
				Identifier { identifier },
				DeclarationFlags(flags),
			],
		) => Box::new(
			once(format!(
				"<ConstantDeclaration identifier={:?} flags=\"{}\">",
				get_source(identifier),
				print_flags(flags),
			))
			.chain(print_prev(i - 4))
			.chain(print_item(value_type))
			.chain(once(format!("</ConstantDeclaration>"))),
		),

		(
			FunctionDeclaration { .. },
			[
				Item { at: return_value },
				List { first: statements },
				Item { at: return_type },
				List { first: parameters },
				Identifier { identifier },
				DeclarationFlags(flags),
			],
		) => Box::new(
			once(format!(
				"<FunctionDeclaration identifier={:?} flags=\"{}\">",
				get_source(identifier),
				print_flags(flags),
			))
			.chain(print_list(parameters, "parameters"))
			.chain(print_item(return_type))
			.chain(print_list(statements, "statements"))
			.chain(print_item(return_value))
			.chain(once(format!("</FunctionDeclaration>"))),
		),

		(
			FunctionHeadDeclaration { .. },
			[
				Padding,
				Padding,
				Item { at: return_type },
				List { first: parameters },
				Identifier { identifier },
				DeclarationFlags(flags),
			],
		) => Box::new(
			once(format!(
				"<FunctionHeadDeclaration identifier={:?} flags=\"{}\">",
				get_source(identifier),
				print_flags(flags),
			))
			.chain(print_list(parameters, "parameters"))
			.chain(print_item(return_type))
			.chain(once(format!("</FunctionHeadDeclaration>"))),
		),

		(
			StructureDeclaration { .. },
			[
				_,
				_,
				List { first: members },
				StructuralType {
					size_in_bytes_if_word,
				},
				Identifier { identifier },
				DeclarationFlags(flags),
			],
		) => Box::new(
			once(format!(
				"<StructureDeclaration identifier={:?} flags=\"{}\" \
				 size-in-bytes=\"{}\">",
				get_source(identifier),
				print_flags(flags),
				size_in_bytes_if_word.map_or(-1, i32::from)
			))
			.chain(print_list(members, "members"))
			.chain(once(format!("</StructureDeclaration>"))),
		),

		(
			ImportDeclaration { .. },
			[_, _, _, _, _, DeclarationFlags(flags)],
		) => Box::new(
			once(format!(
				"<ImportDeclaration flags=\"{}\">",
				print_flags(flags),
			))
			.chain(print_xml(nodes, NodeId(U24::new(i - 2)), tokens, source))
			.chain(once(format!("</ImportDeclaration>"))),
		),

		(Identifier { identifier }, _) => Box::new(once(format!(
			"<Identifier src={:?} />",
			get_source(identifier)
		))),
		(IdentifierAndType { identifier }, _) => Box::new(
			once(format!(
				"<IdentifierAndType src={:?}>",
				get_source(identifier)
			))
			.chain(print_prev(i - 1))
			.chain(once(format!("</IdentifierAndType>"))),
		),
		(IdentifierAndExpression { identifier }, _) => Box::new(
			once(format!(
				"<IdentifierAndExpression src={:?} />",
				get_source(identifier)
			))
			.chain(print_prev(i - 1))
			.chain(once(format!("</IdentifierAndExpression>"))),
		),
		(VariableDeclaration { identifier }, _) => Box::new(
			once(format!(
				"<VariableDeclaration src={:?}>",
				get_source(identifier)
			))
			.chain(print_prev(i - 1))
			.chain(print_prev(i - 2))
			.chain(once(format!("</VariableDeclaration>"))),
		),
		(Assignment {}, _) => Box::new(
			once(format!("<Assignment>"))
				.chain(print_prev(i - 1))
				.chain(print_prev(i - 2))
				.chain(once(format!("</Assignment>"))),
		),
		(Loop { token: _ }, _) => Box::new(once(format!("<Loop />"))),
		(Goto { token: _ }, [_, _, _, _, _, Identifier { identifier }]) =>
		{
			Box::new(once(format!(
				"<Goto label={:?} />",
				get_source(identifier)
			)))
		}
		(Label { colon: _ }, [_, _, _, _, _, Identifier { identifier }]) =>
		{
			Box::new(once(format!(
				"<Label src={:?} />",
				get_source(identifier)
			)))
		}
		(If { comparison }, _) => Box::new(
			once(format!("<If>"))
				.chain(print_item(comparison))
				.chain(print_prev(i - 1))
				.chain(once(format!("</If>"))),
		),
		(Then {}, _) => Box::new(
			once(format!("<Then>"))
				.chain(print_prev(i - 1))
				.chain(once(format!("</Then>"))),
		),
		(ThenElse { then }, _) => Box::new(
			once(format!("<Then>"))
				.chain(print_item(then))
				.chain(once(format!("</Then>")))
				.chain(once(format!("<Else>")))
				.chain(print_prev(i - 1))
				.chain(once(format!("</Else>"))),
		),
		(Block { first }, _) => Box::new(
			once(format!("<Block>"))
				.chain(print_list(first, "statements"))
				.chain(once(format!("</Block>"))),
		),

		(Parenthesized {}, _) => Box::new(
			once(format!("<Parenthesized>"))
				.chain(print_prev(i - 1))
				.chain(once(format!("</Parenthesized>"))),
		),
		(
			Comparison { token: _ },
			[_, _, _, _, Item { at: left }, ComparisonOp(op)],
		) => Box::new(
			once(format!("<Comparison op=\"{op:?}\">"))
				.chain(print_item(left))
				.chain(print_prev(i - 3))
				.chain(once(format!("</Comparison>"))),
		),
		(
			Binary { token: _ },
			[_, _, _, _, Item { at: left }, BinaryOp(op)],
		) => Box::new(
			once(format!("<Binary op=\"{op:?}\">"))
				.chain(print_item(left))
				.chain(print_prev(i - 3))
				.chain(once(format!("</Binary>"))),
		),
		(Unary { token: _ }, [_, _, _, _, _, UnaryOp(op)]) => Box::new(
			once(format!("<Unary op=\"{op:?}\">"))
				.chain(print_prev(i - 2))
				.chain(once(format!("</Unary>"))),
		),

		(BooleanLiteral { literal }, _) => Box::new(once(format!(
			"<BooleanLiteral src={:?} value=\"{}\" />",
			get_source(literal),
			get_integer_value(literal),
		))),
		(CharLiteral { literal }, _) => Box::new(once(format!(
			"<CharLiteral src={:?} value=\"{}\" />",
			get_source(literal),
			get_integer_value(literal),
		))),
		(UntypedIntegerLiteral { literal }, _) => Box::new(once(format!(
			"<UntypedIntegerLiteral src={:?} value=\"{}\" />",
			get_source(literal),
			get_integer_value(literal),
		))),
		(TypedIntegerLiteral { literal }, _) => Box::new(once(format!(
			"<UntypedIntegerLiteral src={:?} value=\"{}\" type=\"{:?}\" />",
			get_source(literal),
			get_integer_value(literal),
			get_value_type(literal),
		))),

		(SimpleStringLiteral { literal }, _) => Box::new(once(format!(
			"<SimpleStringLiteral src={:?} />",
			get_source(literal).trim_matches('"')
		))),

		(
			CompositeStringLiteral { start },
			[_, _, _, _, _, EndOfSpan { end }],
		) => Box::new(
			once(format!("<CompositeStringLiteral>"))
				.chain(once(format!("{:?}", get_span_source(start, end))))
				.chain(once(format!("</CompositeStringLiteral>"))),
		),

		(
			Deref {
				start_of_reference: _,
			},
			[
				_,
				_,
				_,
				List { first: steps },
				Identifier { identifier },
				DerefAddressDepth { depth },
			],
		) => Box::new(
			once(format!(
				"<Deref address_depth=\"{depth}\" identifier={:?}>",
				get_source(identifier)
			))
			.chain(print_list(steps, "steps"))
			.chain(once(format!("</Deref>"))),
		),

		(BitCast { cast_keyword: _ }, _) => Box::new(
			once(format!("<BitCast>"))
				.chain(print_prev(i - 1))
				.chain(once(format!("</BitCast>"))),
		),
		(TypeCast { start_of_type: _ }, _) => Box::new(
			once(format!("<BitCast>"))
				.chain(print_prev(i - 1))
				.chain(print_prev(i - 2))
				.chain(once(format!("</TypeCast>"))),
		),
		(LengthOf {}, _) => Box::new(
			once(format!("<LengthOf>"))
				.chain(print_prev(i - 1))
				.chain(once(format!("</LengthOf>"))),
		),
		(SizeOf {}, _) => Box::new(
			once(format!("<SizeOf>"))
				.chain(print_prev(i - 1))
				.chain(once(format!("</SizeOf>"))),
		),

		(SimpleValueType(value_type), _) => Box::new(once(format!(
			"<UnresolvedStructOrWordVT type=\"{value_type:?}\" />"
		))),
		(UnresolvedStructOrWordVT { identifier }, _) =>
		{
			Box::new(once(format!(
				"<UnresolvedStructOrWordVT src={:?} />",
				get_source(identifier)
			)))
		}

		(Item { at }, _) => print_item(at),
		(ListItem { next }, _) =>
		{
			Box::new(print_prev(i - 1).chain(print_item(next)))
		}
		(NoMoreItems, _) => Box::new(std::iter::empty()),

		_ => Box::new(once(format!("<MALFORMED node=\"{node:?}\" />"))),
	}
}
