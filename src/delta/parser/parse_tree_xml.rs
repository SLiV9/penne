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

		(ListItem { next }, _) =>
		{
			Box::new(print_prev(i - 1).chain(print_item(next)))
		}
		(NoMoreItems, _) => Box::new(std::iter::empty()),

		_ => Box::new(once(format!("<MALFORMED node=\"{node:?}\" />"))),
	}
}
