use std::mem::MaybeUninit;

use super::parse_node::NodeId;
use super::parse_node::ParseNode;

use crate::delta::lexer::tokens;
use crate::delta::lexer::tokens::Tokens;
use crate::delta::parser::ParsingError;
use crate::delta::parser::parse_node::U24;

pub const MAX_NUM_PARSING_ERRORS: usize = 100;

pub struct ParseTree
{
	nodes: Vec<ParseNode>,

	declarations: Vec<NodeId>,

	errors: Vec<(ParsingError, tokens::TokenId, tokens::TokenId)>,
}

impl ParseTree
{
	pub(super) fn empty(
		tokens: &Tokens,
		num_possible_declarations: usize,
	) -> Self
	{
		// For nodes, we want to avoid the realloc at all costs.
		let num_tokens = tokens.base_tokens().len();
		let nodes = Vec::with_capacity(num_tokens);

		// The caller knows how many declarations there can be.
		let declarations = Vec::with_capacity(num_possible_declarations);

		// For errors we have MAX_NUM_PARSING_ERRORS as a hard cap
		// because there is no point showing the user millions of errors.
		let error_cap = std::cmp::min(num_tokens, MAX_NUM_PARSING_ERRORS);
		let errors = Vec::with_capacity(error_cap);

		Self {
			nodes,
			declarations,
			errors,
		}
	}

	pub(super) fn buffer(&mut self) -> ParseBuffer<'_>
	{
		let Self {
			nodes,
			declarations,
			errors,
		} = self;
		assert_eq!(nodes.len(), 0);
		assert_eq!(declarations.len(), 0);
		assert_eq!(errors.len(), 0);
		ParseBuffer {
			num_nodes: 0,
			nodes: nodes.spare_capacity_mut(),
			active_list: None,
			declarations,
			errors,
		}
	}

	/// SAFETY: The `num_nodes` argument MUST be the value returned by
	/// calling [ParseBuffer::into_num_initialized_nodes].
	pub(super) unsafe fn set_nodes_len(&mut self, num_nodes: usize)
	{
		assert_eq!(self.nodes.len(), 0);
		assert!(num_nodes <= self.nodes.capacity());
		// Safety: we have checked that `num_nodes` is less than the capacity
		// of the buffer.
		// The buffer is initialized in `push`, which is also the only
		// place where `num_nodes` is modified. The caller guarantees the
		// `num_nodes` argument comes from `into_num_initialized_nodes`.
		unsafe { self.nodes.set_len(num_nodes) };
	}
}

/// Safety invariant: all nodes in `self.nodes[..self.num_nodes]`
/// have been initialized.
pub(super) struct ParseBuffer<'buffer>
{
	num_nodes: usize,
	nodes: &'buffer mut [MaybeUninit<ParseNode>],

	active_list: Option<NodeId>,

	declarations: &'buffer mut Vec<NodeId>,

	errors: &'buffer mut Vec<(ParsingError, tokens::TokenId, tokens::TokenId)>,
}

impl<'buffer> ParseBuffer<'buffer>
{
	pub(super) fn into_num_initialized_nodes(self) -> usize
	{
		self.num_nodes
	}

	#[inline]
	pub(super) fn push(&mut self, node: ParseNode) -> NodeId
	{
		let i = self.num_nodes;
		if i >= self.nodes.len()
		{
			panic!("Number of parse nodes exceeds number of tokens");
		}
		let node_id = NodeId(U24::new(i));
		self.nodes[i].write(node);
		self.num_nodes += 1;
		node_id
	}

	#[inline]
	pub(super) fn expect_most_recent_node(&mut self, node: NodeId)
	{
		debug_assert_eq!(usize::from(node.0) + 1, self.num_nodes);
	}

	#[inline]
	pub(super) fn push_older_node(&mut self, node: NodeId)
	{
		self.push(ParseNode::Item { at: node });
	}

	#[inline]
	pub(super) fn push_list(&mut self, node: NodeId)
	{
		self.push(ParseNode::List { first: node });
	}

	#[inline]
	pub(super) fn start_list(&mut self)
	{
		debug_assert!(self.active_list.is_none());
	}

	#[inline]
	fn patch_list_item(&mut self, old_node: NodeId, new_content: ParseNode)
	{
		let i = usize::from(old_node.0);
		assert!(i < self.num_nodes);
		// Safety: `self.num_nodes` is only increased in `push`,
		// upholding the invariant of `self`.
		let old_node = unsafe { self.nodes[i].assume_init_mut() };
		debug_assert!(matches!(*old_node, ParseNode::UnpatchedListItem));
		*old_node = new_content;
	}

	#[inline]
	pub(super) fn push_list_item(&mut self, content_node: NodeId)
	{
		self.expect_most_recent_node(content_node);
		// This will be patched later, unless there are parse errors.
		let new_node = self.push(ParseNode::UnpatchedListItem);
		if let Some(old_node) = self.active_list.replace(new_node)
		{
			self.patch_list_item(
				old_node,
				ParseNode::ListItem { next: new_node },
			);
		}
	}

	#[inline]
	pub(super) fn push_end_of_list(&mut self) -> NodeId
	{
		let new_node = self.push(ParseNode::NoMoreItems);
		if let Some(old_node) = self.active_list.take()
		{
			self.patch_list_item(
				old_node,
				ParseNode::ListItem { next: new_node },
			);
		}
		new_node
	}

	#[inline]
	pub(super) fn push_none(&mut self) -> NodeId
	{
		self.push(ParseNode::NoMoreItems)
	}

	pub(super) fn finish_declaration(&mut self)
	{
		let i = self.num_nodes;
		assert!(i < self.nodes.len());
		let node_id = NodeId(U24::new(i));
		assert!(self.declarations.len() < self.declarations.capacity());
		self.declarations.push(node_id);
	}

	pub(super) fn store_error(
		&mut self,
		error: ParsingError,
		start: tokens::TokenId,
		end: tokens::TokenId,
	)
	{
		let i = self.errors.len();
		if i >= self.errors.capacity()
		{
			// We ignore errors after the first MAX_NUM_PARSING_ERRORS,
			// because there is no point showing the user all of them.
			return;
		}
		self.errors.push((error, start, end));
	}
}
