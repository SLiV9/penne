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

pub(super) struct ParseBuffer<'buffer>
{
	num_nodes: usize,
	nodes: &'buffer mut [MaybeUninit<ParseNode>],

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
		let node_id = NodeId(U24::new(i as u32));
		self.nodes[i].write(node);
		self.num_nodes += 1;
		node_id
	}

	pub fn finish_declaration(&mut self)
	{
		let i = self.num_nodes;
		assert!(i < self.nodes.len());
		let node_id = NodeId(U24::new(i as u32));
		assert!(self.declarations.len() < self.declarations.capacity());
		self.declarations.push(node_id);
	}

	pub(super) fn push_error(
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
