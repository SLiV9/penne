use std::mem::MaybeUninit;

use super::parse_node::NodeId;
use super::parse_node::ParseNode;

use crate::alpha::Errors;
use crate::alpha::common::DeclarationFlag;
use crate::alpha::error;
use crate::alpha::included;
use crate::delta::lexer::BaseToken;
use crate::delta::lexer::tokens::Tokens;
use crate::delta::parser::ParsingError;
use crate::delta::parser::parse_node::ParseNode::UnpatchedListItem;
use crate::delta::parser::parse_node::U24;

pub const MAX_NUM_PARSING_ERRORS: usize = 100;
pub(crate) const MAX_PARSE_NODE_CONTEXT: usize = 5;

#[path = "parse_tree_xml.rs"]
mod parse_tree_xml;

#[derive(Debug)]
pub struct ParseTree
{
	nodes: Vec<ParseNode>,

	declarations: Vec<NodeId>,

	parsing_errors: Vec<ParsingError>,
	analysis_errors: Vec<error::Error>,
}

impl ParseTree
{
	pub(super) fn empty(
		tokens: &Tokens,
		num_possible_declarations: usize,
	) -> Self
	{
		// For nodes, we want to avoid the realloc at all costs.
		// TODO so 1 is too small, 2 is very likely true but a bit of a magic number
		let num_tokens =
			MAX_PARSE_NODE_CONTEXT + 2 * tokens.base_tokens().len();
		let nodes = Vec::with_capacity(num_tokens);

		// The caller knows how many declarations there can be.
		let declarations = Vec::with_capacity(num_possible_declarations);

		// For errors we have MAX_NUM_PARSING_ERRORS as a hard cap
		// because there is no point showing the user millions of errors.
		let error_cap = std::cmp::min(num_tokens, MAX_NUM_PARSING_ERRORS);
		let parsing_errors = Vec::with_capacity(error_cap);

		// Don't reserve post-parse errors.
		let analysis_errors = Vec::new();

		Self {
			nodes,
			declarations,
			parsing_errors,
			analysis_errors,
		}
	}

	pub(super) fn buffer(&mut self) -> ParseBuffer<'_>
	{
		let Self {
			nodes,
			declarations,
			parsing_errors: errors,
			analysis_errors,
		} = self;
		assert_eq!(nodes.len(), 0);
		assert_eq!(declarations.len(), 0);
		assert_eq!(errors.len(), 0);
		assert_eq!(analysis_errors.len(), 0);
		ParseBuffer {
			num_nodes: 0,
			nodes: nodes.spare_capacity_mut(),
			active_private_zone: None,
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

		if self.nodes.len() * 2 < self.nodes.capacity()
		{
			self.nodes.shrink_to_fit();
		}
		if self.declarations.len() * 2 < self.declarations.capacity()
		{
			self.declarations.shrink_to_fit();
		}
	}
}

/// Safety invariant: all nodes in `self.nodes[..self.num_nodes]`
/// have been initialized.
pub(super) struct ParseBuffer<'buffer>
{
	num_nodes: usize,
	nodes: &'buffer mut [MaybeUninit<ParseNode>],

	active_private_zone: Option<NodeId>,

	declarations: &'buffer mut Vec<NodeId>,

	errors: &'buffer mut Vec<ParsingError>,
}

#[must_use]
pub struct ActiveList
{
	first_node: NodeId,
	last_node: NodeId,
}

#[must_use]
pub struct UnfinishedImpl
{
	unfinished_node: NodeId,
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
			panic!("Number of parse nodes greatly exceeds number of tokens");
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
		let _: NodeId = self.push(ParseNode::Item { at: node });
	}

	#[inline]
	pub(super) fn push_optional_node(&mut self, node_id: Option<NodeId>)
	{
		if let Some(node_id) = node_id
		{
			self.push_older_node(node_id);
		}
		else
		{
			self.push_undeclared(ParseNode::NoMoreItems)
		}
	}

	#[inline]
	pub(super) fn push_list(&mut self, node: NodeId)
	{
		let _: NodeId = self.push(ParseNode::List { first: node });
	}

	#[inline]
	pub(super) fn start_list(&mut self) -> Option<ActiveList>
	{
		None
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
	pub(super) fn push_list_item(
		&mut self,
		content_node: NodeId,
		active_list: &mut Option<ActiveList>,
	)
	{
		self.expect_most_recent_node(content_node);
		// This will be patched later, unless there are parse errors.
		let new_node = self.push(ParseNode::UnpatchedListItem);
		*active_list = match *active_list
		{
			Some(ActiveList {
				first_node,
				last_node,
			}) =>
			{
				self.patch_list_item(
					last_node,
					ParseNode::ListItem { next: new_node },
				);
				Some(ActiveList {
					first_node,
					last_node: new_node,
				})
			}
			None => Some(ActiveList {
				first_node: new_node,
				last_node: new_node,
			}),
		};
	}

	#[inline]
	pub(super) fn push_end_of_list(
		&mut self,
		active_list: Option<ActiveList>,
	) -> NodeId
	{
		let new_node = self.push(ParseNode::NoMoreItems);
		if let Some(ActiveList {
			first_node,
			last_node,
		}) = active_list
		{
			self.patch_list_item(
				last_node,
				ParseNode::ListItem { next: new_node },
			);
			first_node
		}
		else
		{
			new_node
		}
	}

	#[inline]
	pub(super) fn push_unfinished_impl(&mut self) -> UnfinishedImpl
	{
		let unfinished_node = self.push(UnpatchedListItem);
		UnfinishedImpl { unfinished_node }
	}

	#[inline]
	pub(super) fn finish_impl(
		&mut self,
		UnfinishedImpl { unfinished_node }: UnfinishedImpl,
		content: ParseNode,
	)
	{
		self.patch_list_item(unfinished_node, content);
	}

	#[inline]
	pub(super) fn push_undeclared(&mut self, node: ParseNode)
	{
		let _: NodeId = self.push(node);
	}

	#[inline]
	pub(super) fn set_private(&mut self)
	{
		if self.active_private_zone.is_none()
		{
			let node = self.push(ParseNode::EndlessPrivateZone);
			self.active_private_zone = Some(node);
		}
	}

	#[inline]
	pub(super) fn set_public(&mut self)
	{
		if let Some(start) = self.active_private_zone.take()
		{
			let end = self.push(ParseNode::EndPrivateZone { start });
			self.patch_start_of_private_zone(
				start,
				ParseNode::StartPrivateZone { end },
			);
		}
	}

	#[inline]
	fn patch_start_of_private_zone(
		&mut self,
		old_node: NodeId,
		new_content: ParseNode,
	)
	{
		let i = usize::from(old_node.0);
		assert!(i < self.num_nodes);
		// Safety: `self.num_nodes` is only increased in `push`,
		// upholding the invariant of `self`.
		let old_node = unsafe { self.nodes[i].assume_init_mut() };
		debug_assert!(matches!(*old_node, ParseNode::EndlessPrivateZone));
		*old_node = new_content;
	}

	pub(super) fn finish_declaration(&mut self, node: NodeId)
	{
		assert!(self.declarations.len() < self.declarations.capacity());
		self.declarations.push(node);
	}

	pub(super) fn store_error(&mut self, error: ParsingError)
	{
		let i = self.errors.len();
		if i >= self.errors.capacity()
		{
			// We ignore errors after the first MAX_NUM_PARSING_ERRORS,
			// because there is no point showing the user all of them.
			return;
		}
		self.errors.push(error);
	}
}

impl ParseTree
{
	pub fn num_parse_nodes(&self) -> usize
	{
		self.nodes.len()
	}

	pub fn num_declarations(&self) -> usize
	{
		self.declarations.len()
	}

	#[inline(never)]
	pub fn build_header(&self) -> ParseTree
	{
		assert!(self.parsing_errors.is_empty());
		let mut nodes = Vec::with_capacity(self.nodes.len());
		self.build_header_nodes(&mut nodes);
		let mut declarations = Vec::with_capacity(self.declarations.len());
		for (i, node) in nodes.iter().enumerate()
		{
			if node.is_declaration()
			{
				declarations.push(NodeId(U24::new(i)));
			}
		}
		ParseTree {
			nodes,
			declarations,
			parsing_errors: Vec::new(),
			analysis_errors: Vec::new(),
		}
	}

	#[inline(never)]
	fn build_header_nodes(&self, nodes: &mut Vec<ParseNode>)
	{
		assert_eq!(nodes.capacity(), self.nodes.len());
		let buffer = nodes.spare_capacity_mut();
		let mut num_public_nodes = 0;
		let mut num_skipped_nodes = 0;
		let mut push = |node| {
			buffer[num_public_nodes].write(node);
			num_public_nodes += 1;
		};
		let mut i = 0;
		while i < self.nodes.len()
		{
			match self.nodes[i]
			{
				ParseNode::StartPrivateZone { end } =>
				{
					let end = usize::from(end.0);
					num_skipped_nodes += end + 1 - i;
					i = end;
					debug_assert!(matches!(
						self.nodes[i],
						ParseNode::EndPrivateZone { .. }
					));
					i += 1;
					continue;
				}
				ParseNode::EndPrivateZone { .. } =>
				{
					debug_assert!(false, "unreachable");
					break;
				}
				ParseNode::EndlessPrivateZone => break,
				node =>
				{
					push(node.convert_for_head(num_skipped_nodes));
					i += 1;
				}
			}
		}
		// Safety: `num_public_nodes` is only modified in the `push` closure.
		unsafe { nodes.set_len(num_public_nodes) };
	}

	pub fn append_header(&mut self, other: &ParseTree)
	{
		let ParseTree {
			nodes: other_nodes,
			declarations: other_declarations,
			parsing_errors: other_parsing_errors,
			analysis_errors: other_analysis_errors,
		} = other;
		assert!(other_parsing_errors.is_empty());
		assert!(other_analysis_errors.is_empty());
		let old_num_nodes = self.nodes.len();
		let old_num_declarations = self.declarations.len();
		// TODO if node id would exceed U24 bounds, only store an error
		self.nodes.copy_from_slice(other_nodes);
		self.declarations.copy_from_slice(other_declarations);
		for node in &mut self.nodes[old_num_nodes..]
		{
			*node = node.convert_for_append(old_num_nodes);
		}
		for declaration in &mut self.declarations[old_num_declarations..]
		{
			let i = usize::from(declaration.0);
			declaration.0 = U24::new(i + old_num_nodes);
		}
	}

	pub fn process_imports(
		&mut self,
		tokens: &Tokens,
		source: &str,
		mut callback: impl FnMut(&std::path::Path) -> Result<(), ()>,
	)
	{
		for decl_node_id in &self.declarations
		{
			let i = usize::from(decl_node_id.0);
			let decl_node = self.nodes[i];
			let context: &[ParseNode; MAX_PARSE_NODE_CONTEXT] =
				self.nodes[..i].last_chunk().expect("padding");
			dbg!(decl_node, context);
			match (decl_node, context)
			{
				(
					ParseNode::ImportDeclaration {
						start_of_declaration,
					},
					[_, _, x2, x1, flags],
				) =>
				{
					let ParseNode::DeclarationFlags(flags) = flags
					else
					{
						unreachable!("parsing produced invalid parse tree")
					};
					dbg!(start_of_declaration);
					dbg!(flags);
					if flags.contains(DeclarationFlag::Public)
					{
						// TODO nicer location
						self.analysis_errors.push(error::Error::PublicImport {
							location: tokens
								.get_location(start_of_declaration.into()),
						});
					}
					let import_string = Self::get_string_literal_contents(
						*x1, *x2, tokens, source,
					);
					let filename = import_string.clone();
					let import = std::path::PathBuf::from(import_string);
					let result = callback(&import);
					match result
					{
						Ok(()) => (),
						Err(()) =>
						{
							// TODO nicer location
							let location = tokens
								.get_location(start_of_declaration.into());
							let hint = included::source_name_hint(&filename)
								.map(str::to_string);
							let error = match hint
							{
								Some(hinted_package_name) =>
								{
									error::Error::UnresolvedImportWithHint {
										filename,
										location,
										hinted_package_name,
									}
								}
								None => error::Error::UnresolvedImport {
									filename,
									location,
								},
							};
							self.analysis_errors.push(error);
						}
					}
				}
				_ => (),
			}
		}
	}

	fn get_string_literal_contents(
		string_literal_node: ParseNode,
		support_node: ParseNode,
		tokens: &Tokens,
		source: &str,
	) -> String
	{
		let location = match string_literal_node
		{
			ParseNode::SimpleStringLiteral { literal } =>
			{
				tokens.get_location(literal.into())
			}
			ParseNode::CompositeStringLiteral { start } =>
			{
				let ParseNode::EndOfSpan { end } = support_node
				else
				{
					unreachable!("parsing produced invalid parse tree")
				};
				tokens.get_location_of_span(start.into()..end.into())
			}
			_ => unreachable!("parsing produced invalid parse tree"),
		};
		// TODO simple string literal contains quotes
		// TODO composite contains internal quotes
		// TODO parse escapes and everything
		// TODO doing this in the middle of everything else seems awful
		// TODO I've made lexing faster but everything else worse
		// TODO in fact if I don't store the result then I need to do it multiple times probably
		// TODO or maybe I can add a separate string internalizer step
		source[location.span].to_string()
	}
}

impl ParseTree
{
	pub fn drain_errors(&mut self, tokens: &Tokens) -> Option<Errors>
	{
		if self.parsing_errors.is_empty() && self.analysis_errors.is_empty()
		{
			return None;
		}

		let errors = (self.parsing_errors.drain(..))
			.map(|error| build_error(error, tokens))
			.chain(self.analysis_errors.drain(..))
			.collect();

		Some(Errors { errors })
	}
}

fn build_error(error: ParsingError, tokens: &Tokens) -> error::Error
{
	match error
	{
		ParsingError::UnexpectedToken { token, expectation }
			if tokens.get(token) == BaseToken::EndOfSource =>
		{
			error::Error::UnexpectedEndOfFile {
				location: tokens.get_location(token),
				last_location: tokens.get_location_of_previous_token(token),
				expectation: expectation.to_string(),
			}
		}
		ParsingError::UnexpectedToken { token, expectation } =>
		{
			error::Error::UnexpectedToken {
				location: tokens.get_location(token),
				expectation: expectation.to_string(),
			}
		}
		ParsingError::UnexpectedSemicolonAfterIdentifier {
			semicolon,
			identifier_start,
		} => error::Error::UnexpectedSemicolonAfterIdentifier {
			location: tokens.get_location(semicolon),
			after: tokens.get_location_of_span(identifier_start..semicolon),
		},
		ParsingError::UnexpectedSemicolonAfterReturnValue {
			semicolon,
			return_value_start,
		} => error::Error::UnexpectedSemicolonAfterReturnValue {
			location: tokens.get_location(semicolon),
			after: tokens.get_location_of_span(return_value_start..semicolon),
		},
		ParsingError::MissingReturnValueAfterStatement {
			unexpected_token,
			return_statement_start,
		} => error::Error::MissingReturnValueAfterStatement {
			location: tokens.get_location(unexpected_token),
			after: tokens
				.get_location_of_span(return_statement_start..unexpected_token),
		},
		ParsingError::MissingConstantType { unexpected_token } =>
		{
			error::Error::MissingConstantType {
				location: tokens.get_location(unexpected_token),
			}
		}
		ParsingError::MissingParameterType { unexpected_token } =>
		{
			error::Error::MissingParameterType {
				location: tokens.get_location(unexpected_token),
			}
		}
		ParsingError::MissingMemberType { unexpected_token } =>
		{
			error::Error::MissingMemberType {
				location: tokens.get_location(unexpected_token),
			}
		}
		ParsingError::MaximumParseDepthExceeded { start, end } =>
		{
			error::Error::MaximumParseDepthExceeded {
				location: tokens.get_location_of_span(start..end),
			}
		}
	}
}
