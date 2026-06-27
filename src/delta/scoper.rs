pub mod labels;
mod namespace;
mod stack;
pub mod top_level;
pub mod variables;

use crate::delta::parser::parse_node::{NodeId, U24};

#[derive(Clone, Copy, Debug)]
pub struct ResolutionId(U24);

#[derive(Clone, Copy, Debug)]
pub struct LabelId(U24);

pub enum ScopingError
{
	DuplicateDeclaration
	{
		name: String,
		location: NodeId,
		previous: NodeId,
	},
	UndeclaredReference
	{
		name: String,
		location: NodeId,
	},
	Poisoned,
}
