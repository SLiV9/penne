use crate::delta::parser::parse_node::NodeId;

use super::ScopingError;

#[derive(Default)]
pub struct IdentifierStack
{
	names: Vec<String>,
	locations: Vec<NodeId>,
	stack_lengths: Vec<usize>,
}

impl IdentifierStack
{
	pub fn insert(
		&mut self,
		name: String,
		location: NodeId,
	) -> Result<(), ScopingError>
	{
		let start_of_scope = self.stack_lengths.last().copied().unwrap_or(0);
		assert!(start_of_scope <= self.names.len());
		for i in start_of_scope..self.names.len()
		{
			if &self.names[i] == &name
			{
				return Err(ScopingError::DuplicateDeclaration {
					name,
					location,
					previous: self.locations[i],
				});
			}
		}
		self.names.push(name);
		Ok(())
	}

	pub fn push_scope(&mut self)
	{
		self.stack_lengths.push(self.names.len());
	}

	pub fn pop_scope(&mut self)
	{
		let Some(n) = self.stack_lengths.pop()
		else
		{
			panic!("Called `pop_scope` more often than `push_scope`");
		};
		assert!(n <= self.names.len());
		self.names.truncate(n);
	}
}
