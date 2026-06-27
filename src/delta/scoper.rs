use crate::delta::parser::parse_node::{NodeId, U24};

#[derive(Clone, Copy, Debug)]
pub struct ResolutionId(U24);

pub enum ScopingError
{
	DuplicateDeclaration
	{
		name: String,
		location: NodeId,
		previous: NodeId,
	},
	Poisoned,
}

#[derive(Default)]
struct IdentifierStack
{
	names: Vec<String>,
	locations: Vec<NodeId>,
	stack_lengths: Vec<usize>,
}

impl IdentifierStack
{
	fn insert(
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

	fn push_scope(&mut self)
	{
		self.stack_lengths.push(self.names.len());
	}

	fn pop_scope(&mut self)
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

#[derive(Default)]
pub struct Scoper
{
	variables: IdentifierStack,
	user_types: IdentifierStack,
	user_type_members: IdentifierStack,
	functions: IdentifierStack,
	resolution_id: usize,
}

// TOOD scope labels backwards

impl Scoper
{
	fn new_resolution_id(&mut self) -> ResolutionId
	{
		self.resolution_id += 1;
		ResolutionId(U24::new(self.resolution_id))
	}

	pub fn declare_structural(
		&mut self,
		name: String,
		location: NodeId,
	) -> Result<ResolutionId, ScopingError>
	{
		self.user_types.insert(name, location)?;
		self.user_type_members.push_scope();
		Ok(self.new_resolution_id())
	}

	pub fn declare_member_in_latest_structural(
		&mut self,
		name: String,
		location: NodeId,
	) -> Result<ResolutionId, ScopingError>
	{
		self.user_type_members.insert(name, location)?;
		Ok(self.new_resolution_id())
	}

	pub fn declare_constant(
		&mut self,
		name: String,
		location: NodeId,
	) -> Result<ResolutionId, ScopingError>
	{
		debug_assert!(self.variables.stack_lengths.is_empty());
		self.variables.insert(name, location)?;
		Ok(self.new_resolution_id())
	}

	pub fn declare_function(
		&mut self,
		name: String,
		location: NodeId,
	) -> Result<ResolutionId, ScopingError>
	{
		self.functions.insert(name, location)?;
		Ok(self.new_resolution_id())
	}
}
