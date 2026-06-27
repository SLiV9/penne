use crate::delta::{
	parser::parse_node::{NodeId, U24},
	scoper::{ResolutionId, ScopingError, stack::IdentifierStack},
};

use super::namespace::Namespace;

#[derive(Default)]
pub struct TopLevelScoper
{
	functions: Namespace,
	constants: Namespace,
	user_types: Namespace,
	user_type_members: IdentifierStack,
	poisoned_names: Namespace,
	pub(super) resolution_id: usize,
}

// TOOD scope labels backwards

impl TopLevelScoper
{
	fn new_resolution_id(&mut self) -> ResolutionId
	{
		self.resolution_id += 1;
		ResolutionId(U24::new(self.resolution_id))
	}

	pub fn declare_function(
		&mut self,
		name: String,
		location: NodeId,
	) -> Result<ResolutionId, ScopingError>
	{
		let id = self.new_resolution_id();
		self.functions.insert(name, location, id)?;
		Ok(id)
	}

	pub fn use_function(
		&mut self,
		name: &str,
		location: NodeId,
	) -> Result<ResolutionId, ScopingError>
	{
		match self.functions.get(name)
		{
			Some(id) => Ok(id),
			None => Err(self.on_undeclared_name(name, location)),
		}
	}

	pub fn declare_constant(
		&mut self,
		name: String,
		location: NodeId,
	) -> Result<ResolutionId, ScopingError>
	{
		let id = self.new_resolution_id();
		self.constants.insert(name, location, id)?;
		Ok(id)
	}

	pub fn use_constant(
		&mut self,
		name: &str,
		location: NodeId,
	) -> Result<ResolutionId, ScopingError>
	{
		match self.constants.get(name)
		{
			Some(id) => Ok(id),
			None => Err(self.on_undeclared_name(name, location)),
		}
	}

	pub fn declare_structural(
		&mut self,
		name: String,
		location: NodeId,
	) -> Result<ResolutionId, ScopingError>
	{
		let id = self.new_resolution_id();
		self.user_types.insert(name, location, id)?;
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

	pub fn use_structural(
		&mut self,
		name: &str,
		location: NodeId,
	) -> Result<ResolutionId, ScopingError>
	{
		match self.user_types.get(name)
		{
			Some(id) => Ok(id),
			None => Err(self.on_undeclared_name(name, location)),
		}
	}

	fn on_undeclared_name(
		&mut self,
		name: &str,
		location: NodeId,
	) -> ScopingError
	{
		if let Some(_id) = self.poisoned_names.get(name)
		{
			return ScopingError::Poisoned;
		}
		let id = self.new_resolution_id();
		let e = self.poisoned_names.insert(name.to_string(), location, id);
		debug_assert!(e.is_ok(), "checked that it was not present first");
		ScopingError::UndeclaredReference {
			name: name.to_string(),
			location,
		}
	}
}
