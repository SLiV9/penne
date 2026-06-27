use crate::delta::parser::parse_node::NodeId;
use crate::delta::scoper::ResolutionId;
use crate::delta::scoper::ScopingError;

#[derive(Default)]
pub struct Namespace
{
	names: Vec<String>,
	locations: Vec<NodeId>,
	resolution_ids: Vec<ResolutionId>,
}

impl Namespace
{
	pub fn insert(
		&mut self,
		name: String,
		location: NodeId,
		resolution_id: ResolutionId,
	) -> Result<(), ScopingError>
	{
		for i in 0..self.names.len()
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
		self.locations.push(location);
		self.resolution_ids.push(resolution_id);
		Ok(())
	}

	pub fn get(&mut self, name: &str) -> Option<ResolutionId>
	{
		assert_eq!(self.names.len(), self.resolution_ids.len());
		assert_eq!(self.names.len(), self.locations.len());
		for i in 0..self.names.len()
		{
			if &self.names[i] == &name
			{
				return Some(self.resolution_ids[i]);
			}
		}
		None
	}
}
