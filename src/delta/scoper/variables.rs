use crate::delta::scoper::stack::IdentifierStack;
use crate::delta::scoper::{labels::LabelScoper, top_level::TopLevelScoper};

pub struct VariableScoper
{
	variables: IdentifierStack,
	resolution_id: usize,
}

impl VariableScoper
{
	pub fn new(top_level: &TopLevelScoper, labels: &LabelScoper) -> Self
	{
		Self {
			variables: Default::default(),
			resolution_id: top_level.resolution_id,
		}
	}
}
