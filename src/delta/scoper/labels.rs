use crate::delta::{
	parser::parse_node::U24,
	scoper::{LabelId, stack::IdentifierStack},
};

#[derive(Default)]
pub struct LabelScoper
{
	labels: IdentifierStack,
	label_id: usize,
}

impl LabelScoper
{
	fn new_label_id(&mut self) -> LabelId
	{
		self.label_id += 1;
		LabelId(U24::new(self.label_id))
	}
}
