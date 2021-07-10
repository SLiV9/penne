/**/

mod label_references;
mod variable_references;

use crate::common::*;

pub fn analyze(
	program: Vec<Declaration>,
) -> Result<Vec<Declaration>, anyhow::Error>
{
	let program = variable_references::analyze(program)?;
	let program = label_references::analyze(program)?;
	Ok(program)
}
