/**/

mod function_calls;
mod mutability;
mod syntax;

use crate::common::*;

pub fn analyze(program: &Vec<Declaration>) -> Result<(), anyhow::Error>
{
	syntax::analyze(program)?;
	function_calls::analyze(program)?;
	mutability::analyze(program)?;
	Ok(())
}
