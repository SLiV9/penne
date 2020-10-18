/**/

mod label_references;
mod variable_references;

use crate::typer::Declaration;

pub fn analyze(program: &Vec<Declaration>) -> Result<(), anyhow::Error>
{
	variable_references::analyze(program)?;
	label_references::analyze(program)?;
	Ok(())
}
