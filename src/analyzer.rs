/**/

mod label_references;
mod syntax;
mod variable_references;

use crate::typer::Declaration;

pub fn analyze(program: &Vec<Declaration>) -> Result<(), anyhow::Error>
{
	syntax::analyze(program)?;
	variable_references::analyze(program)?;
	label_references::analyze(program)?;
	Ok(())
}
