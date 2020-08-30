/**/

use crate::lexer::Token;

use anyhow::anyhow;

#[derive(Debug)]
pub enum Declaration
{
	Function
	{
		name: String,
		//parameters: Vec<Parameter>,
		body: Vec<Statement>,
	},
}

#[derive(Debug)]
pub enum Statement
{
	Expr(Expression),
}

#[derive(Debug)]
pub enum BinaryOp
{
	Add,
	Subtract,
}

#[derive(Debug)]
pub enum Expression
{
	Binary
	{
		op: BinaryOp,
		left: Expression,
		right: Expression,
	},
}

pub fn parse(tokens: Vec<Token>) -> Result<Vec<Declaration>, anyhow::Error>
{
	unimplemented!()
}
