//
// Part of penne
// Copyright (c) 2020 Sander in 't Veld
// License: MIT
//

//! During the analysis stage, the common AST is checked for any additional
//! syntax or semantical errors.

mod constness;
mod function_calls;
mod mutability;
mod syntax;

use crate::common::*;

pub fn analyze(program: Vec<Declaration>) -> Vec<Declaration>
{
	let program = constness::analyze(program);
	let program = syntax::analyze(program);
	let program = function_calls::analyze(program);
	let program = mutability::analyze(program);
	program
}
