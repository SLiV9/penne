//
// Part of penne
// Copyright (c) 2020 Sander in 't Veld
// License: MIT
//

mod function_calls;
mod mutability;
mod syntax;

use crate::common::*;

pub fn analyze(program: Vec<Declaration>) -> Vec<Declaration>
{
	let program = syntax::analyze(program);
	let program = function_calls::analyze(program);
	let program = mutability::analyze(program);
	program
}
