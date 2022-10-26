//
// Part of penne
// Copyright (c) 2020 Sander in 't Veld
// License: MIT
//

//! During the scoping stage, the validity of variable references,
//! function calls and goto statements is determined.

mod label_references;
mod variable_references;

use crate::common::*;

pub fn analyze(program: Vec<Declaration>) -> Vec<Declaration>
{
	let program = variable_references::analyze(program);
	let program = label_references::analyze(program);
	program
}
