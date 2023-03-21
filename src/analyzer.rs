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

#[derive(Default)]
pub struct Analyzer
{
	function_call_analyzer: function_calls::Analyzer,
	mutability_analyzer: mutability::Analyzer,
}

pub fn declare(declaration: &Declaration, analyzer: &mut Analyzer)
{
	function_calls::declare(declaration, &mut analyzer.function_call_analyzer);
}

pub fn analyze(declaration: Declaration, analyzer: &mut Analyzer)
	-> Declaration
{
	let x = declaration;
	let x = constness::analyze(x);
	let x = syntax::analyze(x);
	let x = function_calls::analyze(x, &mut analyzer.function_call_analyzer);
	let x = mutability::analyze(x, &mut analyzer.mutability_analyzer);
	x
}
