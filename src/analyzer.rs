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

/// The Analyzer keeps track of function calls and variable mutability status
/// needed for during the analysis stage.
#[derive(Default)]
pub struct Analyzer
{
	function_call_analyzer: function_calls::Analyzer,
	mutability_analyzer: mutability::Analyzer,
}

impl Analyzer
{
	/// Add analysis metadata for a declaration.
	/// All function declarations need to be declared before analyzing first
	/// first function body.
	pub fn declare(&mut self, declaration: &Declaration)
	{
		function_calls::declare(declaration, &mut self.function_call_analyzer);
	}

	/// Analyze a declaration for various types of syntax and semantical errors.
	pub fn analyze(&mut self, declaration: Declaration) -> Declaration
	{
		let x = declaration;
		let x = constness::analyze(x);
		let x = syntax::analyze(x);
		let x = function_calls::analyze(x, &mut self.function_call_analyzer);
		let x = mutability::analyze(x, &mut self.mutability_analyzer);
		x
	}
}
