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
	let program = label_references::analyze(program);
	let program = variable_references::analyze(program);
	program
}

pub fn get_container_depth(declaration: &Declaration, max: u32) -> u32
{
	let depth = match declaration
	{
		Declaration::Constant { depth, .. } => depth.as_ref(),
		Declaration::Function { .. } => None,
		Declaration::FunctionHead { .. } => None,
		Declaration::Structure { depth, .. } => depth.as_ref(),
		Declaration::Import { .. } => None,
		Declaration::Poison(_) => None,
	};
	match depth
	{
		Some(Ok(depth)) => *depth,
		Some(Err(_poison)) => max,
		None => max,
	}
}

pub fn is_container(declaration: &Declaration) -> bool
{
	get_container_depth(declaration, u32::MAX) < u32::MAX
}

pub fn get_structure_name(declaration: &Declaration) -> Option<&str>
{
	match declaration
	{
		Declaration::Constant { .. } => None,
		Declaration::Function { .. } => None,
		Declaration::FunctionHead { .. } => None,
		Declaration::Structure { name, .. } => Some(&name.name),
		Declaration::Import { .. } => None,
		Declaration::Poison(_) => None,
	}
}
