//
// Part of penne
// Copyright (c) 2020 Sander in 't Veld
// License: MIT
//

//! This implementation of the Generator generates handwritten textual IR.

use super::GeneratorBuiltin;
use super::DEFAULT_DATA_LAYOUT;
use super::WASM_DATA_LAYOUT;
use super::WASM_TARGET_TRIPLE;

use crate::resolved::*;

pub struct Generator
{
	buf: String,
	target_triple: String,
	data_layout: String,
	named_lengths: std::collections::HashMap<u32, usize>,
}

impl Default for Generator
{
	fn default() -> Generator
	{
		Generator::new()
	}
}

impl Generator
{
	fn new() -> Generator
	{
		Generator {
			buf: String::new(),
			target_triple: target_triple::TARGET.to_string(),
			data_layout: DEFAULT_DATA_LAYOUT.to_string(),
			named_lengths: std::collections::HashMap::new(),
		}
	}
}

impl super::Generator for Generator
{
	fn for_wasm(&mut self) -> Result<(), anyhow::Error>
	{
		self.target_triple = WASM_TARGET_TRIPLE.to_string();
		self.data_layout = WASM_DATA_LAYOUT.to_string();
		Ok(())
	}

	fn add_module(&mut self, module_name: &str) -> Result<(), anyhow::Error>
	{
		let _ = module_name;
		unimplemented!()
	}

	fn link_modules(&mut self) -> Result<(), anyhow::Error>
	{
		// How am I actually going to implement this?
		unimplemented!()
	}

	fn forward_declare_structure(
		&mut self,
		structure_name: &str,
	) -> Result<(), anyhow::Error>
	{
		let _ = structure_name;
		unimplemented!()
	}

	fn declare(
		&mut self,
		declaration: &Declaration,
	) -> Result<(), anyhow::Error>
	{
		let _ = declaration;
		unimplemented!()
	}

	fn generate(
		&mut self,
		declaration: &Declaration,
	) -> Result<(), anyhow::Error>
	{
		let _ = declaration;
		unimplemented!()
	}

	fn verify(&self)
	{
		// Not done for textual IR generation.
	}

	fn generate_ir(&self) -> Result<String, anyhow::Error>
	{
		Ok(self.buf.to_string())
	}

	fn get_named_length(&self, name: &Identifier) -> Option<usize>
	{
		self.named_lengths.get(&name.resolution_id).copied()
	}
}
