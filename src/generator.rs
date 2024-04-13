//
// Part of penne
// Copyright (c) 2020 Sander in 't Veld
// License: MIT
//

//! The IR generation stage turns the resolved AST into LLVM IR.

#[cfg(feature = "llvm-sys")]
pub mod llvm_sys_generator;

#[cfg(feature = "textual-ir")]
pub mod textual_ir_generator;

use crate::resolved::*;

#[cfg(feature = "llvm-sys")]
pub(crate) type DefaultGenerator = llvm_sys_generator::Generator;

#[cfg(not(feature = "llvm-sys"))]
pub(crate) type DefaultGenerator = textual_ir_generator::Generator;

/// The LLVM data layout specification for the default target.
pub const DEFAULT_DATA_LAYOUT: &str = "e-m:e-p:64:64-i64:64-n8:16:32:64-S64";

const WASM_TARGET_TRIPLE: &str = "wasm32-unknown-wasi";
const WASM_DATA_LAYOUT: &str = "e-p:32:32-i64:64-n32:64-S64";

/// The Generator generates LLVM IR.
pub trait Generator: Default
{
	/// Change the target triple from the current OS to WebAssembly.
	fn for_wasm(&mut self) -> Result<(), anyhow::Error>;

	/// Ready the Generator for a new module.
	/// This function must be called before analyzing declarations.
	fn add_module(&mut self, module_name: &str) -> Result<(), anyhow::Error>;

	/// Link the current module into the previous module.
	/// The result becomes the new current module.
	fn link_modules(&mut self) -> Result<(), anyhow::Error>;

	/// Add a structure as an opaque type, so that it can be used in
	/// pointer types.
	fn forward_declare_structure(
		&mut self,
		structure_name: &str,
	) -> Result<(), anyhow::Error>;

	/// Generate surface level IR for constants, structures and
	/// function signatures.
	fn declare(
		&mut self,
		declaration: &Declaration,
	) -> Result<(), anyhow::Error>;

	/// Generate full IR for all types of declarations.
	fn generate(
		&mut self,
		declaration: &Declaration,
	) -> Result<(), anyhow::Error>;

	/// Verify that IR generation for the current module was successful.
	fn verify(&self);

	/// Generate textual IR for the current module.
	fn generate_ir(&self) -> Result<String, anyhow::Error>;

	/// Get the value of a constant of type `usize`,
	/// if it can be obtained through constant folding.
	/// This allows that constant to be used as the length in array types.
	fn get_named_length(&self, name: &Identifier) -> Option<usize>;
}

#[must_use]
#[derive(Debug, Clone)]
pub(crate) enum GeneratorBuiltin
{
	Abort,
	Format
	{
		arguments: Vec<Expression>,
	},
	Write
	{
		fd: builtin::Fd,
		buffer: Box<Expression>,
	},
}

impl Typed for GeneratorBuiltin
{
	fn value_type(&self) -> ValueType
	{
		match self
		{
			GeneratorBuiltin::Abort => ValueType::Void,
			GeneratorBuiltin::Format { .. } => ValueType::for_string_slice(),
			GeneratorBuiltin::Write { .. } => ValueType::Usize,
		}
	}
}
