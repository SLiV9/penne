//
// Part of penne
// Copyright (c) 2020 Sander in 't Veld
// License: MIT
//

//! The IR generation stage turns the resolved AST into LLVM IR.

use crate::resolved::*;

use std::ffi::{CStr, CString};

use llvm_sys::analysis::*;
use llvm_sys::core::*;
use llvm_sys::prelude::*;
use llvm_sys::target_machine::LLVMGetDefaultTargetTriple;
use llvm_sys::*;
use llvm_sys::{LLVMBuilder, LLVMContext, LLVMModule};

macro_rules! cstr {
	($s:literal) => {
		(concat!($s, "\0").as_bytes().as_ptr() as *const ::libc::c_char)
	};
}

/// The LLVM data layout specification for the default target.
pub const DEFAULT_DATA_LAYOUT: &str = "e-m:e-p:64:64-i64:64-n8:16:32:64-S64";

/// The Generator generates LLVM IR.
pub struct Generator
{
	context: *mut LLVMContext,
	builder: *mut LLVMBuilder,
	module: *mut LLVMModule,
	combined_module: Option<*mut LLVMModule>,
	constants: std::collections::HashMap<u32, LLVMValueRef>,
	global_variables: std::collections::HashMap<u32, LLVMValueRef>,
	global_functions: std::collections::HashMap<u32, LLVMValueRef>,
	local_parameters: std::collections::HashMap<u32, LLVMValueRef>,
	local_variables: std::collections::HashMap<u32, LLVMValueRef>,
	local_labeled_blocks: std::collections::HashMap<u32, LLVMBasicBlockRef>,
	used_intrinsics: std::collections::HashMap<&'static str, LLVMValueRef>,
	target_triple: CString,
	data_layout: CString,
	type_of_usize: LLVMTypeRef,
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
		let generator = unsafe {
			let context = LLVMContextCreate();
			let module =
				LLVMModuleCreateWithNameInContext(cstr!("combined"), context);
			let target_triple =
				CStr::from_ptr(LLVMGetDefaultTargetTriple()).to_owned();
			let data_layout = CString::new(DEFAULT_DATA_LAYOUT).unwrap();
			let builder = LLVMCreateBuilderInContext(context);
			let type_of_usize = LLVMInt64TypeInContext(context);
			Generator {
				context,
				builder,
				module,
				combined_module: None,
				constants: std::collections::HashMap::new(),
				global_variables: std::collections::HashMap::new(),
				global_functions: std::collections::HashMap::new(),
				local_parameters: std::collections::HashMap::new(),
				local_variables: std::collections::HashMap::new(),
				local_labeled_blocks: std::collections::HashMap::new(),
				used_intrinsics: std::collections::HashMap::new(),
				target_triple,
				data_layout,
				type_of_usize,
			}
		};
		generator
	}

	/// Change the target triple from the current OS to WebAssembly.
	pub fn for_wasm(&mut self) -> Result<(), anyhow::Error>
	{
		unsafe {
			LLVMSetTarget(self.module, cstr!("wasm32-unknown-wasi"));
			let data_layout = "e-p:32:32-i64:64-n32:64-S64";
			self.data_layout = CString::new(data_layout)?;
			self.type_of_usize = LLVMInt32TypeInContext(self.context);
			LLVMSetDataLayout(self.module, self.data_layout.as_ptr());
			Ok(())
		}
	}

	/// Ready the Generator for a new module.
	/// This function must be called before analyzing declarations.
	pub fn add_module(&mut self, module_name: &str)
		-> Result<(), anyhow::Error>
	{
		// Link the previous `module` into `combined_module`.
		if let Some(combined) = self.combined_module.take()
		{
			// This disposes of `module`.
			let _ = unsafe {
				llvm_sys::linker::LLVMLinkModules2(combined, self.module)
			};
			self.combined_module = Some(combined);
		}
		else
		{
			// Just move `module` into the previously empty `combined_module`.
			self.combined_module = Some(self.module);
		}
		// Now `combined_module` contains a module and `module` does not.
		// That is, it is safe to overwrite `module` without disposing it.

		// Create a new `module`.
		let module_name = CString::new(module_name)?;
		unsafe {
			let module = LLVMModuleCreateWithNameInContext(
				module_name.as_ptr(),
				self.context,
			);
			LLVMSetTarget(module, self.target_triple.as_ptr());
			LLVMSetDataLayout(module, self.data_layout.as_ptr());
			self.module = module;
		}

		// Reset module specific metadata.
		self.constants.clear();
		self.global_variables.clear();
		self.global_functions.clear();
		self.local_parameters.clear();
		self.local_variables.clear();
		self.local_labeled_blocks.clear();

		Ok(())
	}

	/// Link the current module into the previous module.
	/// The result becomes the new current module.
	pub fn link_modules(&mut self) -> Result<(), anyhow::Error>
	{
		if let Some(combined) = self.combined_module.take()
		{
			// This disposes of `module`.
			let _ = unsafe {
				llvm_sys::linker::LLVMLinkModules2(combined, self.module)
			};
			// Move the result into `module`, leaving `combined_module` empty.
			self.module = combined;
			Ok(())
		}
		else
		{
			Ok(())
		}
	}

	/// Add a structure as an opaque type, so that it can be used in
	/// pointer types.
	pub fn forward_declare_structure(
		&mut self,
		structure_name: &str,
	) -> Result<(), anyhow::Error>
	{
		let name = CString::new(structure_name)?;
		unsafe { LLVMStructCreateNamed(self.context, name.as_ptr()) };
		Ok(())
	}

	/// Generate surface level IR for constants, structures and
	/// function signatures.
	pub fn declare(
		&mut self,
		declaration: &Declaration,
	) -> Result<(), anyhow::Error>
	{
		declare(declaration, self)
	}

	/// Generate full IR for all types of declarations.
	pub fn generate(
		&mut self,
		declaration: &Declaration,
	) -> Result<(), anyhow::Error>
	{
		declaration.generate(self)
	}

	/// Verify that IR generation for the current module was successful.
	pub fn verify(&self)
	{
		let _ = unsafe {
			LLVMVerifyModule(
				self.module,
				LLVMVerifierFailureAction::LLVMAbortProcessAction,
				std::ptr::null_mut(),
			)
		};

		if let Some(module) = self.combined_module
		{
			let _ = unsafe {
				LLVMVerifyModule(
					module,
					LLVMVerifierFailureAction::LLVMAbortProcessAction,
					std::ptr::null_mut(),
				)
			};
		}
	}

	fn verify_function(&self, function: LLVMValueRef)
	{
		let _ = unsafe {
			LLVMVerifyFunction(
				function,
				LLVMVerifierFailureAction::LLVMAbortProcessAction,
			)
		};
	}

	/// Generate textual IR for the current module.
	pub fn generate_ir(&self) -> Result<String, anyhow::Error>
	{
		let ircode: CString = unsafe {
			let raw = LLVMPrintModuleToString(self.module);
			let ircode = CStr::from_ptr(raw).to_owned();
			LLVMDisposeMessage(raw);
			ircode
		};
		let ircode = ircode.into_string()?;
		Ok(ircode)
	}

	fn const_u8(&mut self, value: u8) -> LLVMValueRef
	{
		unsafe {
			let bytetype = LLVMInt8TypeInContext(self.context);
			LLVMConstInt(bytetype, value as u64, 0)
		}
	}

	fn const_i32(&mut self, value: i32) -> LLVMValueRef
	{
		unsafe {
			let inttype = LLVMInt32TypeInContext(self.context);
			LLVMConstInt(inttype, value as u64, 1)
		}
	}

	fn const_usize(&mut self, value: usize) -> LLVMValueRef
	{
		unsafe { LLVMConstInt(self.type_of_usize, value as u64, 0) }
	}

	fn const_128_bit_integer(
		&mut self,
		value_bits: u128,
		inttype: LLVMTypeRef,
	) -> LLVMValueRef
	{
		let mask = u128::from(u64::MAX);
		let words = [
			(value_bits & mask) as u64,
			((value_bits >> 64) & mask) as u64,
		];
		unsafe { LLVMConstIntOfArbitraryPrecision(inttype, 2, words.as_ptr()) }
	}

	/// Get the value of a constant of type `usize`,
	/// if it can be obtained through constant folding.
	/// This allows that constant to be used as the length in array types.
	pub fn get_named_length(&self, name: &Identifier) -> Option<usize>
	{
		if let Some(&constant) = self.constants.get(&name.resolution_id)
		{
			let v: u64 = unsafe { LLVMConstIntGetZExtValue(constant) };
			v.try_into().ok()
		}
		else
		{
			None
		}
	}

	fn size_in_bits(&self, type_ref: LLVMTypeRef) -> usize
	{
		let size_in_bits: u64 = unsafe {
			llvm_sys::target::LLVMSizeOfTypeInBits(
				llvm_sys::target::LLVMGetModuleDataLayout(self.module),
				type_ref,
			)
		};
		size_in_bits as usize
	}

	fn get_trap_like_intrinsic(&mut self, name: &'static str) -> LLVMValueRef
	{
		let function = self.used_intrinsics.entry(name).or_insert_with(|| {
			let linkage = LLVMLinkage::LLVMExternalLinkage;
			let callconv = LLVMCallConv::LLVMCCallConv;
			let function_name = CString::new(name.as_bytes()).unwrap();

			unsafe {
				let return_type = LLVMVoidTypeInContext(self.context);
				let function_type =
					LLVMFunctionType(return_type, std::ptr::null_mut(), 0, 0);
				let function = LLVMAddFunction(
					self.module,
					function_name.as_ptr(),
					function_type,
				);
				LLVMSetLinkage(function, linkage);
				LLVMSetFunctionCallConv(function, callconv as u32);
				function
			}
		});
		*function
	}

	fn get_snprintf_intrinsic(&mut self) -> (LLVMValueRef, ValueType)
	{
		let return_type = ValueType::Int32;
		let return_type_ref = unsafe { LLVMInt32TypeInContext(self.context) };
		let name = "snprintf";
		let function = self.used_intrinsics.entry(name).or_insert_with(|| {
			let linkage = LLVMLinkage::LLVMExternalLinkage;
			let callconv = LLVMCallConv::LLVMCCallConv;
			let function_name = CString::new(name.as_bytes()).unwrap();
			let is_var_args = 1;

			unsafe {
				let char_type = LLVMInt8TypeInContext(self.context);
				let char_ptr_type = LLVMPointerType(char_type, 0u32);
				let out_ptr_type = char_ptr_type;
				let size_type = self.type_of_usize;
				let format_ptr_type = char_ptr_type;
				let mut args = [out_ptr_type, size_type, format_ptr_type];
				let function_type = LLVMFunctionType(
					return_type_ref,
					args.as_mut_ptr(),
					args.len() as u32,
					is_var_args,
				);
				let function = LLVMAddFunction(
					self.module,
					function_name.as_ptr(),
					function_type,
				);
				LLVMSetLinkage(function, linkage);
				LLVMSetFunctionCallConv(function, callconv as u32);
				function
			}
		});
		(*function, return_type)
	}

	fn get_write_intrinsic(&mut self) -> (LLVMValueRef, ValueType)
	{
		let return_type = ValueType::Int64;
		let return_type_ref = unsafe { LLVMInt64TypeInContext(self.context) };
		let name = "write";
		let function = self.used_intrinsics.entry(name).or_insert_with(|| {
			let linkage = LLVMLinkage::LLVMExternalLinkage;
			let callconv = LLVMCallConv::LLVMCCallConv;
			let function_name = CString::new(name.as_bytes()).unwrap();

			unsafe {
				let fd_type = LLVMInt32TypeInContext(self.context);
				let char_type = LLVMInt8TypeInContext(self.context);
				let char_ptr_type = LLVMPointerType(char_type, 0u32);
				let size_type = self.type_of_usize;
				let mut args = [fd_type, char_ptr_type, size_type];
				let function_type = LLVMFunctionType(
					return_type_ref,
					args.as_mut_ptr(),
					args.len() as u32,
					0,
				);
				let function = LLVMAddFunction(
					self.module,
					function_name.as_ptr(),
					function_type,
				);
				LLVMSetLinkage(function, linkage);
				LLVMSetFunctionCallConv(function, callconv as u32);
				function
			}
		});
		(*function, return_type)
	}
}

impl Drop for Generator
{
	fn drop(&mut self)
	{
		unsafe {
			LLVMDisposeBuilder(self.builder);
			LLVMDisposeModule(self.module);
			if let Some(module) = self.combined_module
			{
				LLVMDisposeModule(module);
			}
			LLVMContextDispose(self.context);
		}
	}
}

fn declare(
	declaration: &Declaration,
	llvm: &mut Generator,
) -> Result<(), anyhow::Error>
{
	match declaration
	{
		Declaration::Constant {
			name,
			value,
			value_type,
			depth: _,
			flags: _,
		} =>
		{
			let cname = CString::new(&name.name as &str)?;
			let vartype = value_type.generate(llvm)?;
			let global =
				unsafe { LLVMAddGlobal(llvm.module, vartype, cname.as_ptr()) };
			llvm.global_variables.insert(name.resolution_id, global);
			unsafe {
				LLVMSetGlobalConstant(global, 1);
				LLVMSetUnnamedAddr(global, 1);
			}
			let linkage = LLVMLinkage::LLVMPrivateLinkage;
			unsafe { LLVMSetLinkage(global, linkage) };
			let constant = value.generate(llvm)?;
			unsafe { LLVMSetInitializer(global, constant) };
			let is_const = unsafe { LLVMIsConstant(constant) };
			if is_const > 0
			{
				llvm.constants.insert(name.resolution_id, constant);
			}
			Ok(())
		}
		Declaration::Function {
			name,
			parameters,
			body: _,
			return_type,
			flags,
		}
		| Declaration::FunctionHead {
			name,
			parameters,
			return_type,
			flags,
		} =>
		{
			let return_type = match return_type
			{
				Some(return_type) => return_type.generate(llvm)?,

				None =>
				unsafe { LLVMVoidTypeInContext(llvm.context) },
			};

			let function_name = CString::new(name.name.as_bytes())?;

			let param_types: Result<Vec<LLVMTypeRef>, anyhow::Error> =
				parameters
					.iter()
					.map(|parameter| parameter.value_type.generate(llvm))
					.collect();
			let mut param_types: Vec<LLVMTypeRef> = param_types?;

			let function: LLVMValueRef = unsafe {
				let function_type = LLVMFunctionType(
					return_type,
					param_types.as_mut_ptr(),
					param_types.len() as u32,
					0,
				);
				let function = LLVMAddFunction(
					llvm.module,
					function_name.as_ptr(),
					function_type,
				);
				function
			};

			let linkage = if flags.contains(DeclarationFlag::Public)
				|| flags.contains(DeclarationFlag::Main)
				|| flags.contains(DeclarationFlag::Forward)
			{
				LLVMLinkage::LLVMExternalLinkage
			}
			else
			{
				LLVMLinkage::LLVMPrivateLinkage
			};
			unsafe { LLVMSetLinkage(function, linkage) };

			let callconv = if flags.contains(DeclarationFlag::External)
			{
				LLVMCallConv::LLVMCCallConv
			}
			else
			{
				LLVMCallConv::LLVMFastCallConv
			};
			unsafe { LLVMSetFunctionCallConv(function, callconv as u32) };

			llvm.global_functions.insert(name.resolution_id, function);

			Ok(())
		}
		Declaration::Structure {
			name,
			members,
			flags,
			depth: _,
		} =>
		{
			let name = CString::new(&name.name as &str)?;
			let struct_type = unsafe {
				let x = LLVMGetTypeByName(llvm.module, name.as_ptr());
				if !x.is_null()
				{
					x
				}
				else
				{
					LLVMStructCreateNamed(llvm.context, name.as_ptr())
				}
			};

			if flags.contains(DeclarationFlag::OpaqueStruct)
			{
				assert!(members.is_empty());
				return Ok(());
			}

			let member_types: Result<Vec<LLVMTypeRef>, anyhow::Error> = members
				.iter()
				.map(|m| m.value_type.generate(llvm))
				.collect();
			let mut member_types = member_types?;
			let is_packed = 0;

			unsafe {
				LLVMStructSetBody(
					struct_type,
					member_types.as_mut_ptr(),
					member_types.len() as u32,
					is_packed,
				);
			}
			Ok(())
		}
	}
}

trait Generatable
{
	type Item;

	fn generate(
		&self,
		gen: &mut Generator,
	) -> Result<Self::Item, anyhow::Error>;
}

impl Generatable for Declaration
{
	type Item = ();

	fn generate(
		&self,
		llvm: &mut Generator,
	) -> Result<Self::Item, anyhow::Error>
	{
		match self
		{
			Declaration::Constant { .. } =>
			{
				// We already declared this.
				Ok(())
			}
			Declaration::Function {
				name,
				parameters,
				body,
				return_type: _,
				flags: _,
			} =>
			{
				let function = llvm.global_functions.get(&name.resolution_id);
				let function = match function
				{
					Some(function) => *function,
					None => unreachable!(),
				};

				unsafe {
					let entry_block = LLVMAppendBasicBlockInContext(
						llvm.context,
						function,
						cstr!("entry"),
					);
					LLVMPositionBuilderAtEnd(llvm.builder, entry_block);
				};

				for (i, parameter) in parameters.iter().enumerate()
				{
					let param = unsafe { LLVMGetParam(function, i as u32) };
					let loc = param;
					llvm.local_parameters
						.insert(parameter.name.resolution_id, loc);
				}

				body.generate(llvm)?;
				llvm.local_parameters.clear();
				llvm.local_variables.clear();
				llvm.local_labeled_blocks.clear();

				llvm.verify_function(function);

				Ok(())
			}
			Declaration::FunctionHead { .. } =>
			{
				// We already declared this.
				Ok(())
			}
			Declaration::Structure { .. } =>
			{
				// We already declared this.
				Ok(())
			}
		}
	}
}

impl Generatable for FunctionBody
{
	type Item = ();

	fn generate(
		&self,
		llvm: &mut Generator,
	) -> Result<Self::Item, anyhow::Error>
	{
		for statement in &self.statements
		{
			statement.generate(llvm)?;
		}

		if let Some(value) = &self.return_value
		{
			let result: LLVMValueRef = value.generate(llvm)?;
			unsafe {
				LLVMBuildRet(llvm.builder, result);
			};
		}
		else
		{
			unsafe {
				LLVMBuildRetVoid(llvm.builder);
			};
		}

		Ok(())
	}
}

impl Generatable for Block
{
	type Item = ();

	fn generate(
		&self,
		llvm: &mut Generator,
	) -> Result<Self::Item, anyhow::Error>
	{
		if let Some(&Statement::Loop { .. }) = self.statements.last()
		{
			let inner_block = unsafe {
				let current_block = LLVMGetInsertBlock(llvm.builder);
				let function = LLVMGetBasicBlockParent(current_block);
				let inner_block = LLVMAppendBasicBlockInContext(
					llvm.context,
					function,
					cstr!("looped-block"),
				);
				LLVMPositionBuilderAtEnd(llvm.builder, current_block);
				LLVMBuildBr(llvm.builder, inner_block);
				LLVMPositionBuilderAtEnd(llvm.builder, inner_block);
				inner_block
			};

			let len = self.statements.len() - 1;
			for statement in &self.statements[0..len]
			{
				statement.generate(llvm)?;
			}

			unsafe {
				let function = LLVMGetBasicBlockParent(inner_block);
				LLVMBuildBr(llvm.builder, inner_block);
				let after_block = LLVMAppendBasicBlockInContext(
					llvm.context,
					function,
					cstr!("after-looped-block"),
				);
				LLVMPositionBuilderAtEnd(llvm.builder, after_block);
			}
		}
		else
		{
			for statement in &self.statements
			{
				statement.generate(llvm)?;
			}
		}
		Ok(())
	}
}

impl Generatable for Statement
{
	type Item = ();

	fn generate(
		&self,
		llvm: &mut Generator,
	) -> Result<Self::Item, anyhow::Error>
	{
		match self
		{
			Statement::Declaration {
				name,
				value: Some(value),
				value_type,
			} =>
			{
				let cname = CString::new(&name.name as &str)?;
				let vartype = value_type.generate(llvm)?;
				let loc = unsafe {
					LLVMBuildAlloca(llvm.builder, vartype, cname.as_ptr())
				};
				llvm.local_variables.insert(name.resolution_id, loc);
				let value = value.generate(llvm)?;
				unsafe {
					LLVMBuildStore(llvm.builder, value, loc);
				}
				Ok(())
			}
			Statement::Declaration {
				name,
				value: None,
				value_type,
			} =>
			{
				let cname = CString::new(&name.name as &str)?;
				let vartype = value_type.generate(llvm)?;
				let loc = unsafe {
					LLVMBuildAlloca(llvm.builder, vartype, cname.as_ptr())
				};
				llvm.local_variables.insert(name.resolution_id, loc);
				Ok(())
			}
			Statement::Assignment { reference, value } =>
			{
				let address = reference.generate_storage_address(llvm)?;
				let value = value.generate(llvm)?;
				unsafe {
					LLVMBuildStore(llvm.builder, value, address);
				}
				Ok(())
			}
			Statement::EvaluateAndDiscard { value } =>
			{
				let _discarded = value.generate(llvm)?;
				Ok(())
			}
			Statement::Loop { .. } => unreachable!(),
			Statement::Goto { label } =>
			{
				let current_block = unsafe { LLVMGetInsertBlock(llvm.builder) };
				let unreachable_block = unsafe {
					let function = LLVMGetBasicBlockParent(current_block);
					let unreachable_block = LLVMAppendBasicBlockInContext(
						llvm.context,
						function,
						cstr!("unreachable-after-goto"),
					);
					unreachable_block
				};
				let labeled_block = find_or_append_labeled_block(llvm, label)?;
				unsafe {
					LLVMPositionBuilderAtEnd(llvm.builder, current_block);
					LLVMBuildBr(llvm.builder, labeled_block);
					LLVMPositionBuilderAtEnd(llvm.builder, unreachable_block);
				}
				Ok(())
			}
			Statement::Label { label } =>
			{
				let current_block = unsafe { LLVMGetInsertBlock(llvm.builder) };
				let labeled_block = find_or_append_labeled_block(llvm, label)?;
				unsafe {
					LLVMPositionBuilderAtEnd(llvm.builder, current_block);
					LLVMBuildBr(llvm.builder, labeled_block);
					LLVMPositionBuilderAtEnd(llvm.builder, labeled_block);
				}
				Ok(())
			}
			Statement::If {
				condition,
				then_branch,
				else_branch,
			} =>
			{
				let condition = condition.generate(llvm)?;
				let cond_block = unsafe { LLVMGetInsertBlock(llvm.builder) };
				let function = unsafe { LLVMGetBasicBlockParent(cond_block) };

				let then_start_block = unsafe {
					let then_block = LLVMAppendBasicBlockInContext(
						llvm.context,
						function,
						cstr!("then"),
					);
					LLVMPositionBuilderAtEnd(llvm.builder, then_block);
					then_block
				};
				then_branch.generate(llvm)?;
				let then_end_block = unsafe {
					let block = LLVMGetInsertBlock(llvm.builder);
					block
				};

				let else_blocks = if let Some(else_branch) = else_branch
				{
					let start_block = unsafe {
						let else_block = LLVMAppendBasicBlockInContext(
							llvm.context,
							function,
							cstr!("else"),
						);
						LLVMPositionBuilderAtEnd(llvm.builder, else_block);
						else_block
					};
					else_branch.generate(llvm)?;
					let end_block = unsafe {
						let block = LLVMGetInsertBlock(llvm.builder);
						block
					};
					Some((start_block, end_block))
				}
				else
				{
					None
				};

				let after_block = unsafe {
					let after_block = LLVMAppendBasicBlockInContext(
						llvm.context,
						function,
						cstr!("after"),
					);
					after_block
				};
				unsafe {
					LLVMPositionBuilderAtEnd(llvm.builder, then_end_block);
					LLVMBuildBr(llvm.builder, after_block);
				}
				let else_start_block = match else_blocks
				{
					Some((else_start_block, else_end_block)) =>
					unsafe {
						LLVMPositionBuilderAtEnd(llvm.builder, else_end_block);
						LLVMBuildBr(llvm.builder, after_block);
						else_start_block
					},
					None => after_block,
				};
				unsafe {
					LLVMPositionBuilderAtEnd(llvm.builder, cond_block);
					LLVMBuildCondBr(
						llvm.builder,
						condition,
						then_start_block,
						else_start_block,
					);
					LLVMPositionBuilderAtEnd(llvm.builder, after_block);
				}
				Ok(())
			}
			Statement::Block(block) => block.generate(llvm),
		}
	}
}

fn find_or_append_labeled_block(
	llvm: &mut Generator,
	label: &Identifier,
) -> Result<LLVMBasicBlockRef, anyhow::Error>
{
	if let Some(block) = llvm.local_labeled_blocks.get(&label.resolution_id)
	{
		Ok(*block)
	}
	else
	{
		let block_name: &str = &label.name;
		let cname = CString::new(block_name)?;
		let block = unsafe {
			let current_block = LLVMGetInsertBlock(llvm.builder);
			let function = LLVMGetBasicBlockParent(current_block);
			let labeled_block = LLVMAppendBasicBlockInContext(
				llvm.context,
				function,
				cname.as_ptr(),
			);
			labeled_block
		};
		llvm.local_labeled_blocks.insert(label.resolution_id, block);
		Ok(block)
	}
}

impl Generatable for Comparison
{
	type Item = LLVMValueRef;

	fn generate(
		&self,
		llvm: &mut Generator,
	) -> Result<Self::Item, anyhow::Error>
	{
		let is_signed = self.compared_type.is_signed();
		let left = self.left.generate(llvm)?;
		let right = self.right.generate(llvm)?;
		let pred = match self.op
		{
			ComparisonOp::Equals => LLVMIntPredicate::LLVMIntEQ,
			ComparisonOp::DoesNotEqual => LLVMIntPredicate::LLVMIntNE,
			ComparisonOp::IsGreater if is_signed =>
			{
				LLVMIntPredicate::LLVMIntSGT
			}
			ComparisonOp::IsGreater => LLVMIntPredicate::LLVMIntUGT,
			ComparisonOp::IsLess if is_signed => LLVMIntPredicate::LLVMIntSLT,
			ComparisonOp::IsLess => LLVMIntPredicate::LLVMIntULT,
			ComparisonOp::IsGE if is_signed => LLVMIntPredicate::LLVMIntSGE,
			ComparisonOp::IsGE => LLVMIntPredicate::LLVMIntUGE,
			ComparisonOp::IsLE if is_signed => LLVMIntPredicate::LLVMIntSLE,
			ComparisonOp::IsLE => LLVMIntPredicate::LLVMIntULE,
		};
		let result = unsafe {
			LLVMBuildICmp(llvm.builder, pred, left, right, cstr!(""))
		};
		Ok(result)
	}
}

impl Generatable for Expression
{
	type Item = LLVMValueRef;

	fn generate(
		&self,
		llvm: &mut Generator,
	) -> Result<Self::Item, anyhow::Error>
	{
		match self
		{
			Expression::Binary {
				op,
				left,
				right,
				value_type,
			} =>
			{
				let is_signed = value_type.is_signed();
				let left = left.generate(llvm)?;
				let right = right.generate(llvm)?;
				let result = match op
				{
					BinaryOp::Add =>
					unsafe {
						LLVMBuildAdd(llvm.builder, left, right, cstr!(""))
					},
					BinaryOp::Subtract =>
					unsafe {
						LLVMBuildSub(llvm.builder, left, right, cstr!(""))
					},
					BinaryOp::Multiply =>
					unsafe {
						LLVMBuildMul(llvm.builder, left, right, cstr!(""))
					},
					BinaryOp::Divide if is_signed =>
					unsafe {
						LLVMBuildSDiv(llvm.builder, left, right, cstr!(""))
					},
					BinaryOp::Divide =>
					unsafe {
						LLVMBuildUDiv(llvm.builder, left, right, cstr!(""))
					},
					BinaryOp::Modulo if is_signed =>
					unsafe {
						LLVMBuildSRem(llvm.builder, left, right, cstr!(""))
					},
					BinaryOp::Modulo =>
					unsafe {
						LLVMBuildURem(llvm.builder, left, right, cstr!(""))
					},
					BinaryOp::BitwiseAnd =>
					unsafe {
						LLVMBuildAnd(llvm.builder, left, right, cstr!(""))
					},
					BinaryOp::BitwiseOr =>
					unsafe {
						LLVMBuildOr(llvm.builder, left, right, cstr!(""))
					},
					BinaryOp::BitwiseXor =>
					unsafe {
						LLVMBuildXor(llvm.builder, left, right, cstr!(""))
					},
					BinaryOp::ShiftLeft =>
					unsafe {
						LLVMBuildShl(llvm.builder, left, right, cstr!(""))
					},
					BinaryOp::ShiftRight =>
					unsafe {
						LLVMBuildLShr(llvm.builder, left, right, cstr!(""))
					},
				};
				Ok(result)
			}
			Expression::Unary {
				op,
				expression,
				value_type: _,
			} =>
			{
				let expr = expression.generate(llvm)?;
				let result = match op
				{
					UnaryOp::Negative =>
					unsafe {
						LLVMBuildNeg(llvm.builder, expr, cstr!(""))
					},
					UnaryOp::BitwiseComplement =>
					unsafe {
						LLVMBuildNot(llvm.builder, expr, cstr!(""))
					},
				};
				Ok(result)
			}
			Expression::SignedIntegerLiteral { value, value_type } =>
			{
				let value: i128 = *value;
				let inttype = value_type.generate(llvm)?;
				const MIN: i128 = i64::MIN as i128;
				const MAX: i128 = u64::MAX as i128;
				match value
				{
					MIN..=-1 =>
					{
						let value_bits = (value as i64) as u64;
						let signed = 1;
						unsafe { Ok(LLVMConstInt(inttype, value_bits, signed)) }
					}
					0..=MAX =>
					{
						let value_bits = value as u64;
						let signed = 0;
						unsafe { Ok(LLVMConstInt(inttype, value_bits, signed)) }
					}
					value =>
					{
						let value_bits = value as u128;
						Ok(llvm.const_128_bit_integer(value_bits, inttype))
					}
				}
			}
			Expression::BitIntegerLiteral { value, value_type } =>
			{
				let value: u128 = *value;
				match value_type
				{
					ValueType::Pointer { deref_type: _ }
					| ValueType::View { deref_type: _ } =>
					{
						let pointertype = value_type.generate(llvm)?;
						let value_bits = (value & 0xFFFFFFFF) as u64;
						let address = value_bits as usize;
						let value = llvm.const_usize(address);
						unsafe { Ok(LLVMConstIntToPtr(value, pointertype)) }
					}
					ValueType::Usize =>
					{
						let value_bits = (value & 0xFFFFFFFF) as u64;
						Ok(llvm.const_usize(value_bits as usize))
					}
					_ if value <= u64::MAX as u128 =>
					{
						let inttype = value_type.generate(llvm)?;
						let value_bits = value as u64;
						unsafe { Ok(LLVMConstInt(inttype, value_bits, 0)) }
					}
					_ =>
					{
						let inttype = value_type.generate(llvm)?;
						Ok(llvm.const_128_bit_integer(value, inttype))
					}
				}
			}
			Expression::ArrayLiteral {
				elements,
				element_type,
			} =>
			{
				let element_type: LLVMTypeRef = element_type.generate(llvm)?;
				let mut const_values = Vec::with_capacity(elements.len());
				let mut inserts = Vec::with_capacity(0);
				for (i, element) in elements.iter().enumerate()
				{
					let value: LLVMValueRef = element.generate(llvm)?;
					let is_const = unsafe { LLVMIsConstant(value) };
					if is_const > 0
					{
						const_values.push(value);
					}
					else
					{
						let undef = unsafe { LLVMGetUndef(element_type) };
						const_values.push(undef);
						inserts.push((i as u32, value));
					}
				}
				let mut result = unsafe {
					LLVMConstArray(
						element_type,
						const_values.as_mut_ptr(),
						const_values.len() as u32,
					)
				};
				for (i, v) in inserts.into_iter()
				{
					result = unsafe {
						LLVMBuildInsertValue(
							llvm.builder,
							result,
							v,
							i,
							cstr!(""),
						)
					};
				}
				Ok(result)
			}
			Expression::StringLiteral { bytes } =>
			{
				generate_inplace_string_literal(bytes, llvm)
			}
			Expression::Structural {
				structural_type,
				members,
			} => generate_structure_literal(structural_type, members, llvm),
			Expression::Parenthesized { inner } => inner.generate(llvm),
			Expression::Deref {
				reference,
				deref_type,
			} => reference.generate_deref(deref_type, llvm),
			Expression::Autocoerce {
				expression,
				coerced_type,
			} => generate_autocoerce(expression, coerced_type, llvm),
			Expression::BitCast {
				expression,
				coerced_type,
			} => generate_bitcast(expression, coerced_type, llvm),
			Expression::PrimitiveCast {
				expression,
				expression_type,
				coerced_type,
			} => generate_primitive_cast(
				expression,
				expression_type,
				coerced_type,
				llvm,
			),
			Expression::LengthOfArray { reference } =>
			{
				let address = reference.generate_storage_address(llvm)?;
				generate_array_len(address, llvm)
			}
			Expression::SizeOf {
				queried_type: ValueType::Bool,
			} =>
			{
				let value = llvm.const_usize(1);
				Ok(value)
			}
			Expression::SizeOf { queried_type } =>
			{
				let queried_type = queried_type.generate(llvm)?;
				let size_in_bits = llvm.size_in_bits(queried_type);
				assert_eq!(size_in_bits % 8, 0);
				let value = llvm.const_usize(size_in_bits / 8);
				Ok(value)
			}
			Expression::FunctionCall {
				name,
				arguments,
				return_type: _,
			} =>
			{
				let function = llvm.global_functions.get(&name.resolution_id);
				let function = match function
				{
					Some(function) => *function,
					None => unreachable!(),
				};

				let arguments: Result<Vec<LLVMValueRef>, anyhow::Error> =
					arguments
						.iter()
						.map(|argument| argument.generate(llvm))
						.collect();
				let mut arguments: Vec<LLVMValueRef> = arguments?;

				let result = unsafe {
					LLVMBuildCall(
						llvm.builder,
						function,
						arguments.as_mut_ptr(),
						arguments.len() as u32,
						cstr!(""),
					)
				};
				Ok(result)
			}
			Expression::InlineBlock { statements, value } =>
			{
				for statement in statements
				{
					statement.generate(llvm)?;
				}
				value.generate(llvm)
			}
			Expression::Builtin(builtin) => builtin.generate(llvm),
		}
	}
}

impl Generatable for ValueType
{
	type Item = LLVMTypeRef;

	fn generate(
		&self,
		llvm: &mut Generator,
	) -> Result<Self::Item, anyhow::Error>
	{
		let typeref = match self
		{
			ValueType::Void => unreachable!(),
			ValueType::Int8 =>
			unsafe { LLVMInt8TypeInContext(llvm.context) },
			ValueType::Int16 =>
			unsafe { LLVMInt16TypeInContext(llvm.context) },
			ValueType::Int32 =>
			unsafe { LLVMInt32TypeInContext(llvm.context) },
			ValueType::Int64 =>
			unsafe { LLVMInt64TypeInContext(llvm.context) },
			ValueType::Int128 =>
			unsafe {
				LLVMInt128TypeInContext(llvm.context)
			},
			ValueType::Uint8 =>
			unsafe { LLVMInt8TypeInContext(llvm.context) },
			ValueType::Uint16 =>
			unsafe {
				LLVMInt16TypeInContext(llvm.context)
			},
			ValueType::Uint32 =>
			unsafe {
				LLVMInt32TypeInContext(llvm.context)
			},
			ValueType::Uint64 =>
			unsafe {
				LLVMInt64TypeInContext(llvm.context)
			},
			ValueType::Uint128 =>
			unsafe {
				LLVMInt128TypeInContext(llvm.context)
			},
			ValueType::Usize => llvm.type_of_usize,
			ValueType::Char8 =>
			unsafe { LLVMInt8TypeInContext(llvm.context) },
			ValueType::Bool =>
			unsafe { LLVMInt1TypeInContext(llvm.context) },
			ValueType::Array {
				element_type,
				length,
			} =>
			{
				let element_type = element_type.generate(llvm)?;
				let length: usize = *length;
				let length: u32 = length.try_into()?;
				unsafe { LLVMArrayType(element_type, length) }
			}
			ValueType::ArrayWithNamedLength { .. } => unreachable!(),
			ValueType::Slice { element_type }
			| ValueType::SlicePointer { element_type } =>
			{
				let element_type = element_type.generate(llvm)?;
				let storagetype = unsafe { LLVMArrayType(element_type, 0u32) };
				let pointertype = unsafe { LLVMPointerType(storagetype, 0u32) };
				let sizetype = ValueType::Usize.generate(llvm)?;
				let mut member_types = [pointertype, sizetype];
				unsafe {
					LLVMStructTypeInContext(
						llvm.context,
						member_types.as_mut_ptr(),
						member_types.len() as u32,
						0,
					)
				}
			}
			ValueType::EndlessArray { element_type } =>
			{
				element_type.generate(llvm)?
			}
			ValueType::Arraylike { element_type } =>
			{
				element_type.generate(llvm)?
			}
			ValueType::Struct { identifier }
			| ValueType::Word {
				identifier,
				size_in_bytes: _,
			} =>
			{
				let struct_name = CString::new(&identifier.name as &str)?;
				unsafe { LLVMGetTypeByName(llvm.module, struct_name.as_ptr()) }
			}
			ValueType::UnresolvedStructOrWord { .. } => unreachable!(),
			ValueType::Pointer { deref_type }
			| ValueType::View { deref_type } =>
			{
				let deref_type = deref_type.generate(llvm)?;
				unsafe { LLVMPointerType(deref_type, 0u32) }
			}
		};
		Ok(typeref)
	}
}

impl Reference
{
	fn generate_deref(
		&self,
		_deref_type: &ValueType,
		llvm: &mut Generator,
	) -> Result<LLVMValueRef, anyhow::Error>
	{
		let id = &self.base.resolution_id;
		if self.is_trivial()
		{
			if let Some(&constant) = llvm.constants.get(id)
			{
				return Ok(constant);
			}
		}
		if let Some(param) = llvm.local_parameters.get(id)
		{
			let shortcut = self.generate_word_deref(*param, llvm)?;
			if let Some(value) = shortcut
			{
				return Ok(value);
			}
		}

		let address = self.generate_storage_address(llvm)?;
		let result = if self.take_address
		{
			address
		}
		else
		{
			unsafe { LLVMBuildLoad(llvm.builder, address, cstr!("")) }
		};
		Ok(result)
	}

	fn generate_word_deref(
		&self,
		from: LLVMValueRef,
		llvm: &mut Generator,
	) -> Result<Option<LLVMValueRef>, anyhow::Error>
	{
		let mut value = from;
		for step in self.steps.iter()
		{
			match step
			{
				ReferenceStep::Autodeslice { offset } =>
				{
					let addr = value;
					let offset: u8 = *offset;
					let offset: u32 = offset.into();
					value = unsafe {
						LLVMBuildExtractValue(
							llvm.builder,
							addr,
							offset,
							cstr!(""),
						)
					};
				}
				ReferenceStep::Member { offset } =>
				{
					let addr = value;
					let offset: usize = *offset;
					let offset: u32 = offset.try_into()?;
					value = unsafe {
						LLVMBuildExtractValue(
							llvm.builder,
							addr,
							offset,
							cstr!(""),
						)
					};
				}
				ReferenceStep::Element { .. } => return Ok(None),
				ReferenceStep::Autoderef => return Ok(None),
				ReferenceStep::Autoview => return Ok(None),
			}
		}
		Ok(Some(value))
	}

	fn generate_storage_address(
		&self,
		llvm: &mut Generator,
	) -> Result<LLVMValueRef, anyhow::Error>
	{
		let mut indices = Vec::new();
		let mut is_immediate_parameter = false;

		let id = &self.base.resolution_id;
		let base_addr = if let Some(value) = llvm.local_parameters.get(id)
		{
			if self.steps.is_empty()
			{
				// We assume that the parameter is ValueType::Slice(Pointer).
				let param = *value;
				let tmp = unsafe {
					LLVMBuildExtractValue(llvm.builder, param, 0u32, cstr!(""))
				};
				tmp
			}
			else
			{
				is_immediate_parameter = true;
				*value
			}
		}
		else if let Some(value) = llvm.local_variables.get(id)
		{
			let addr = *value;
			indices.push(llvm.const_i32(0));
			addr
		}
		else if let Some(value) = llvm.global_variables.get(id)
		{
			let addr = *value;
			indices.push(llvm.const_i32(0));
			addr
		}
		else
		{
			unreachable!()
		};

		if self.steps.is_empty()
		{
			return Ok(base_addr);
		}

		let mut steps = self.steps.iter().peekable();
		let mut addr = base_addr;
		while let Some(step) = steps.next()
		{
			match step
			{
				ReferenceStep::Element {
					argument,
					is_endless: _,
				} =>
				{
					let argument: LLVMValueRef = argument.generate(llvm)?;
					indices.push(argument)
				}
				ReferenceStep::Member { offset } =>
				{
					let offset: usize = *offset;
					let offset: i32 = offset.try_into()?;
					indices.push(llvm.const_i32(offset));
				}
				ReferenceStep::Autoderef | ReferenceStep::Autoview =>
				{
					let is_followed_by_access = match steps.peek()
					{
						Some(ReferenceStep::Element { is_endless, .. }) =>
						{
							!is_endless
						}
						Some(ReferenceStep::Member { .. }) =>
						{
							if is_immediate_parameter
							{
								indices.push(llvm.const_i32(0));
							}
							true
						}
						Some(ReferenceStep::Autoderef { .. }) => false,
						Some(ReferenceStep::Autoview { .. }) => false,
						Some(ReferenceStep::Autodeslice { offset: 0 }) =>
						{
							if is_immediate_parameter
							{
								// If we are a parameter slice, we do deref,
								// because we need the pointer to the data.
								is_immediate_parameter = false;
								false
							}
							else
							{
								true
							}
						}
						Some(ReferenceStep::Autodeslice { offset: 1 }) =>
						{
							indices.push(llvm.const_i32(0));
							false
						}
						Some(ReferenceStep::Autodeslice { offset: _ }) =>
						{
							unreachable!()
						}
						None => false,
					};
					// If we are a parameter, we are not really a pointer,
					// so we do not need to deref (i.e. load).
					if is_immediate_parameter
					{
						is_immediate_parameter = false;
						continue;
					}
					if !indices.is_empty()
					{
						addr = unsafe {
							LLVMBuildGEP(
								llvm.builder,
								addr,
								indices.as_mut_ptr(),
								indices.len() as u32,
								cstr!(""),
							)
						};
					}
					addr =
						unsafe { LLVMBuildLoad(llvm.builder, addr, cstr!("")) };
					indices.clear();
					if is_followed_by_access
					{
						indices.push(llvm.const_i32(0));
					}
				}
				ReferenceStep::Autodeslice { offset: 0 } =>
				{
					if indices.is_empty()
					{
						addr = unsafe {
							LLVMBuildExtractValue(
								llvm.builder,
								addr,
								0,
								cstr!(""),
							)
						};
					}
					else
					{
						indices.push(llvm.const_i32(0));
						addr = unsafe {
							LLVMBuildGEP(
								llvm.builder,
								addr,
								indices.as_mut_ptr(),
								indices.len() as u32,
								cstr!(""),
							)
						};
						addr = unsafe {
							LLVMBuildLoad(llvm.builder, addr, cstr!(""))
						};
						indices.clear();
					}
					indices.push(llvm.const_i32(0))
				}
				ReferenceStep::Autodeslice { offset: 1 } =>
				{
					indices.push(llvm.const_i32(1))
				}
				ReferenceStep::Autodeslice { offset: _ } => unreachable!(),
			}
		}

		if !indices.is_empty()
		{
			addr = unsafe {
				LLVMBuildGEP(
					llvm.builder,
					addr,
					indices.as_mut_ptr(),
					indices.len() as u32,
					cstr!(""),
				)
			};
		}

		Ok(addr)
	}
}

fn generate_array_len(
	address: LLVMValueRef,
	llvm: &mut Generator,
) -> Result<LLVMValueRef, anyhow::Error>
{
	let pointer_type = unsafe { LLVMTypeOf(address) };
	let array_type = unsafe { LLVMGetElementType(pointer_type) };
	let length: u32 = unsafe { LLVMGetArrayLength(array_type) };
	let result = llvm.const_usize(length as usize);
	Ok(result)
}

fn generate_view(
	address: LLVMValueRef,
	viewed_type: &ValueType,
	llvm: &mut Generator,
) -> Result<LLVMValueRef, anyhow::Error>
{
	let pointee_type = viewed_type.generate(llvm)?;
	let pointer_type = unsafe { LLVMPointerType(pointee_type, 0u32) };
	let address_value = unsafe {
		LLVMBuildPointerCast(llvm.builder, address, pointer_type, cstr!(""))
	};
	Ok(address_value)
}

fn generate_ext_array_view(
	address: LLVMValueRef,
	element_type: &ValueType,
	llvm: &mut Generator,
) -> Result<LLVMValueRef, anyhow::Error>
{
	let element_type = element_type.generate(llvm)?;
	let pointertype = unsafe { LLVMPointerType(element_type, 0u32) };
	let address_value = unsafe {
		LLVMBuildPointerCast(llvm.builder, address, pointertype, cstr!(""))
	};
	Ok(address_value)
}

fn generate_array_slice(
	address: LLVMValueRef,
	element_type: &ValueType,
	length: usize,
	llvm: &mut Generator,
) -> Result<LLVMValueRef, anyhow::Error>
{
	let length_value = llvm.const_usize(length);
	generate_slice_from_ptr_and_len(address, element_type, length_value, llvm)
}

fn generate_slice_from_ptr_and_len(
	address: LLVMValueRef,
	element_type: &ValueType,
	length_value: LLVMValueRef,
	llvm: &mut Generator,
) -> Result<LLVMValueRef, anyhow::Error>
{
	let element_type = element_type.generate(llvm)?;
	let storagetype = unsafe { LLVMArrayType(element_type, 0u32) };
	let pointertype = unsafe { LLVMPointerType(storagetype, 0u32) };
	let sizetype = ValueType::Usize.generate(llvm)?;
	let mut member_types = [pointertype, sizetype];
	let slice_type = unsafe {
		LLVMStructTypeInContext(
			llvm.context,
			member_types.as_mut_ptr(),
			member_types.len() as u32,
			0,
		)
	};
	let mut slice = unsafe { LLVMGetUndef(slice_type) };
	let address_value = unsafe {
		LLVMBuildPointerCast(llvm.builder, address, pointertype, cstr!(""))
	};
	slice = unsafe {
		LLVMBuildInsertValue(
			llvm.builder,
			slice,
			address_value,
			0u32,
			cstr!(""),
		)
	};
	slice = unsafe {
		LLVMBuildInsertValue(llvm.builder, slice, length_value, 1u32, cstr!(""))
	};
	Ok(slice)
}

fn generate_ptr_and_len_from_slice(
	slice: LLVMValueRef,
	llvm: &mut Generator,
) -> Result<(LLVMValueRef, LLVMValueRef), anyhow::Error>
{
	let slice_ptr =
		unsafe { LLVMBuildExtractValue(llvm.builder, slice, 0u32, cstr!("")) };
	let slice_ptr =
		generate_ext_array_view(slice_ptr, &ValueType::Char8, llvm)?;
	let slice_len =
		unsafe { LLVMBuildExtractValue(llvm.builder, slice, 1u32, cstr!("")) };
	Ok((slice_ptr, slice_len))
}

#[allow(unused)]
fn generate_const_string_literal(
	bytes: &[u8],
	_llvm: &mut Generator,
) -> Result<LLVMValueRef, anyhow::Error>
{
	let len = bytes.len() as u32;
	let byte_ptr: *const u8 = bytes.as_ptr();
	let initializer = unsafe { LLVMConstString(byte_ptr as *const i8, len, 1) };
	Ok(initializer)
}

fn generate_inplace_string_literal(
	bytes: &[u8],
	llvm: &mut Generator,
) -> Result<LLVMValueRef, anyhow::Error>
{
	let element_type: LLVMTypeRef = ValueType::Char8.generate(llvm)?;
	let mut values: Vec<LLVMValueRef> =
		bytes.iter().map(|b| llvm.const_u8(*b)).collect();
	let initializer = unsafe {
		LLVMConstArray(element_type, values.as_mut_ptr(), values.len() as u32)
	};
	Ok(initializer)
}

fn generate_global_string_literal(
	bytes: &[u8],
	llvm: &mut Generator,
) -> Result<LLVMValueRef, anyhow::Error>
{
	let initializer = generate_inplace_string_literal(bytes, llvm)?;
	let element_type: LLVMTypeRef = ValueType::Char8.generate(llvm)?;
	let num_elements = bytes.len() as u32;
	let array_type = unsafe { LLVMArrayType(element_type, num_elements) };
	let global =
		unsafe { LLVMAddGlobal(llvm.module, array_type, cstr!(".str")) };
	unsafe {
		LLVMSetGlobalConstant(global, 1);
		LLVMSetUnnamedAddr(global, 1);
		LLVMSetLinkage(global, LLVMLinkage::LLVMPrivateLinkage);
		LLVMSetInitializer(global, initializer);
	}
	Ok(global)
}

fn generate_global_cstr(
	cstr: CString,
	llvm: &mut Generator,
) -> Result<LLVMValueRef, anyhow::Error>
{
	let bytes = cstr.as_bytes_with_nul();
	let string = generate_global_string_literal(bytes, llvm)?;
	let result = unsafe {
		let mut indices = [llvm.const_i32(0), llvm.const_i32(0)];
		LLVMBuildGEP(
			llvm.builder,
			string,
			indices.as_mut_ptr(),
			indices.len() as u32,
			cstr!(""),
		)
	};
	Ok(result)
}

fn generate_global_nstr_and_len(
	nstr: &str,
	llvm: &mut Generator,
) -> Result<(LLVMValueRef, usize), anyhow::Error>
{
	let bytes = nstr.as_bytes();
	let len = bytes.len();
	let string = generate_global_string_literal(bytes, llvm)?;
	let result = unsafe {
		let mut indices = [llvm.const_i32(0), llvm.const_i32(0)];
		LLVMBuildGEP(
			llvm.builder,
			string,
			indices.as_mut_ptr(),
			indices.len() as u32,
			cstr!(""),
		)
	};
	Ok((result, len))
}

fn generate_structure_literal(
	structural_type: &ValueType,
	members: &[MemberExpression],
	llvm: &mut Generator,
) -> Result<LLVMValueRef, anyhow::Error>
{
	let structure_type = structural_type.generate(llvm)?;
	let mut structure = unsafe { LLVMGetUndef(structure_type) };
	for member in members
	{
		let value = member.expression.generate(llvm)?;
		let offset: u32 = member.offset.try_into()?;
		structure = unsafe {
			LLVMBuildInsertValue(
				llvm.builder,
				structure,
				value,
				offset,
				cstr!(""),
			)
		};
	}
	Ok(structure)
}

fn generate_autocoerce(
	expression: &Expression,
	coerced_type: &ValueType,
	llvm: &mut Generator,
) -> Result<LLVMValueRef, anyhow::Error>
{
	match coerced_type
	{
		ValueType::Slice { element_type } => match expression
		{
			Expression::Deref {
				reference,
				deref_type,
			} => match deref_type
			{
				ValueType::Array {
					element_type: _,
					length,
				} =>
				{
					let address = reference.generate_storage_address(llvm)?;
					generate_array_slice(address, element_type, *length, llvm)
				}
				_ => unimplemented!(),
			},
			Expression::ArrayLiteral {
				elements,
				element_type: _,
			} =>
			{
				let value = expression.generate(llvm)?;
				let length = elements.len();
				let array_type = ValueType::Array {
					element_type: element_type.clone(),
					length,
				};
				let vtype = array_type.generate(llvm)?;
				let address = generate_tmp_address(value, vtype, llvm)?;
				generate_array_slice(address, element_type, length, llvm)
			}
			Expression::StringLiteral { bytes } =>
			{
				let address = generate_global_string_literal(bytes, llvm)?;
				let length = bytes.len();
				generate_array_slice(address, element_type, length, llvm)
			}
			_ => unimplemented!(),
		},
		ValueType::SlicePointer { element_type } => match expression
		{
			Expression::Deref {
				reference,
				deref_type: x,
			} if reference.take_address => match x.get_pointee_type()
			{
				Some(ValueType::Array {
					element_type: _,
					length,
				}) =>
				{
					let address = reference.generate_storage_address(llvm)?;
					generate_array_slice(address, &element_type, length, llvm)
				}
				Some(_) => unimplemented!(),
				None => unreachable!(),
			},
			_ => unimplemented!(),
		},
		ValueType::View { deref_type } => match deref_type.as_ref()
		{
			ValueType::EndlessArray { element_type } => match expression
			{
				Expression::Deref {
					reference,
					deref_type: _,
				} =>
				{
					let address = reference.generate_storage_address(llvm)?;
					generate_ext_array_view(address, element_type, llvm)
				}
				Expression::StringLiteral { bytes } =>
				{
					let address = generate_global_string_literal(bytes, llvm)?;
					generate_ext_array_view(address, element_type, llvm)
				}
				_ => unimplemented!(),
			},
			viewed_type => match expression
			{
				Expression::Deref {
					reference,
					deref_type: _,
				} =>
				{
					let address = reference.generate_storage_address(llvm)?;
					generate_view(address, viewed_type, llvm)
				}
				expr =>
				{
					let value = expr.generate(llvm)?;
					let vtype = viewed_type.generate(llvm)?;
					let address = generate_tmp_address(value, vtype, llvm)?;
					generate_view(address, viewed_type, llvm)
				}
			},
		},
		ValueType::Pointer { deref_type } => match deref_type.as_ref()
		{
			ValueType::EndlessArray { element_type } => match expression
			{
				Expression::Deref {
					reference,
					deref_type: _,
				} =>
				{
					let address = reference.generate_storage_address(llvm)?;
					generate_ext_array_view(address, element_type, llvm)
				}
				_ => unimplemented!(),
			},
			inner_type => match expression
			{
				Expression::Deref {
					reference,
					deref_type: x,
				} if reference.take_address => match x.get_pointee_type()
				{
					Some(pointee_type) =>
					{
						let pointee = Expression::Deref {
							reference: Reference {
								take_address: false,
								..reference.clone()
							},
							deref_type: pointee_type,
						};
						let value =
							generate_autocoerce(&pointee, inner_type, llvm)?;
						let vtype = inner_type.generate(llvm)?;
						let tmp = generate_tmp_address(value, vtype, llvm)?;
						Ok(tmp)
					}
					None => unreachable!(),
				},
				_ => unimplemented!(),
			},
		},
		_ => unimplemented!(),
	}
}

fn generate_tmp_address(
	value: LLVMValueRef,
	vtype: LLVMTypeRef,
	llvm: &mut Generator,
) -> Result<LLVMValueRef, anyhow::Error>
{
	let tmp = unsafe { LLVMBuildAlloca(llvm.builder, vtype, cstr!("")) };
	unsafe {
		LLVMBuildStore(llvm.builder, value, tmp);
	}
	Ok(tmp)
}

fn generate_bitcast(
	expression: &Expression,
	coerced_type: &ValueType,
	llvm: &mut Generator,
) -> Result<LLVMValueRef, anyhow::Error>
{
	let value = expression.generate(llvm)?;
	let dest_type = coerced_type.generate(llvm)?;
	let result =
		unsafe { LLVMBuildBitCast(llvm.builder, value, dest_type, cstr!("")) };
	Ok(result)
}

fn generate_primitive_cast(
	expression: &Expression,
	expression_type: &ValueType,
	coerced_type: &ValueType,
	llvm: &mut Generator,
) -> Result<LLVMValueRef, anyhow::Error>
{
	let value = expression.generate(llvm)?;
	generate_conversion(value, expression_type, coerced_type, llvm)
}

fn generate_conversion(
	value: LLVMValueRef,
	value_type: &ValueType,
	coerced_type: &ValueType,
	llvm: &mut Generator,
) -> Result<LLVMValueRef, anyhow::Error>
{
	match (value_type, coerced_type)
	{
		(x, y) if x == y => unreachable!(),
		(vt, ct) if vt.is_integral() && ct.is_integral() =>
		{
			let src_type = value_type.generate(llvm)?;
			let src_size_in_bits = llvm.size_in_bits(src_type);
			let dest_type = coerced_type.generate(llvm)?;
			let dest_size_in_bits = llvm.size_in_bits(dest_type);
			let is_truncated = dest_size_in_bits < src_size_in_bits;
			let result = if is_truncated
			{
				unsafe {
					LLVMBuildTrunc(llvm.builder, value, dest_type, cstr!(""))
				}
			}
			else if vt.is_signed()
			{
				unsafe {
					LLVMBuildSExtOrBitCast(
						llvm.builder,
						value,
						dest_type,
						cstr!(""),
					)
				}
			}
			else
			{
				unsafe {
					LLVMBuildZExtOrBitCast(
						llvm.builder,
						value,
						dest_type,
						cstr!(""),
					)
				}
			};
			Ok(result)
		}
		(ValueType::Uint8, ValueType::Char8) => Ok(value),
		(ValueType::Char8, ValueType::Uint8) => Ok(value),
		(ValueType::Bool, ct) if ct.is_integral() =>
		{
			let dest_type = coerced_type.generate(llvm)?;
			let result = unsafe {
				LLVMBuildZExtOrBitCast(
					llvm.builder,
					value,
					dest_type,
					cstr!(""),
				)
			};
			Ok(result)
		}
		(_, _) => unreachable!(),
	}
}

#[must_use]
#[derive(Debug, Clone)]
pub enum GeneratorBuiltin
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

impl Generatable for GeneratorBuiltin
{
	type Item = LLVMValueRef;

	fn generate(
		&self,
		llvm: &mut Generator,
	) -> Result<Self::Item, anyhow::Error>
	{
		match self
		{
			GeneratorBuiltin::Abort =>
			{
				let function = llvm.get_trap_like_intrinsic("abort");
				let result = unsafe {
					LLVMBuildCall(
						llvm.builder,
						function,
						std::ptr::null_mut(),
						0u32,
						cstr!(""),
					)
				};
				Ok(result)
			}
			GeneratorBuiltin::Format { arguments } =>
			{
				generate_format(arguments, llvm)
			}
			GeneratorBuiltin::Write { fd, buffer } =>
			{
				let buffer_slice = buffer.generate(llvm)?;
				let (slice_ptr, slice_len) =
					generate_ptr_and_len_from_slice(buffer_slice, llvm)?;

				let fd = fd.generate(llvm)?;
				let mut arguments = [fd, slice_ptr, slice_len];

				let (function, return_type) = llvm.get_write_intrinsic();
				let result = unsafe {
					LLVMBuildCall(
						llvm.builder,
						function,
						arguments.as_mut_ptr(),
						arguments.len() as u32,
						cstr!(""),
					)
				};
				// TODO error handling with return type
				let _ = return_type;
				Ok(result)
			}
		}
	}
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

#[derive(Debug, Default)]
struct FormatBuffer
{
	format: Vec<u8>,
	inserted_arguments: Vec<LLVMValueRef>,
}

impl FormatBuffer
{
	fn add_user_text(
		&mut self,
		text: &str,
		llvm: &mut Generator,
	) -> Result<(), anyhow::Error>
	{
		let bytes = text.as_bytes();
		if bytes.iter().all(is_snprintf_safe)
		{
			self.format.extend(bytes);
		}
		else
		{
			let (value, len) = generate_global_nstr_and_len(text, llvm)?;
			self.add_specifier("%.*s");
			self.insert(llvm.const_usize(len));
			self.insert(value);
		}
		Ok(())
	}

	fn add_text(&mut self, text: &'static str)
	{
		let bytes = text.as_bytes();
		assert!(bytes.iter().all(is_snprintf_safe));
		self.format.extend(bytes);
	}

	fn add_specifier(&mut self, specifier: &'static str)
	{
		self.format.extend(specifier.as_bytes());
	}

	fn insert(&mut self, value: LLVMValueRef)
	{
		self.inserted_arguments.push(value);
	}
}

fn is_snprintf_safe(byte: &u8) -> bool
{
	match byte
	{
		b'%' => false,
		b'\0' => false,
		_ => true,
	}
}

fn generate_format(
	arguments: &[Expression],
	llvm: &mut Generator,
) -> Result<LLVMValueRef, anyhow::Error>
{
	let mut format_buffer = FormatBuffer::default();
	for argument in arguments
	{
		match argument
		{
			Expression::StringLiteral { bytes }
				if bytes.iter().all(is_snprintf_safe) =>
			{
				format_buffer.format.extend(&bytes[..]);
			}
			_ =>
			{
				format_arg(argument, llvm, &mut format_buffer)?;
			}
		}
	}

	if format_buffer.inserted_arguments.is_empty()
	{
		let bytes = &format_buffer.format[..];
		let address = generate_global_string_literal(bytes, llvm)?;
		let char_type = ValueType::Char8;
		let length = bytes.len();
		return generate_array_slice(address, &char_type, length, llvm);
	}

	let char_ptr_type = unsafe {
		let char_type = LLVMInt8TypeInContext(llvm.context);
		LLVMPointerType(char_type, 0)
	};

	let format = CString::new(format_buffer.format)?;
	let format = generate_global_cstr(format, llvm)?;

	let null_ptr = unsafe { LLVMConstNull(char_ptr_type) };
	let mut snprintf_arguments = vec![null_ptr, llvm.const_usize(0), format];
	snprintf_arguments.extend(format_buffer.inserted_arguments);

	let (function, return_type) = llvm.get_snprintf_intrinsic();
	let length_result = unsafe {
		LLVMBuildCall(
			llvm.builder,
			function,
			snprintf_arguments.as_mut_ptr(),
			snprintf_arguments.len() as u32,
			cstr!(""),
		)
	};
	// TODO error handling if length_result < 0
	let length_without_nul = generate_conversion(
		length_result,
		&return_type,
		&ValueType::Usize,
		llvm,
	)?;
	unsafe { LLVMSetValueName(length_without_nul, cstr!(".fmtlen")) };
	let length = unsafe {
		LLVMBuildAdd(
			llvm.builder,
			length_without_nul,
			llvm.const_usize(1),
			cstr!(".fmtbuflen"),
		)
	};

	let output_buffer = unsafe {
		let char_type = LLVMInt8TypeInContext(llvm.context);
		LLVMBuildArrayAlloca(llvm.builder, char_type, length, cstr!(".fmtbuf"))
	};

	snprintf_arguments[0] = output_buffer;
	snprintf_arguments[1] = length;

	let length_result = unsafe {
		LLVMBuildCall(
			llvm.builder,
			function,
			snprintf_arguments.as_mut_ptr(),
			snprintf_arguments.len() as u32,
			cstr!(""),
		)
	};
	// TODO error handling if length_result < 0
	let length_without_nul = generate_conversion(
		length_result,
		&return_type,
		&ValueType::Usize,
		llvm,
	)?;
	unsafe { LLVMSetValueName(length_without_nul, cstr!(".outlen")) };

	generate_slice_from_ptr_and_len(
		output_buffer,
		&ValueType::Char8,
		length_without_nul,
		llvm,
	)
}

fn format_arg(
	argument: &Expression,
	llvm: &mut Generator,
	buffer: &mut FormatBuffer,
) -> Result<(), anyhow::Error>
{
	match argument.value_type()
	{
		ValueType::Void => unreachable!(),
		ValueType::Int8 => format_arg_as_i64(argument, llvm, buffer),
		ValueType::Int16 => format_arg_as_i64(argument, llvm, buffer),
		ValueType::Int32 => format_arg_as_i64(argument, llvm, buffer),
		ValueType::Int64 => format_i64(argument, llvm, buffer),
		ValueType::Int128 => format_d128(argument, true, llvm, buffer),
		ValueType::Uint8 => format_arg_as_u64(argument, llvm, buffer),
		ValueType::Uint16 => format_arg_as_u64(argument, llvm, buffer),
		ValueType::Uint32 => format_arg_as_u64(argument, llvm, buffer),
		ValueType::Uint64 => format_u64(argument, llvm, buffer),
		ValueType::Uint128 => format_d128(argument, false, llvm, buffer),
		ValueType::Usize => format_arg_as_u64(argument, llvm, buffer),
		ValueType::Char8 =>
		{
			buffer.add_specifier("%c");
			buffer.insert(argument.generate(llvm)?);
			Ok(())
		}
		ValueType::Bool => format_bool(argument, llvm, buffer),
		ValueType::Array { .. } => format_slice(argument, llvm, buffer),
		ValueType::ArrayWithNamedLength { .. } => unreachable!(),
		ValueType::Slice { .. } => format_slice(argument, llvm, buffer),
		ValueType::SlicePointer { .. } => unreachable!(),
		ValueType::EndlessArray { .. } =>
		{
			format_endless(argument, llvm, buffer)
		}
		ValueType::Arraylike { .. } => unreachable!(),
		ValueType::Struct { identifier } =>
		{
			format_struct(argument, &identifier, llvm, buffer)
		}
		ValueType::Word {
			identifier,
			size_in_bytes: _,
		} => format_struct(argument, &identifier, llvm, buffer),
		ValueType::UnresolvedStructOrWord { .. } => unreachable!(),
		ValueType::Pointer { .. } =>
		{
			let address = argument.generate(llvm)?;
			buffer.add_specifier("%p");
			buffer.insert(address);
			Ok(())
		}
		ValueType::View { .. } => unreachable!(),
	}
}

fn format_i64(
	argument: &Expression,
	llvm: &mut Generator,
	buffer: &mut FormatBuffer,
) -> Result<(), anyhow::Error>
{
	let value = argument.generate(llvm)?;
	buffer.add_specifier("%lld");
	buffer.insert(value);
	Ok(())
}

fn format_u64(
	argument: &Expression,
	llvm: &mut Generator,
	buffer: &mut FormatBuffer,
) -> Result<(), anyhow::Error>
{
	let value = argument.generate(llvm)?;
	buffer.add_specifier("%llu");
	buffer.insert(value);
	Ok(())
}

fn format_arg_as_i64(
	argument: &Expression,
	llvm: &mut Generator,
	buffer: &mut FormatBuffer,
) -> Result<(), anyhow::Error>
{
	let value_type = argument.value_type();
	let value = argument.generate(llvm)?;
	let value =
		generate_conversion(value, &value_type, &ValueType::Int64, llvm)?;
	buffer.add_specifier("%lld");
	buffer.insert(value);
	Ok(())
}

fn format_arg_as_u64(
	argument: &Expression,
	llvm: &mut Generator,
	buffer: &mut FormatBuffer,
) -> Result<(), anyhow::Error>
{
	let value_type = argument.value_type();
	let value = argument.generate(llvm)?;
	let value =
		generate_conversion(value, &value_type, &ValueType::Uint64, llvm)?;
	buffer.add_specifier("%llu");
	buffer.insert(value);
	Ok(())
}

fn format_d128(
	argument: &Expression,
	is_signed: bool,
	llvm: &mut Generator,
	buffer: &mut FormatBuffer,
) -> Result<(), anyhow::Error>
{
	let value_type = argument.value_type().generate(llvm)?;
	let value = argument.generate(llvm)?;

	let i64_type = unsafe { LLVMInt64TypeInContext(llvm.context) };
	let i32_type = unsafe { LLVMInt32TypeInContext(llvm.context) };

	let (head, num_digits_tail, mid, low, nonneg) = unsafe {
		let zero = LLVMConstInt(value_type, 0u64, i32::from(is_signed));
		let (absvalue, nonneg) = if is_signed
		{
			let ge = LLVMIntPredicate::LLVMIntSGE;
			let n = LLVMBuildICmp(llvm.builder, ge, value, zero, cstr!(""));
			let absvalue = LLVMBuildSelect(
				llvm.builder,
				n,
				value,
				LLVMBuildNeg(llvm.builder, value, cstr!("")),
				cstr!(""),
			);
			let nonneg = LLVMBuildZExt(llvm.builder, n, i32_type, cstr!(""));
			(absvalue, nonneg)
		}
		else
		{
			(value, llvm.const_i32(1))
		};
		let ten16 = LLVMConstInt(i64_type, 10u64.pow(16), 1);
		let ten16 = LLVMBuildSExt(llvm.builder, ten16, value_type, cstr!(""));
		let q = LLVMBuildUDiv(llvm.builder, absvalue, ten16, cstr!(""));
		let l = LLVMBuildURem(llvm.builder, absvalue, ten16, cstr!(""));
		let eq = LLVMIntPredicate::LLVMIntEQ;
		let only_l = LLVMBuildICmp(llvm.builder, eq, q, zero, cstr!(""));
		let h = LLVMBuildUDiv(llvm.builder, q, ten16, cstr!(""));
		let m = LLVMBuildURem(llvm.builder, q, ten16, cstr!(""));
		let only_ml = LLVMBuildICmp(llvm.builder, eq, h, zero, cstr!(""));
		let h = LLVMBuildTrunc(llvm.builder, h, i64_type, cstr!(".high"));
		let m = LLVMBuildTrunc(llvm.builder, m, i64_type, cstr!(".mid"));
		let l = LLVMBuildTrunc(llvm.builder, l, i64_type, cstr!(".low"));
		let head = LLVMBuildSelect(
			llvm.builder,
			only_l,
			l,
			LLVMBuildSelect(llvm.builder, only_ml, m, h, cstr!("")),
			cstr!(".head"),
		);
		let offset = LLVMBuildSelect(
			llvm.builder,
			only_ml,
			llvm.const_i32(16),
			llvm.const_i32(32),
			cstr!(""),
		);
		let offset = LLVMBuildSelect(
			llvm.builder,
			only_l,
			llvm.const_i32(0),
			offset,
			cstr!(".offset"),
		);
		(head, offset, m, l, nonneg)
	};

	let buf_len = llvm.const_usize(41);
	let intermediate = unsafe {
		let char_type = LLVMInt8TypeInContext(llvm.context);
		LLVMBuildArrayAlloca(llvm.builder, char_type, buf_len, cstr!(".buf128"))
	};
	let var_num_characters_written_head =
		unsafe { LLVMBuildAlloca(llvm.builder, i32_type, cstr!(".n")) };

	let bytes = b"-%llu%n\0\0\0\0\0\0\0\0\0\
	              -%llu%n%016llu\0\0\
	              -%llu%n%4$016llu%3$016llu\0";
	let special_str = generate_global_string_literal(bytes, llvm)?;
	unsafe { LLVMSetValueName(special_str, cstr!(".fmt128l0ml0hml")) };
	let format = unsafe {
		let offset = LLVMBuildAdd(
			llvm.builder,
			num_digits_tail,
			nonneg,
			cstr!(".offset"),
		);
		let mut indices = [llvm.const_i32(0), offset];
		LLVMBuildGEP(
			llvm.builder,
			special_str,
			indices.as_mut_ptr(),
			indices.len() as u32,
			cstr!(""),
		)
	};

	let mut snprintf_arguments = vec![
		intermediate,
		buf_len,
		format,
		head,
		var_num_characters_written_head,
		low,
		mid,
	];

	let (function, return_type) = llvm.get_snprintf_intrinsic();
	let length_result = unsafe {
		LLVMBuildCall(
			llvm.builder,
			function,
			snprintf_arguments.as_mut_ptr(),
			snprintf_arguments.len() as u32,
			cstr!(""),
		)
	};
	// TODO error handling if length_result < 0
	let _ = (length_result, return_type);

	let num_characters = unsafe {
		let num_characters_written_head = LLVMBuildLoad(
			llvm.builder,
			var_num_characters_written_head,
			cstr!(".num_characters_written_head"),
		);
		LLVMBuildAdd(
			llvm.builder,
			num_characters_written_head,
			num_digits_tail,
			cstr!(""),
		)
	};

	buffer.add_specifier("%.*s");
	buffer.insert(num_characters);
	buffer.insert(intermediate);
	Ok(())
}

fn format_slice(
	argument: &Expression,
	llvm: &mut Generator,
	buffer: &mut FormatBuffer,
) -> Result<(), anyhow::Error>
{
	if let Some(ValueType::Char8) = argument.value_type().get_element_type()
	{
		let slice = Expression::Autocoerce {
			expression: Box::new(argument.clone()),
			coerced_type: ValueType::for_string_slice(),
		};
		let slice = slice.generate(llvm)?;
		let (slice_ptr, slice_len) =
			generate_ptr_and_len_from_slice(slice, llvm)?;
		buffer.add_specifier("%.*s");
		buffer.insert(slice_len);
		buffer.insert(slice_ptr);
		Ok(())
	}
	else
	{
		let slice = argument.generate(llvm)?;
		let (slice_ptr, slice_len) =
			generate_ptr_and_len_from_slice(slice, llvm)?;
		// let n = ?
		// for i in 0..n {
		//
		// }
		// create format string that is n times %.*s
		// for i in 0..n {
		// TODO call the same llvm IR snippet for multiple Expressions
		// TODO my Generator currently doesn't have a nice way to do that
		// }
		// LLVMBuildArrayAlloca
		buffer.add_text("[");
		// TODO print elements
		// buffer.add_specifier("%.*s");
		// buffer.insert(formatted_elements_len);
		// buffer.insert(formatted_elements_ptr);
		buffer.add_text("]");
		Ok(())
	}
}

fn format_bool(
	argument: &Expression,
	llvm: &mut Generator,
	buffer: &mut FormatBuffer,
) -> Result<(), anyhow::Error>
{
	let value_type = argument.value_type();
	let value = argument.generate(llvm)?;
	let value =
		generate_conversion(value, &value_type, &ValueType::Uint32, llvm)?;

	// Calculate offset = value ? 8 : 0 using bitshift.
	let three = llvm.const_i32(3);
	let offset = unsafe { LLVMBuildShl(llvm.builder, value, three, cstr!("")) };

	// A little hack to have "true" start at offset 8.
	let bytes = b"false\0\0\0true\0";
	let special_str = generate_global_string_literal(bytes, llvm)?;
	unsafe { LLVMSetValueName(special_str, cstr!(".falsetrue")) };

	let mut indices = [llvm.const_i32(0), offset];
	let offset_str = unsafe {
		LLVMBuildGEP(
			llvm.builder,
			special_str,
			indices.as_mut_ptr(),
			indices.len() as u32,
			cstr!(""),
		)
	};

	buffer.add_specifier("%s");
	buffer.insert(offset_str);
	Ok(())
}

#[allow(unused_unsafe)]
fn format_endless(
	argument: &Expression,
	llvm: &mut Generator,
	buffer: &mut FormatBuffer,
) -> Result<(), anyhow::Error>
{
	let cstr_address = match argument
	{
		Expression::Deref {
			reference: Reference { base, steps, .. },
			deref_type: ValueType::EndlessArray { element_type },
		} =>
		{
			if **element_type == ValueType::Char8
			{
				match steps.iter().last()
				{
					Some(ReferenceStep::Autoderef) =>
					{
						let n = steps.len() - 1;
						let steps = steps[..n].iter().cloned().collect();
						let expression = Expression::Deref {
							reference: Reference {
								base: base.clone(),
								steps,
								take_address: false,
							},
							deref_type: ValueType::Pointer {
								deref_type: Box::new(ValueType::EndlessArray {
									element_type: Box::new(ValueType::Char8),
								}),
							},
						};
						let address = expression.generate(llvm)?;
						Some(address)
					}
					_ => None,
				}
			}
			else
			{
				None
			}
		}
		_ => None,
	};
	if let Some(address) = cstr_address
	{
		// Assume that [...]char8 is nul terminated.
		let nul_terminated_cstr = unsafe { address };
		buffer.add_specifier("%s");
		buffer.insert(nul_terminated_cstr);
		Ok(())
	}
	else
	{
		let text = "...";
		let (placeholder, len) = generate_global_nstr_and_len(&text, llvm)?;
		buffer.add_specifier("%.*s");
		buffer.insert(llvm.const_usize(len));
		buffer.insert(placeholder);
		Ok(())
	}
}

fn format_struct(
	argument: &Expression,
	struct_name: &Identifier,
	llvm: &mut Generator,
	buffer: &mut FormatBuffer,
) -> Result<(), anyhow::Error>
{
	buffer.add_user_text(&struct_name.name, llvm)?;
	buffer.add_text(" {");

	let sname = CString::new(&struct_name.name as &str)?;
	let struct_type = unsafe { LLVMGetTypeByName(llvm.module, sname.as_ptr()) };
	// TODO print members

	buffer.add_text("}");
	Ok(())
}

impl Generatable for builtin::Fd
{
	type Item = LLVMValueRef;

	fn generate(
		&self,
		llvm: &mut Generator,
	) -> Result<Self::Item, anyhow::Error>
	{
		match self
		{
			Self::Stdout => Ok(llvm.const_i32(1)),
			Self::Stderr => Ok(llvm.const_i32(2)),
		}
	}
}

#[cfg_attr(coverage, no_coverage)]
#[cfg(not(tarpaulin_include))]
#[allow(unused)]
fn debug_print_value_and_type(name: &str, value: LLVMValueRef)
{
	unsafe {
		println!(
			"{} = {:?}",
			name,
			CStr::from_ptr(LLVMPrintValueToString(value))
		);
	}
	let value_type = unsafe { LLVMTypeOf(value) };
	unsafe {
		println!(
			"type of {} = {:?}",
			name,
			CStr::from_ptr(LLVMPrintTypeToString(value_type))
		);
	}
}
