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

pub const DEFAULT_DATA_LAYOUT: &str = "e-m:e-p:64:64-i64:64-n8:16:32:64-S128";

pub fn generate(
	program: &Vec<Declaration>,
	source_filename: &str,
	for_wasm: bool,
) -> Result<String, anyhow::Error>
{
	let mut generator = Generator::new(source_filename)?;
	if for_wasm
	{
		generator.for_wasm()?;
	}

	for declaration in organize(program)
	{
		declare(declaration, &mut generator)?;
	}
	for declaration in program
	{
		declaration.generate(&mut generator)?;
	}

	generator.verify();
	let ircode = generator.generate_ir()?;
	Ok(ircode)
}

struct Generator
{
	module: *mut LLVMModule,
	context: *mut LLVMContext,
	builder: *mut LLVMBuilder,
	global_variables: std::collections::HashMap<u32, LLVMValueRef>,
	global_functions: std::collections::HashMap<u32, LLVMValueRef>,
	local_parameters: std::collections::HashMap<u32, LLVMValueRef>,
	local_variables: std::collections::HashMap<u32, LLVMValueRef>,
	local_labeled_blocks: std::collections::HashMap<u32, LLVMBasicBlockRef>,
	type_of_usize: LLVMTypeRef,
}

impl Generator
{
	fn new(module_name: &str) -> Result<Generator, anyhow::Error>
	{
		let module_name = CString::new(module_name)?;
		let generator = unsafe {
			let context = LLVMContextCreate();
			let module = LLVMModuleCreateWithNameInContext(
				module_name.as_ptr(),
				context,
			);
			LLVMSetTarget(module, LLVMGetDefaultTargetTriple());
			let data_layout = CString::new(DEFAULT_DATA_LAYOUT)?;
			LLVMSetDataLayout(module, data_layout.as_ptr());
			let builder = LLVMCreateBuilderInContext(context);
			let type_of_usize = LLVMInt64TypeInContext(context);
			Generator {
				module,
				context,
				builder,
				global_variables: std::collections::HashMap::new(),
				global_functions: std::collections::HashMap::new(),
				local_parameters: std::collections::HashMap::new(),
				local_variables: std::collections::HashMap::new(),
				local_labeled_blocks: std::collections::HashMap::new(),
				type_of_usize,
			}
		};
		Ok(generator)
	}

	fn for_wasm(&mut self) -> Result<(), anyhow::Error>
	{
		unsafe {
			let triple = CString::new("wasm32-unknown-wasi")?;
			LLVMSetTarget(self.module, triple.as_ptr());
			let data_layout = "e-p:32:32-i64:64-n32:64-S64";
			let data_layout = CString::new(data_layout)?;
			LLVMSetDataLayout(self.module, data_layout.as_ptr());
			self.type_of_usize = LLVMInt32TypeInContext(self.context);
			Ok(())
		}
	}

	fn verify(&self)
	{
		unsafe {
			LLVMVerifyModule(
				self.module,
				LLVMVerifierFailureAction::LLVMPrintMessageAction,
				std::ptr::null_mut(),
			);
		}
	}

	fn verify_function(&self, function: LLVMValueRef)
	{
		unsafe {
			LLVMVerifyFunction(
				function,
				LLVMVerifierFailureAction::LLVMPrintMessageAction,
			);
		}
	}

	fn generate_ir(&self) -> Result<String, anyhow::Error>
	{
		let ircode = unsafe {
			let raw = LLVMPrintModuleToString(self.module);
			let ircode = CStr::from_ptr(raw).to_owned();
			LLVMDisposeMessage(raw);
			ircode
		};
		let ircode = ircode.into_string()?;
		Ok(ircode)
	}

	fn const_bool(&mut self, value: bool) -> LLVMValueRef
	{
		unsafe {
			let bytetype = LLVMInt1TypeInContext(self.context);
			LLVMConstInt(bytetype, value as u64, 0)
		}
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
}

impl Drop for Generator
{
	fn drop(&mut self)
	{
		unsafe {
			LLVMDisposeBuilder(self.builder);
			LLVMDisposeModule(self.module);
			LLVMContextDispose(self.context);
		}
	}
}

fn organize(declarations: &[Declaration]) -> Vec<&Declaration>
{
	let mut declarations: Vec<&Declaration> = declarations.iter().collect();
	// Sort structure declarations before other declarations (infty),
	// and sort the structure declarations by their depth, from low to high.
	let n = declarations.len();
	declarations.sort_by_key(|x| get_structure_depth(x, n));
	declarations
}

fn get_structure_depth(declaration: &Declaration, max: usize) -> usize
{
	match declaration
	{
		Declaration::Constant { .. } => max,
		Declaration::Function { .. } => max,
		Declaration::FunctionHead { .. } => max,
		Declaration::Structure { depth, .. } => *depth as usize,
	}
}

fn declare(
	declaration: &Declaration,
	llvm: &mut Generator,
) -> Result<(), anyhow::Error>
{
	match declaration
	{
		Declaration::Constant { .. } => Ok(()),
		Declaration::Function {
			name,
			parameters,
			body: _,
			return_type,
			flags: _,
		}
		| Declaration::FunctionHead {
			name,
			parameters,
			return_type,
			flags: _,
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

			llvm.global_functions.insert(name.resolution_id, function);

			Ok(())
		}
		Declaration::Structure {
			name,
			members,
			flags: _,
			size_in_bytes: _,
			depth: _,
		} =>
		{
			let name = CString::new(&name.name as &str)?;
			let struct_type =
				unsafe { LLVMStructCreateNamed(llvm.context, name.as_ptr()) };

			let member_types: Result<Vec<LLVMTypeRef>, anyhow::Error> = members
				.iter()
				.map(|m| m.value_type.generate(llvm))
				.collect();
			let mut member_types = member_types?;
			let is_packed = 1;

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
			Declaration::Constant {
				name,
				value,
				value_type,
				flags: _,
			} =>
			{
				let cname = CString::new(&name.name as &str)?;
				let vartype = value_type.generate(llvm)?;
				let global = unsafe {
					LLVMAddGlobal(llvm.module, vartype, cname.as_ptr())
				};
				llvm.global_variables.insert(name.resolution_id, global);
				let constant = value.generate(llvm)?;
				unsafe {
					LLVMSetInitializer(global, constant);
					LLVMSetGlobalConstant(global, 1);
				}
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

				let entry_block_name = CString::new("entry")?;
				unsafe {
					let entry_block = LLVMAppendBasicBlockInContext(
						llvm.context,
						function,
						entry_block_name.as_ptr(),
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
			Declaration::FunctionHead {
				name,
				parameters: _,
				return_type: _,
				flags,
			} =>
			{
				let function = llvm.global_functions.get(&name.resolution_id);
				let function = match function
				{
					Some(function) => *function,
					None => unreachable!(),
				};

				if flags.contains(DeclarationFlag::External)
				{
					unsafe {
						LLVMSetLinkage(
							function,
							LLVMLinkage::LLVMExternalLinkage,
						);
					}
				}
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
			let cname = CString::new("looped-block")?;
			let inner_block = unsafe {
				let current_block = LLVMGetInsertBlock(llvm.builder);
				let function = LLVMGetBasicBlockParent(current_block);
				let inner_block = LLVMAppendBasicBlockInContext(
					llvm.context,
					function,
					cname.as_ptr(),
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

			let cname = CString::new("after-looped-block")?;
			unsafe {
				let function = LLVMGetBasicBlockParent(inner_block);
				LLVMBuildBr(llvm.builder, inner_block);
				let after_block = LLVMAppendBasicBlockInContext(
					llvm.context,
					function,
					cname.as_ptr(),
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
			Statement::MethodCall { name, arguments } =>
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

				let tmpname = CString::new("")?;
				unsafe {
					LLVMBuildCall(
						llvm.builder,
						function,
						arguments.as_mut_ptr(),
						arguments.len() as u32,
						tmpname.as_ptr(),
					);
				}
				Ok(())
			}
			Statement::Loop { .. } => unreachable!(),
			Statement::Goto { label } =>
			{
				let current_block = unsafe { LLVMGetInsertBlock(llvm.builder) };
				let cname = CString::new("unreachable-after-goto")?;
				let unreachable_block = unsafe {
					let function = LLVMGetBasicBlockParent(current_block);
					let unreachable_block = LLVMAppendBasicBlockInContext(
						llvm.context,
						function,
						cname.as_ptr(),
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

				let cname = CString::new("then")?;
				let then_start_block = unsafe {
					let then_block = LLVMAppendBasicBlockInContext(
						llvm.context,
						function,
						cname.as_ptr(),
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
					let cname = CString::new("else")?;
					let start_block = unsafe {
						let else_block = LLVMAppendBasicBlockInContext(
							llvm.context,
							function,
							cname.as_ptr(),
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

				let cname = CString::new("after")?;
				let after_block = unsafe {
					let after_block = LLVMAppendBasicBlockInContext(
						llvm.context,
						function,
						cname.as_ptr(),
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
		let is_signed = match self.compared_type
		{
			ValueType::Int8 => true,
			ValueType::Int16 => true,
			ValueType::Int32 => true,
			ValueType::Int64 => true,
			ValueType::Int128 => true,
			_ => false,
		};
		let left = self.left.generate(llvm)?;
		let right = self.right.generate(llvm)?;
		let name = CString::new("")?;
		let pred = match self.op
		{
			ComparisonOp::Equals => llvm_sys::LLVMIntPredicate::LLVMIntEQ,
			ComparisonOp::DoesNotEqual => llvm_sys::LLVMIntPredicate::LLVMIntNE,
			ComparisonOp::IsGreater if is_signed =>
			{
				llvm_sys::LLVMIntPredicate::LLVMIntSGT
			}
			ComparisonOp::IsGreater => llvm_sys::LLVMIntPredicate::LLVMIntUGT,
			ComparisonOp::IsLess if is_signed =>
			{
				llvm_sys::LLVMIntPredicate::LLVMIntSLT
			}
			ComparisonOp::IsLess => llvm_sys::LLVMIntPredicate::LLVMIntULT,
			ComparisonOp::IsGE if is_signed =>
			{
				llvm_sys::LLVMIntPredicate::LLVMIntSGE
			}
			ComparisonOp::IsGE => llvm_sys::LLVMIntPredicate::LLVMIntUGE,
			ComparisonOp::IsLE if is_signed =>
			{
				llvm_sys::LLVMIntPredicate::LLVMIntSLE
			}
			ComparisonOp::IsLE => llvm_sys::LLVMIntPredicate::LLVMIntULE,
		};
		let result = unsafe {
			LLVMBuildICmp(llvm.builder, pred, left, right, name.as_ptr())
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
				let is_signed = match value_type
				{
					ValueType::Int8 => true,
					ValueType::Int16 => true,
					ValueType::Int32 => true,
					ValueType::Int64 => true,
					ValueType::Int128 => true,
					_ => false,
				};
				let left = left.generate(llvm)?;
				let right = right.generate(llvm)?;
				let name = CString::new("")?;
				let result = match op
				{
					BinaryOp::Add =>
					unsafe {
						LLVMBuildAdd(llvm.builder, left, right, name.as_ptr())
					},
					BinaryOp::Subtract =>
					unsafe {
						LLVMBuildSub(llvm.builder, left, right, name.as_ptr())
					},
					BinaryOp::Multiply =>
					unsafe {
						LLVMBuildMul(llvm.builder, left, right, name.as_ptr())
					},
					BinaryOp::Divide if is_signed =>
					unsafe {
						LLVMBuildSDiv(llvm.builder, left, right, name.as_ptr())
					},
					BinaryOp::Divide =>
					unsafe {
						LLVMBuildUDiv(llvm.builder, left, right, name.as_ptr())
					},
					BinaryOp::Modulo if is_signed =>
					unsafe {
						LLVMBuildSRem(llvm.builder, left, right, name.as_ptr())
					},
					BinaryOp::Modulo =>
					unsafe {
						LLVMBuildURem(llvm.builder, left, right, name.as_ptr())
					},
					BinaryOp::BitwiseAnd =>
					unsafe {
						LLVMBuildAnd(llvm.builder, left, right, name.as_ptr())
					},
					BinaryOp::BitwiseOr =>
					unsafe {
						LLVMBuildOr(llvm.builder, left, right, name.as_ptr())
					},
					BinaryOp::BitwiseXor =>
					unsafe {
						LLVMBuildXor(llvm.builder, left, right, name.as_ptr())
					},
					BinaryOp::ShiftLeft =>
					unsafe {
						LLVMBuildShl(llvm.builder, left, right, name.as_ptr())
					},
					BinaryOp::ShiftRight if is_signed =>
					unsafe {
						LLVMBuildAShr(llvm.builder, left, right, name.as_ptr())
					},
					BinaryOp::ShiftRight =>
					unsafe {
						LLVMBuildLShr(llvm.builder, left, right, name.as_ptr())
					},
				};
				Ok(result)
			}
			Expression::Unary { op, expression } =>
			{
				let expr = expression.generate(llvm)?;
				let name = CString::new("")?;
				let result = match op
				{
					UnaryOp::Negative =>
					unsafe {
						LLVMBuildNeg(llvm.builder, expr, name.as_ptr())
					},
					UnaryOp::BitwiseComplement =>
					unsafe {
						LLVMBuildNot(llvm.builder, expr, name.as_ptr())
					},
				};
				Ok(result)
			}
			Expression::PrimitiveLiteral(literal) => literal.generate(llvm),
			Expression::NakedIntegerLiteral { value, value_type } =>
			{
				// Naked integers are allowed to be 64-bits,
				// thus between i64::MIN and u64::MAX.
				let value: i128 = *value;
				let signed = value < 0;
				let value_bits: u64 = if signed
				{
					(value as i64) as u64
				}
				else
				{
					value as u64
				};
				let inttype = value_type.generate(llvm)?;
				unsafe { Ok(LLVMConstInt(inttype, value_bits, signed as i32)) }
			}
			Expression::BitIntegerLiteral { value, value_type } =>
			{
				let value_bits: u64 = *value;
				match value_type
				{
					ValueType::Pointer { deref_type: _ }
					| ValueType::View { deref_type: _ } =>
					{
						let pointertype = value_type.generate(llvm)?;
						let address = value_bits as usize;
						let value = llvm.const_usize(address);
						unsafe { Ok(LLVMConstIntToPtr(value, pointertype)) }
					}
					_ =>
					{
						let inttype = value_type.generate(llvm)?;
						unsafe { Ok(LLVMConstInt(inttype, value_bits, 0)) }
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
					let tmpname = CString::new("")?;
					result = unsafe {
						LLVMBuildInsertValue(
							llvm.builder,
							result,
							v,
							i,
							tmpname.as_ptr(),
						)
					};
				}
				Ok(result)
			}
			Expression::StringLiteral { bytes: _ } => unimplemented!(),
			Expression::ByteStringLiteral { bytes } =>
			{
				let element_type: LLVMTypeRef =
					ValueType::Uint8.generate(llvm)?;
				let mut values = Vec::with_capacity(bytes.len());
				for byte in bytes
				{
					let value = llvm.const_u8(*byte);
					values.push(value);
				}
				let initializer = unsafe {
					LLVMConstArray(
						element_type,
						values.as_mut_ptr(),
						values.len() as u32,
					)
				};
				let num_elements: u32 = bytes.len() as u32;
				let array_type =
					unsafe { LLVMArrayType(element_type, num_elements) };
				let strname = CString::new(".str")?;
				let global = unsafe {
					LLVMAddGlobal(llvm.module, array_type, strname.as_ptr())
				};
				unsafe {
					LLVMSetGlobalConstant(global, 1);
					LLVMSetUnnamedAddr(global, 1);
					LLVMSetLinkage(global, LLVMLinkage::LLVMPrivateLinkage);
					LLVMSetInitializer(global, initializer);
				}
				Ok(global)
			}
			Expression::Structural {
				structural_type,
				members,
			} => generate_structure_literal(structural_type, members, llvm),
			Expression::Deref {
				reference,
				deref_type,
			} => reference.generate_deref(deref_type, llvm),
			Expression::Autocoerce {
				expression,
				coerced_type,
			} => generate_autocoerce(expression, coerced_type, llvm),
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
				reference.generate_array_len(llvm)
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

				let function_name = CString::new(&name.name as &str)?;
				let result = unsafe {
					LLVMBuildCall(
						llvm.builder,
						function,
						arguments.as_mut_ptr(),
						arguments.len() as u32,
						function_name.as_ptr(),
					)
				};
				Ok(result)
			}
		}
	}
}

impl Generatable for PrimitiveLiteral
{
	type Item = LLVMValueRef;

	fn generate(
		&self,
		llvm: &mut Generator,
	) -> Result<Self::Item, anyhow::Error>
	{
		match self
		{
			PrimitiveLiteral::Int8(value) =>
			unsafe {
				let inttype = LLVMInt8TypeInContext(llvm.context);
				Ok(LLVMConstInt(inttype, *value as u64, 1))
			},
			PrimitiveLiteral::Int16(value) =>
			unsafe {
				let inttype = LLVMInt16TypeInContext(llvm.context);
				Ok(LLVMConstInt(inttype, *value as u64, 1))
			},
			PrimitiveLiteral::Int32(value) =>
			unsafe {
				let inttype = LLVMInt32TypeInContext(llvm.context);
				Ok(LLVMConstInt(inttype, *value as u64, 1))
			},
			PrimitiveLiteral::Int64(value) =>
			unsafe {
				let inttype = LLVMInt64TypeInContext(llvm.context);
				Ok(LLVMConstInt(inttype, *value as u64, 1))
			},
			PrimitiveLiteral::Int128(value) =>
			unsafe {
				let inttype = LLVMInt128TypeInContext(llvm.context);
				let value: i128 = *value;
				let value_bits: u128 = value as u128;
				let words = [
					((value_bits >> 64) & 0xFFFFFFFF) as u64,
					(value_bits & 0xFFFFFFFF) as u64,
				];
				Ok(LLVMConstIntOfArbitraryPrecision(inttype, 2, words.as_ptr()))
			},
			PrimitiveLiteral::Uint8(value) =>
			unsafe {
				let inttype = LLVMInt8TypeInContext(llvm.context);
				Ok(LLVMConstInt(inttype, *value as u64, 0))
			},
			PrimitiveLiteral::Uint16(value) =>
			unsafe {
				let inttype = LLVMInt16TypeInContext(llvm.context);
				Ok(LLVMConstInt(inttype, *value as u64, 0))
			},
			PrimitiveLiteral::Uint32(value) =>
			unsafe {
				let inttype = LLVMInt32TypeInContext(llvm.context);
				Ok(LLVMConstInt(inttype, *value as u64, 0))
			},
			PrimitiveLiteral::Uint64(value) =>
			unsafe {
				let inttype = LLVMInt64TypeInContext(llvm.context);
				Ok(LLVMConstInt(inttype, *value as u64, 0))
			},
			PrimitiveLiteral::Uint128(value) =>
			unsafe {
				let inttype = LLVMInt128TypeInContext(llvm.context);
				let value: u128 = *value;
				let value_bits: u128 = value as u128;
				let words = [
					((value_bits >> 64) & 0xFFFFFFFF) as u64,
					(value_bits & 0xFFFFFFFF) as u64,
				];
				Ok(LLVMConstIntOfArbitraryPrecision(inttype, 2, words.as_ptr()))
			},
			PrimitiveLiteral::Usize(value) => Ok(llvm.const_usize(*value)),
			PrimitiveLiteral::Bool(value) => Ok(llvm.const_bool(*value)),
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
			ValueType::Bool =>
			unsafe { LLVMInt1TypeInContext(llvm.context) },
			ValueType::Char =>
			unsafe { LLVMInt32TypeInContext(llvm.context) },
			ValueType::String => unimplemented!(),
			ValueType::Array {
				element_type,
				length,
			} =>
			{
				let element_type = element_type.generate(llvm)?;
				unsafe { LLVMArrayType(element_type, *length as u32) }
			}
			ValueType::Slice { element_type } =>
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
			ValueType::Struct {
				identifier,
				size_in_bytes: _,
			}
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
			let tmpname = CString::new("")?;
			unsafe { LLVMBuildLoad(llvm.builder, address, tmpname.as_ptr()) }
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
					let tmpname = CString::new("")?;
					let addr = value;
					let offset: u8 = *offset;
					let offset: u32 = offset.into();
					value = unsafe {
						LLVMBuildExtractValue(
							llvm.builder,
							addr,
							offset,
							tmpname.as_ptr(),
						)
					};
				}
				ReferenceStep::Member { offset } =>
				{
					let tmpname = CString::new("")?;
					let addr = value;
					let offset: usize = *offset;
					let offset: u32 = offset.try_into()?;
					value = unsafe {
						LLVMBuildExtractValue(
							llvm.builder,
							addr,
							offset,
							tmpname.as_ptr(),
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
				// We assume that the parameter is ValueType::Slice.
				let param = *value;
				let tmpname = CString::new("")?;
				let tmp = unsafe {
					LLVMBuildExtractValue(
						llvm.builder,
						param,
						0u32,
						tmpname.as_ptr(),
					)
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
						Some(ReferenceStep::Member { .. }) => true,
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
						let tmpname = CString::new("")?;
						addr = unsafe {
							LLVMBuildGEP(
								llvm.builder,
								addr,
								indices.as_mut_ptr(),
								indices.len() as u32,
								tmpname.as_ptr(),
							)
						};
					}
					let tmpname = CString::new("")?;
					addr = unsafe {
						LLVMBuildLoad(llvm.builder, addr, tmpname.as_ptr())
					};
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
						let tmpname = CString::new("")?;
						addr = unsafe {
							LLVMBuildExtractValue(
								llvm.builder,
								addr,
								0,
								tmpname.as_ptr(),
							)
						};
					}
					else
					{
						indices.push(llvm.const_i32(0));
						let tmpname = CString::new("")?;
						addr = unsafe {
							LLVMBuildGEP(
								llvm.builder,
								addr,
								indices.as_mut_ptr(),
								indices.len() as u32,
								tmpname.as_ptr(),
							)
						};
						let tmpname = CString::new("")?;
						addr = unsafe {
							LLVMBuildLoad(llvm.builder, addr, tmpname.as_ptr())
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
			let tmpname = CString::new("")?;
			addr = unsafe {
				LLVMBuildGEP(
					llvm.builder,
					addr,
					indices.as_mut_ptr(),
					indices.len() as u32,
					tmpname.as_ptr(),
				)
			};
		}

		Ok(addr)
	}

	fn generate_array_len(
		&self,
		llvm: &mut Generator,
	) -> Result<LLVMValueRef, anyhow::Error>
	{
		let address = self.generate_storage_address(llvm)?;
		let pointer_type = unsafe { LLVMTypeOf(address) };
		let array_type = unsafe { LLVMGetElementType(pointer_type) };
		let length: u32 = unsafe { LLVMGetArrayLength(array_type) };
		let result = llvm.const_usize(length as usize);
		Ok(result)
	}
}

fn generate_view(
	address: LLVMValueRef,
	viewed_type: &ValueType,
	llvm: &mut Generator,
) -> Result<LLVMValueRef, anyhow::Error>
{
	let tmpname = CString::new("")?;
	let pointee_type = viewed_type.generate(llvm)?;
	let pointer_type = unsafe { LLVMPointerType(pointee_type, 0u32) };
	let address_value = unsafe {
		LLVMBuildPointerCast(
			llvm.builder,
			address,
			pointer_type,
			tmpname.as_ptr(),
		)
	};
	Ok(address_value)
}

fn generate_ext_array_view(
	address: LLVMValueRef,
	element_type: &ValueType,
	llvm: &mut Generator,
) -> Result<LLVMValueRef, anyhow::Error>
{
	let tmpname = CString::new("")?;
	let element_type = element_type.generate(llvm)?;
	let pointertype = unsafe { LLVMPointerType(element_type, 0u32) };
	let address_value = unsafe {
		LLVMBuildPointerCast(
			llvm.builder,
			address,
			pointertype,
			tmpname.as_ptr(),
		)
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
	let tmpname = CString::new("")?;
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
		LLVMBuildPointerCast(
			llvm.builder,
			address,
			pointertype,
			tmpname.as_ptr(),
		)
	};
	slice = unsafe {
		LLVMBuildInsertValue(
			llvm.builder,
			slice,
			address_value,
			0u32,
			tmpname.as_ptr(),
		)
	};
	let length_value = llvm.const_usize(length);
	slice = unsafe {
		LLVMBuildInsertValue(
			llvm.builder,
			slice,
			length_value,
			1u32,
			tmpname.as_ptr(),
		)
	};
	Ok(slice)
}

fn generate_structure_literal(
	structural_type: &ValueType,
	members: &[MemberExpression],
	llvm: &mut Generator,
) -> Result<LLVMValueRef, anyhow::Error>
{
	let tmpname = CString::new("")?;
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
				tmpname.as_ptr(),
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
					element_type,
					length,
				} =>
				{
					let address = reference.generate_storage_address(llvm)?;
					generate_array_slice(address, element_type, *length, llvm)
				}
				_ => unimplemented!(),
			},
			Expression::StringLiteral { .. } => unimplemented!(),
			Expression::ByteStringLiteral { bytes } =>
			{
				let address = expression.generate(llvm)?;
				let length = bytes.len();
				generate_array_slice(address, element_type, length, llvm)
			}
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
				Expression::StringLiteral { .. } => unimplemented!(),
				Expression::ByteStringLiteral { .. } =>
				{
					let address = expression.generate(llvm)?;
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
				_ => unimplemented!(),
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
				Expression::StringLiteral { .. } => unimplemented!(),
				Expression::ByteStringLiteral { bytes: _ } =>
				{
					let address = expression.generate(llvm)?;
					generate_ext_array_view(address, element_type, llvm)
				}
				_ => unimplemented!(),
			},
			inner_type => match expression
			{
				Expression::Deref {
					reference,
					deref_type: expr_type,
				} if reference.take_address => match expr_type.get_pointee_type()
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
						let tmpname = CString::new("")?;
						let vartype = inner_type.generate(llvm)?;
						let tmp = unsafe {
							LLVMBuildAlloca(
								llvm.builder,
								vartype,
								tmpname.as_ptr(),
							)
						};
						let value =
							generate_autocoerce(&pointee, inner_type, llvm)?;
						unsafe {
							LLVMBuildStore(llvm.builder, value, tmp);
						}
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

fn generate_primitive_cast(
	expression: &Expression,
	expression_type: &ValueType,
	coerced_type: &ValueType,
	llvm: &mut Generator,
) -> Result<LLVMValueRef, anyhow::Error>
{
	let value = expression.generate(llvm)?;
	generate_cast(value, expression_type, coerced_type, llvm)
}

fn generate_cast(
	value: LLVMValueRef,
	value_type: &ValueType,
	coerced_type: &ValueType,
	llvm: &mut Generator,
) -> Result<LLVMValueRef, anyhow::Error>
{
	match (value_type, coerced_type)
	{
		(x, y) if x == y => Ok(value),
		(ValueType::Usize, _) =>
		{
			let interim_type = ValueType::Uint128;
			let dest_type = interim_type.generate(llvm)?;
			let tmpname = CString::new("")?;
			let interim = unsafe {
				LLVMBuildZExtOrBitCast(
					llvm.builder,
					value,
					dest_type,
					tmpname.as_ptr(),
				)
			};
			generate_cast(interim, &interim_type, coerced_type, llvm)
		}
		(_, ValueType::Usize) =>
		{
			let interim_type = ValueType::Uint128;
			let value = generate_cast(value, value_type, &interim_type, llvm)?;
			let dest_type = coerced_type.generate(llvm)?;
			let tmpname = CString::new("")?;
			let result = unsafe {
				LLVMBuildTruncOrBitCast(
					llvm.builder,
					value,
					dest_type,
					tmpname.as_ptr(),
				)
			};
			Ok(result)
		}
		(vt, ct) if vt.is_integral() && ct.is_integral() =>
		{
			let dest_type = coerced_type.generate(llvm)?;
			let tmpname = CString::new("")?;
			let is_truncated = ct.fixed_bit_length() < vt.fixed_bit_length();
			let result = if is_truncated
			{
				unsafe {
					LLVMBuildTrunc(
						llvm.builder,
						value,
						dest_type,
						tmpname.as_ptr(),
					)
				}
			}
			else if vt.is_signed()
			{
				unsafe {
					LLVMBuildSExtOrBitCast(
						llvm.builder,
						value,
						dest_type,
						tmpname.as_ptr(),
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
						tmpname.as_ptr(),
					)
				}
			};
			Ok(result)
		}
		(ValueType::Bool, ct) if ct.is_integral() =>
		{
			let dest_type = coerced_type.generate(llvm)?;
			let tmpname = CString::new("")?;
			let result = unsafe {
				LLVMBuildZExtOrBitCast(
					llvm.builder,
					value,
					dest_type,
					tmpname.as_ptr(),
				)
			};
			Ok(result)
		}
		(vt, ValueType::Bool) if vt.is_integral() =>
		{
			let dest_type = coerced_type.generate(llvm)?;
			let tmpname = CString::new("")?;
			let result = unsafe {
				LLVMBuildTruncOrBitCast(
					llvm.builder,
					value,
					dest_type,
					tmpname.as_ptr(),
				)
			};
			Ok(result)
		}
		(_, _) => unimplemented!(),
	}
}

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
