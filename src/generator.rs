/**/

use crate::analyzer::ValueType;
use crate::analyzer::{BinaryOp, ComparisonOp};
use crate::analyzer::{Block, Declaration, FunctionBody, Statement};
use crate::analyzer::{Comparison, Expression, Literal};

use std::ffi::{CStr, CString};

use llvm_sys::analysis::*;
use llvm_sys::core::*;
use llvm_sys::prelude::*;
use llvm_sys::{LLVMBuilder, LLVMContext, LLVMModule};

use anyhow::anyhow;

pub fn generate(
	program: &Vec<Declaration>,
	source_filename: &str,
) -> Result<String, anyhow::Error>
{
	let mut generator = Generator::new(source_filename)?;

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
	local_variables: std::collections::HashMap<String, LLVMValueRef>,
	local_blocks: std::collections::HashMap<String, LLVMBasicBlockRef>,
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
			let builder = LLVMCreateBuilderInContext(context);
			Generator {
				module,
				context,
				builder,
				local_variables: std::collections::HashMap::new(),
				local_blocks: std::collections::HashMap::new(),
			}
		};
		Ok(generator)
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
			Declaration::Function {
				name,
				body,
				return_type,
			} =>
			{
				let return_type = match return_type
				{
					Some(return_type) => return_type.generate(llvm)?,

					None =>
					unsafe { LLVMVoidTypeInContext(llvm.context) },
				};

				let function_name = CString::new(name.as_bytes())?;

				let function = unsafe {
					let mut param_types: Vec<LLVMTypeRef> = vec![];
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

				let entry_block_name = CString::new("entry")?;

				unsafe {
					let entry_block = LLVMAppendBasicBlockInContext(
						llvm.context,
						function,
						entry_block_name.as_ptr(),
					);
					LLVMPositionBuilderAtEnd(llvm.builder, entry_block);
				};

				body.generate(llvm)?;

				llvm.verify_function(function);

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
		for statement in &self.statements
		{
			statement.generate(llvm)?;
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
				value_type: Some(vt),
			} =>
			{
				let cname = CString::new(name as &str)?;
				let vtype = vt.generate(llvm)?;
				let loc = unsafe {
					LLVMBuildAlloca(llvm.builder, vtype, cname.as_ptr())
				};
				llvm.local_variables.insert(name.to_string(), loc);
				let value = value.generate(llvm)?;
				unsafe {
					LLVMBuildStore(llvm.builder, value, loc);
				}
				Ok(())
			}
			Statement::Declaration {
				name,
				value: None,
				value_type: Some(vt),
			} =>
			{
				let cname = CString::new(name as &str)?;
				let vtype = vt.generate(llvm)?;
				let loc = unsafe {
					LLVMBuildAlloca(llvm.builder, vtype, cname.as_ptr())
				};
				llvm.local_variables.insert(name.to_string(), loc);
				Ok(())
			}
			Statement::Declaration {
				name,
				value,
				value_type: None,
			} => Err(anyhow!(
				"failed to infer type for '{}' = {:?}",
				name,
				value
			)),
			Statement::Assignment { name, value } =>
			{
				let loc = match llvm.local_variables.get(name)
				{
					Some(loc) => *loc,
					None =>
					{
						return Err(anyhow!(
							"undefined reference to '{}'",
							name
						))
					}
				};
				let value = value.generate(llvm)?;
				unsafe {
					LLVMBuildStore(llvm.builder, value, loc);
				}
				Ok(())
			}
			Statement::Loop => unimplemented!(),
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
				else_branch: Some(else_branch),
			} => unimplemented!(),
			Statement::If {
				condition,
				then_branch,
				else_branch: None,
			} => unimplemented!(),
			Statement::Block(block) => block.generate(llvm),
		}
	}
}

fn find_or_append_labeled_block(
	llvm: &mut Generator,
	label: &str,
) -> Result<LLVMBasicBlockRef, anyhow::Error>
{
	if let Some(block) = llvm.local_blocks.get(label)
	{
		Ok(*block)
	}
	else
	{
		let cname = CString::new(label)?;
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
		llvm.local_blocks.insert(label.to_string(), block);
		Ok(block)
	}
}

impl Generatable for Comparison
{
	type Item = ();

	fn generate(
		&self,
		llvm: &mut Generator,
	) -> Result<Self::Item, anyhow::Error>
	{
		match self.op
		{
			ComparisonOp::Equals => unimplemented!(),
		}
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
			Expression::Binary { op, left, right } =>
			{
				let left = left.generate(llvm)?;
				let right = right.generate(llvm)?;
				let name = CString::new("tmp")?;
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
				};
				Ok(result)
			}
			Expression::Literal(literal) => literal.generate(llvm),
			Expression::Variable {
				name,
				value_type: _,
			} =>
			{
				let tmpname = CString::new("tmp")?;
				let loc = match llvm.local_variables.get(name)
				{
					Some(loc) => *loc,
					None =>
					{
						return Err(anyhow!(
							"undefined reference to '{}'",
							name
						))
					}
				};
				let result = unsafe {
					LLVMBuildLoad(llvm.builder, loc, tmpname.as_ptr())
				};
				Ok(result)
			}
		}
	}
}

impl Generatable for Literal
{
	type Item = LLVMValueRef;

	fn generate(
		&self,
		llvm: &mut Generator,
	) -> Result<Self::Item, anyhow::Error>
	{
		match self
		{
			Literal::Int32(value) =>
			{
				let result = unsafe {
					let inttype = LLVMInt32TypeInContext(llvm.context);
					LLVMConstInt(inttype, *value as u64, 1)
				};
				Ok(result)
			}
			Literal::Bool(value) =>
			{
				let result = unsafe {
					let bytetype = LLVMInt8TypeInContext(llvm.context);
					LLVMConstInt(bytetype, *value as u64, 0)
				};
				Ok(result)
			}
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
			ValueType::Int32 =>
			unsafe { LLVMInt32TypeInContext(llvm.context) },
			ValueType::Bool =>
			unsafe { LLVMInt8TypeInContext(llvm.context) },
		};
		Ok(typeref)
	}
}