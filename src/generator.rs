/**/

use crate::common::*;

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
	global_functions: std::collections::HashMap<String, LLVMValueRef>,
	local_variables: std::collections::HashMap<String, LLVMValueRef>,
	local_labeled_blocks: std::collections::HashMap<String, LLVMBasicBlockRef>,
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
				global_functions: std::collections::HashMap::new(),
				local_variables: std::collections::HashMap::new(),
				local_labeled_blocks: std::collections::HashMap::new(),
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
				parameters,
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

				let function_name = CString::new(name.name.as_bytes())?;

				let param_types: Result<Vec<LLVMTypeRef>, anyhow::Error> =
					parameters
						.iter()
						.map(|parameter| match parameter.value_type
						{
							Some(vt) => vt.generate(llvm),
							None => Err(anyhow!("failed to infer type")
								.context(parameter.name.location.format())
								.context(format!(
									"failed to infer type for '{}'",
									parameter.name.name
								))),
						})
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

				let entry_block_name = CString::new("entry")?;

				unsafe {
					let entry_block = LLVMAppendBasicBlockInContext(
						llvm.context,
						function,
						entry_block_name.as_ptr(),
					);
					LLVMPositionBuilderAtEnd(llvm.builder, entry_block);
				};

				for (i, (parameter, vartype)) in
					parameters.iter().zip(param_types.iter()).enumerate()
				{
					let param = unsafe { LLVMGetParam(function, i as u32) };
					let loc = if parameter.is_mutable
					{
						let cname = CString::new(&parameter.name.name as &str)?;
						let vt: LLVMTypeRef = *vartype;
						let loc = unsafe {
							LLVMBuildAlloca(llvm.builder, vt, cname.as_ptr())
						};
						unsafe {
							LLVMBuildStore(llvm.builder, param, loc);
						}
						loc
					}
					else
					{
						param
					};
					llvm.local_variables
						.insert(parameter.name.name.to_string(), loc);
				}

				body.generate(llvm)?;
				llvm.local_variables.clear();
				llvm.local_labeled_blocks.clear();

				llvm.verify_function(function);

				llvm.global_functions
					.insert(name.name.to_string(), function);

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
				value_type: Some(vt),
				location: _,
			} =>
			{
				let cname = CString::new(&name.name as &str)?;
				let vartype = vt.generate(llvm)?;
				let loc = unsafe {
					LLVMBuildAlloca(llvm.builder, vartype, cname.as_ptr())
				};
				llvm.local_variables.insert(name.name.to_string(), loc);
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
				location: _,
			} =>
			{
				let cname = CString::new(&name.name as &str)?;
				let vartype = vt.generate(llvm)?;
				let loc = unsafe {
					LLVMBuildAlloca(llvm.builder, vartype, cname.as_ptr())
				};
				llvm.local_variables.insert(name.name.to_string(), loc);
				Ok(())
			}
			Statement::Declaration {
				name,
				value: _,
				value_type: None,
				location,
			} => Err(anyhow!("failed to infer type")
				.context(location.format())
				.context(format!("failed to infer type for '{}'", name.name))),
			Statement::Assignment {
				name,
				value,
				location,
			} =>
			{
				let loc = match llvm.local_variables.get(&name.name)
				{
					Some(loc) => *loc,
					None =>
					{
						return Err(anyhow!("undefined reference")
							.context(location.format())
							.context(format!(
								"undefined reference to '{}'",
								name.name
							)))
					}
				};
				let value = value.generate(llvm)?;
				unsafe {
					LLVMBuildStore(llvm.builder, value, loc);
				}
				Ok(())
			}
			Statement::Loop { location } => Err(anyhow!("misplaced loop")
				.context(location.format())
				.context("misplaced loop")),
			Statement::Goto { label, location: _ } =>
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
				let labeled_block =
					find_or_append_labeled_block(llvm, &label.name)?;
				unsafe {
					LLVMPositionBuilderAtEnd(llvm.builder, current_block);
					LLVMBuildBr(llvm.builder, labeled_block);
					LLVMPositionBuilderAtEnd(llvm.builder, unreachable_block);
				}
				Ok(())
			}
			Statement::Label { label, location: _ } =>
			{
				let current_block = unsafe { LLVMGetInsertBlock(llvm.builder) };
				let labeled_block =
					find_or_append_labeled_block(llvm, &label.name)?;
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
				location: _,
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
	label: &str,
) -> Result<LLVMBasicBlockRef, anyhow::Error>
{
	if let Some(block) = llvm.local_labeled_blocks.get(label)
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
		llvm.local_labeled_blocks.insert(label.to_string(), block);
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
		let left = self.left.generate(llvm)?;
		let right = self.right.generate(llvm)?;
		let name = CString::new("tmp")?;
		let pred = match self.op
		{
			ComparisonOp::Equals => llvm_sys::LLVMIntPredicate::LLVMIntEQ,
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
				location: _,
			} =>
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
			Expression::PrimitiveLiteral(literal) => literal.generate(llvm),
			Expression::ArrayLiteral { .. } => unimplemented!(),
			Expression::StringLiteral(_literal) => unimplemented!(),
			Expression::Variable {
				name,
				value_type: _,
			} =>
			{
				let tmpname = CString::new("tmp")?;
				let loc = match llvm.local_variables.get(&name.name)
				{
					Some(loc) => *loc,
					None =>
					{
						return Err(anyhow!("undefined reference")
							.context(name.location.format())
							.context(format!(
								"undefined reference to '{}'",
								name.name
							)))
					}
				};
				let result = unsafe {
					LLVMBuildLoad(llvm.builder, loc, tmpname.as_ptr())
				};
				Ok(result)
			}
			Expression::FunctionCall {
				name,
				arguments,
				return_type: _,
			} =>
			{
				let function = match llvm.global_functions.get(&name.name)
				{
					Some(function) => *function,
					None =>
					{
						return Err(anyhow!("undefined reference")
							.context(name.location.format())
							.context(format!(
								"undefined reference to function '{}'",
								name.name
							)))
					}
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
			PrimitiveLiteral::Int32(value) =>
			{
				let result = unsafe {
					let inttype = LLVMInt32TypeInContext(llvm.context);
					LLVMConstInt(inttype, *value as u64, 1)
				};
				Ok(result)
			}
			PrimitiveLiteral::Bool(value) =>
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
