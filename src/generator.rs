/**/

use crate::common::*;

use std::ffi::{CStr, CString};

use llvm_sys::analysis::*;
use llvm_sys::core::*;
use llvm_sys::prelude::*;
use llvm_sys::*;
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
				global_variables: std::collections::HashMap::new(),
				global_functions: std::collections::HashMap::new(),
				local_parameters: std::collections::HashMap::new(),
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
		unsafe {
			let inttype = LLVMInt64TypeInContext(self.context);
			LLVMConstInt(inttype, value as u64, 0)
		}
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
					.map(|parameter| match &parameter.value_type
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

			llvm.global_functions.insert(name.resolution_id, function);

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
					None =>
					{
						return Err(anyhow!("failed to find signature")
							.context(name.location.format())
							.context(format!(
								"failed to generate signature of function '{}'",
								name.name
							)))
					}
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
				name: _,
				parameters: _,
				return_type: _,
				flags: _,
			} => Ok(()),
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
				value_type: Some(vt),
				location: _,
			} =>
			{
				let cname = CString::new(&name.name as &str)?;
				let vartype = vt.generate(llvm)?;
				let loc = unsafe {
					LLVMBuildAlloca(llvm.builder, vartype, cname.as_ptr())
				};
				llvm.local_variables.insert(name.resolution_id, loc);
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
				reference,
				value,
				location: _,
			} =>
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
					None =>
					{
						return Err(anyhow!("undefined reference")
							.context(name.location.format())
							.context(format!(
								"undefined reference to method '{}'",
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
				let labeled_block = find_or_append_labeled_block(llvm, &label)?;
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
				let labeled_block = find_or_append_labeled_block(llvm, &label)?;
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
		let left = self.left.generate(llvm)?;
		let right = self.right.generate(llvm)?;
		let name = CString::new("")?;
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
				};
				Ok(result)
			}
			Expression::PrimitiveLiteral(literal) => literal.generate(llvm),
			Expression::NakedIntegerLiteral {
				value,
				value_type: Some(value_type),
				location: _,
			} =>
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
			Expression::NakedIntegerLiteral {
				value: _,
				value_type: None,
				location,
			} => Err(anyhow!("failed to infer type")
				.context(location.format())
				.context(format!("failed to infer integer literal type"))),
			Expression::BitIntegerLiteral {
				value,
				value_type: Some(value_type),
				location: _,
			} =>
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
			Expression::BitIntegerLiteral {
				value: _,
				value_type: None,
				location,
			} => Err(anyhow!("failed to infer type")
				.context(location.format())
				.context(format!("failed to infer integer literal type"))),
			Expression::ArrayLiteral {
				array: Array { elements, .. },
				element_type: Some(element_type),
			} =>
			{
				let element_type: LLVMTypeRef = element_type.generate(llvm)?;
				let mut values = Vec::with_capacity(elements.len());
				for element in elements
				{
					let value: LLVMValueRef = element.generate(llvm)?;
					values.push(value);
				}
				let result = unsafe {
					LLVMConstArray(
						element_type,
						values.as_mut_ptr(),
						values.len() as u32,
					)
				};
				Ok(result)
			}
			Expression::ArrayLiteral {
				element_type: None,
				array: Array { location, .. },
			} => Err(anyhow!("failed to infer type")
				.context(location.format())
				.context(format!(
					"failed to infer array literal element type"
				))),
			Expression::StringLiteral {
				bytes: _,
				value_type: Some(ValueType::String),
				location: _,
			} => unimplemented!(),
			Expression::StringLiteral {
				bytes,
				value_type: Some(ValueType::Slice { element_type }),
				location: _,
			} =>
			{
				let element_type: LLVMTypeRef = element_type.generate(llvm)?;
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
			Expression::StringLiteral {
				bytes: _,
				value_type: _,
				location,
			} => Err(anyhow!("failed to infer type")
				.context(location.format())
				.context(format!("failed to infer string literal type"))),
			Expression::Deref {
				reference,
				deref_type: None,
			} => Err(anyhow!("failed to infer type")
				.context(reference.location.format())
				.context(format!("failed to infer type of reference"))),
			Expression::Deref {
				reference,
				deref_type: Some(deref_type),
			} => reference.generate_deref(deref_type, llvm),
			Expression::Autocoerce {
				expression,
				coerced_type,
			} => generate_autocoerce(&expression, coerced_type, llvm),
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
			PrimitiveLiteral::Usize(value) =>
			unsafe {
				let inttype = LLVMInt64TypeInContext(llvm.context);
				Ok(LLVMConstInt(inttype, *value as u64, 0))
			},
			PrimitiveLiteral::Bool(value) => Ok(llvm.const_u8(*value as u8)),
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
			ValueType::Usize =>
			unsafe { LLVMInt64TypeInContext(llvm.context) },
			ValueType::Bool =>
			unsafe { LLVMInt8TypeInContext(llvm.context) },
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
			ValueType::ExtArray { element_type } =>
			{
				let element_type = element_type.generate(llvm)?;
				element_type
			}
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
			if self.steps.is_empty()
			{
				return Ok(*param);
			}
		}

		let address = self.generate_storage_address(llvm)?;
		let result = if self.address_depth > 0
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
				return Err(anyhow!("is parameter")
					.context(self.location.format())
					.context(format!(
						"cannot take address of parameter '{}'",
						self.base.name
					)));
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
			return Err(anyhow!("undefined reference")
				.context(self.location.format())
				.context(format!(
					"undefined reference to '{}'",
					self.base.name
				)));
		};

		if self.steps.is_empty()
		{
			return Ok(base_addr);
		}

		let mut addr = base_addr;
		for step in &self.steps
		{
			match step
			{
				ReferenceStep::Element { argument } =>
				{
					let argument: LLVMValueRef = argument.generate(llvm)?;
					indices.push(argument)
				}
				ReferenceStep::Member { member: _ } =>
				{
					// TODO get offset of member in struct
					let offset = 0;
					indices.push(llvm.const_i32(offset));
				}
				ReferenceStep::Autoderef if is_immediate_parameter =>
				{
					is_immediate_parameter = false;
				}
				ReferenceStep::Autoderef =>
				{
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
				}
				ReferenceStep::Autodeslice =>
				{
					assert!(indices.is_empty());

					let tmpname = CString::new("")?;
					addr = unsafe {
						LLVMBuildExtractValue(
							llvm.builder,
							addr,
							0u32,
							tmpname.as_ptr(),
						)
					};
					indices.push(llvm.const_i32(0));
				}
			}
		}

		if indices.len() > 0
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
		let id = &self.base.resolution_id;
		let base_addr = if let Some(value) = llvm.local_parameters.get(id)
		{
			let param = *value;
			// We assume that the parameter is ValueType::Slice.
			if self.steps.is_empty()
			{
				let tmpname = CString::new("")?;
				let result = unsafe {
					LLVMBuildExtractValue(
						llvm.builder,
						param,
						1u32,
						tmpname.as_ptr(),
					)
				};
				return Ok(result);
			}
			else
			{
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
		}
		else if let Some(loc) = llvm.local_variables.get(id)
		{
			*loc
		}
		else if let Some(loc) = llvm.global_variables.get(id)
		{
			*loc
		}
		else
		{
			return Err(anyhow!("undefined reference")
				.context(self.location.format())
				.context(format!(
					"undefined reference to '{}'",
					self.base.name
				)));
		};

		let array_type = unsafe { LLVMGetAllocatedType(base_addr) };
		let length: u32 = unsafe { LLVMGetArrayLength(array_type) };
		let result = llvm.const_usize(length as usize);
		Ok(result)
	}

	fn generate_ext_array_view(
		&self,
		llvm: &mut Generator,
		element_type: Box<ValueType>,
	) -> Result<LLVMValueRef, anyhow::Error>
	{
		let tmpname = CString::new("")?;
		let element_type = element_type.generate(llvm)?;
		let pointertype = unsafe { LLVMPointerType(element_type, 0u32) };
		let address = self.generate_storage_address(llvm)?;
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
		&self,
		llvm: &mut Generator,
		element_type: Box<ValueType>,
		length: usize,
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
		let address = self.generate_storage_address(llvm)?;
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
}

fn generate_autocoerce(
	expression: &Expression,
	coerced_type: &ValueType,
	llvm: &mut Generator,
) -> Result<LLVMValueRef, anyhow::Error>
{
	match coerced_type
	{
		ValueType::Slice { element_type: _ } => match expression
		{
			Expression::Deref {
				reference,
				deref_type,
			} => match deref_type
			{
				Some(ValueType::Array {
					element_type,
					length,
				}) => reference.generate_array_slice(
					llvm,
					element_type.clone(),
					*length,
				),
				Some(_) => unimplemented!(),
				None => Err(anyhow!("failed to infer type")
					.context(reference.location.format())
					.context(format!(
						"failed to infer type for '{}'",
						reference.base.name
					))),
			},
			_ => unimplemented!(),
		},
		ValueType::View { deref_type } => match deref_type.as_ref()
		{
			ValueType::ExtArray { element_type } => match expression
			{
				Expression::Deref {
					reference,
					deref_type: _,
				} =>
				{
					let element_type = element_type.clone();
					reference.generate_ext_array_view(llvm, element_type)
				}
				Expression::StringLiteral { .. } =>
				{
					let tmpname = CString::new("")?;
					let element_type = element_type.generate(llvm)?;
					let pointertype =
						unsafe { LLVMPointerType(element_type, 0u32) };
					let address = expression.generate(llvm)?;
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
				_ => unimplemented!(),
			},
			_ => unimplemented!(),
		},
		_ => unimplemented!(),
	}
}
