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
	global_functions: std::collections::HashMap<String, LLVMValueRef>,
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

	fn const_i64(&mut self, value: i32) -> LLVMValueRef
	{
		unsafe {
			let inttype = LLVMInt64TypeInContext(self.context);
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
		Declaration::Function {
			name,
			parameters,
			body: _,
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

			llvm.global_functions
				.insert(name.name.to_string(), function);

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
			Declaration::Function {
				name,
				parameters,
				body,
				return_type: _,
			} =>
			{
				let entry_block_name = CString::new("entry")?;

				let function = match llvm.global_functions.get(&name.name)
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
			Expression::StringLiteral(_literal) => unimplemented!(),
			Expression::Deref {
				reference,
				value_type: _,
			} => reference.generate_deref(llvm),
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
						.map(|argument| match argument
						{
							Expression::Deref {
								reference,
								value_type:
									Some(ValueType::Array {
										element_type,
										length,
									}),
							} => reference.generate_array_slice(
								llvm,
								element_type.clone(),
								*length,
							),
							_ => argument.generate(llvm),
						})
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
				let mut member_types = [sizetype, pointertype];
				unsafe {
					LLVMStructTypeInContext(
						llvm.context,
						member_types.as_mut_ptr(),
						member_types.len() as u32,
						0,
					)
				}
			}
		};
		Ok(typeref)
	}
}

impl Reference
{
	fn generate_storage_address(
		&self,
		llvm: &mut Generator,
	) -> Result<LLVMValueRef, anyhow::Error>
	{
		match &self
		{
			Reference::Identifier(name) =>
			{
				let loc = match llvm.local_variables.get(&name.resolution_id)
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
				Ok(loc)
			}
			Reference::ArrayElement { name, argument } =>
			{
				let tmpname = CString::new("")?;
				let mut indices = Vec::new();
				indices.push(llvm.const_i64(0));
				let loc = match llvm.local_variables.get(&name.resolution_id)
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
				let argument: LLVMValueRef = argument.generate(llvm)?;
				indices.push(argument);
				let address = unsafe {
					LLVMBuildGEP(
						llvm.builder,
						loc,
						indices.as_mut_ptr(),
						indices.len() as u32,
						tmpname.as_ptr(),
					)
				};
				Ok(address)
			}
		}
	}

	fn generate_deref(
		&self,
		llvm: &mut Generator,
	) -> Result<LLVMValueRef, anyhow::Error>
	{
		match &self
		{
			Reference::Identifier(name) =>
			{
				if let Some(value) =
					llvm.local_parameters.get(&name.resolution_id)
				{
					return Ok(*value);
				}
				else if let Some(loc) =
					llvm.local_variables.get(&name.resolution_id)
				{
					let loc = *loc;
					let tmpname = CString::new("")?;
					let result = unsafe {
						LLVMBuildLoad(llvm.builder, loc, tmpname.as_ptr())
					};
					Ok(result)
				}
				else
				{
					Err(anyhow!("undefined reference")
						.context(name.location.format())
						.context(format!(
							"undefined reference to '{}'",
							name.name
						)))
				}
			}
			Reference::ArrayElement { name, argument } =>
			{
				let tmpname = CString::new("")?;
				let loc = if let Some(value) =
					llvm.local_parameters.get(&name.resolution_id)
				{
					let loc = *value;
					let array_loc = unsafe {
						LLVMBuildExtractValue(
							llvm.builder,
							loc,
							1u32,
							tmpname.as_ptr(),
						)
					};
					array_loc
				}
				else if let Some(loc) =
					llvm.local_variables.get(&name.resolution_id)
				{
					*loc
				}
				else
				{
					return Err(anyhow!("undefined reference")
						.context(name.location.format())
						.context(format!(
							"undefined reference to '{}'",
							name.name
						)));
				};
				let mut indices = Vec::new();
				indices.push(llvm.const_i64(0));
				let argument: LLVMValueRef = argument.generate(llvm)?;
				indices.push(argument);
				let address = unsafe {
					LLVMBuildGEP(
						llvm.builder,
						loc,
						indices.as_mut_ptr(),
						indices.len() as u32,
						tmpname.as_ptr(),
					)
				};
				let tmpname = CString::new("")?;
				let result = unsafe {
					LLVMBuildLoad(llvm.builder, address, tmpname.as_ptr())
				};
				Ok(result)
			}
		}
	}

	fn generate_array_len(
		&self,
		llvm: &mut Generator,
	) -> Result<LLVMValueRef, anyhow::Error>
	{
		let loc = match &self
		{
			Reference::Identifier(name) =>
			{
				if let Some(value) =
					llvm.local_parameters.get(&name.resolution_id)
				{
					let loc = *value;
					let tmpname = CString::new("")?;
					let result = unsafe {
						LLVMBuildExtractValue(
							llvm.builder,
							loc,
							0u32,
							tmpname.as_ptr(),
						)
					};
					return Ok(result);
				}
				else if let Some(loc) =
					llvm.local_variables.get(&name.resolution_id)
				{
					*loc
				}
				else
				{
					return Err(anyhow!("undefined reference")
						.context(name.location.format())
						.context(format!(
							"undefined reference to '{}'",
							name.name
						)));
				}
			}
			Reference::ArrayElement { name, argument: _ } =>
			{
				if let Some(value) =
					llvm.local_parameters.get(&name.resolution_id)
				{
					let loc = *value;
					let tmpname = CString::new("")?;
					let array_loc = unsafe {
						LLVMBuildExtractValue(
							llvm.builder,
							loc,
							1u32,
							tmpname.as_ptr(),
						)
					};
					array_loc
				}
				else if let Some(loc) =
					llvm.local_variables.get(&name.resolution_id)
				{
					*loc
				}
				else
				{
					return Err(anyhow!("undefined reference")
						.context(name.location.format())
						.context(format!(
							"undefined reference to '{}'",
							name.name
						)));
				}
			}
		};

		let array_type = unsafe { LLVMGetAllocatedType(loc) };

		let length: u32 = unsafe { LLVMGetArrayLength(array_type) };
		let result = llvm.const_usize(length as usize);
		Ok(result)
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
		let mut member_types = [sizetype, pointertype];
		let slice_type = unsafe {
			LLVMStructTypeInContext(
				llvm.context,
				member_types.as_mut_ptr(),
				member_types.len() as u32,
				0,
			)
		};
		let mut slice = unsafe { LLVMGetUndef(slice_type) };
		let length_value = llvm.const_usize(length);
		slice = unsafe {
			LLVMBuildInsertValue(
				llvm.builder,
				slice,
				length_value,
				0u32,
				tmpname.as_ptr(),
			)
		};
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
				1u32,
				tmpname.as_ptr(),
			)
		};
		Ok(slice)
	}
}
