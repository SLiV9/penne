/**/

use crate::analyzer::ValueType;
use crate::analyzer::{BinaryOp, ComparisonOp};
use crate::analyzer::{Block, Declaration, Statement};
use crate::analyzer::{Comparison, Expression, Literal};

use std::ffi::{CStr, CString};

use llvm_sys::analysis::*;
use llvm_sys::core::*;
use llvm_sys::execution_engine::*;
use llvm_sys::prelude::*;
use llvm_sys::{LLVMBuilder, LLVMContext, LLVMModule};

use anyhow::anyhow;

pub fn generate(
	program: &Vec<Declaration>,
	source_filename: &str,
) -> Result<Module, anyhow::Error>
{
	// TODO replace with module name based on filename or something like that
	let mut generator = Generator::new(source_filename)?;

	for declaration in program
	{
		declaration.generate(&mut generator)?;
	}

	generator.verify();

	let module = Module(generator);
	Ok(module)
}

pub struct Module(Generator);

impl Module
{
	pub fn generate_ir(&self) -> Result<String, anyhow::Error>
	{
		self.0.generate_ir()
	}

	pub fn execute(self) -> Result<Option<i32>, anyhow::Error>
	{
		unsafe {
			LLVMLinkInMCJIT();
			if llvm_sys::target::LLVM_InitializeNativeTarget() != 0
			{
				return Err(anyhow!("failed to initialize native target"));
			}
		}

		// Extract the module from the generator because the execution engine
		// will dispose of it, for some reason.
		let module: *mut LLVMModule = std::ptr::null_mut();
		unsafe {
			std::ptr::swap(self.0.module, module);
		}

		let engine = unsafe {
			let mut engine: LLVMExecutionEngineRef = std::ptr::null_mut();
			let mut error: *mut i8 = std::ptr::null_mut();
			let status = LLVMCreateExecutionEngineForModule(
				&mut engine as *mut LLVMExecutionEngineRef,
				module,
				&mut error as *mut *mut i8,
			);
			if status != 0
			{
				if !error.is_null()
				{
					let errorvalue = anyhow!(
						"failed to create execution engine: {}",
						CStr::from_ptr(error).to_string_lossy()
					);
					LLVMDisposeMessage(error);
					return Err(errorvalue);
				}
				else
				{
					return Err(anyhow!("failed to create execution engine"));
				}
			}
			engine
		};

		let main_name = CString::new("main")?;
		let result = unsafe {
			let mut main: LLVMValueRef = std::ptr::null_mut();
			let status = LLVMFindFunction(
				engine,
				main_name.as_ptr(),
				&mut main as *mut LLVMValueRef,
			);
			if status != 0
			{
				return Err(anyhow!("failed to find main"));
			}
			let mut args = vec![];
			let result = LLVMRunFunction(engine, main, 0, args.as_mut_ptr());
			if self.0.returns_int
			{
				let x = LLVMGenericValueToInt(result, 1) as i32;
				Some(x)
			}
			else
			{
				None
			}
		};

		unsafe {
			LLVMDisposeExecutionEngine(engine);
		}

		Ok(result)
	}
}

struct Generator
{
	module: *mut LLVMModule,
	context: *mut LLVMContext,
	builder: *mut LLVMBuilder,
	returns_int: bool,
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
				returns_int: false,
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
			if !self.builder.is_null()
			{
				LLVMDisposeBuilder(self.builder);
			}
			if !self.module.is_null()
			{
				LLVMDisposeModule(self.module);
			}
			if !self.context.is_null()
			{
				LLVMContextDispose(self.context);
			}
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
					Some(return_type) =>
					{
						if return_type.is_integral()
						{
							llvm.returns_int = true;
						}

						return_type.generate(llvm)?
					}

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

		if self.value == Expression::Void
		{
			unsafe {
				LLVMBuildRetVoid(llvm.builder);
			};
		}
		else
		{
			let result: LLVMValueRef = self.value.generate(llvm)?;
			unsafe {
				LLVMBuildRet(llvm.builder, result);
			};
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
				value_type: _,
			} => unimplemented!(),
			Statement::Declaration {
				name,
				value: None,
				value_type: _,
			} => unimplemented!(),
			Statement::Assignment { name, value } => unimplemented!(),
			Statement::Loop => unimplemented!(),
			Statement::Goto { label } => unimplemented!(),
			Statement::Label { label } =>
			{
				// TODO
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
			Expression::Binary { op, left, right } => match op
			{
				BinaryOp::Add => unimplemented!(),
				BinaryOp::Subtract => unimplemented!(),
			},
			Expression::Literal(literal) => literal.generate(llvm),
			Expression::Variable { name, value_type } => unimplemented!(),
			Expression::Void => unimplemented!(),
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
