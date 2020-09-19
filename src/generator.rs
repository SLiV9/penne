/**/

use crate::parser::{BinaryOp, Comparison, ComparisonOp, Expression, Literal};
use crate::parser::{Block, Declaration, Statement};

use std::ffi::{CStr, CString};

use llvm_sys::analysis::*;
use llvm_sys::core::*;
use llvm_sys::prelude::*;
use llvm_sys::{LLVMBuilder, LLVMContext, LLVMModule};

pub fn generate(
	program: &Vec<Declaration>,
	source_filename: &str,
) -> Result<String, anyhow::Error>
{
	// TODO replace with module name based on filename or something like that
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
			Declaration::Function { name, body } =>
			{
				let function_name = CString::new(name.as_bytes())?;

				let function = unsafe {
					let void = LLVMVoidTypeInContext(llvm.context);
					let mut param_types: Vec<LLVMTypeRef> = vec![];
					let function_type = LLVMFunctionType(
						void,
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
			} => unimplemented!(),
			Statement::Declaration { name, value: None } => unimplemented!(),
			Statement::Assignment { name, value } => unimplemented!(),
			Statement::Loop => unimplemented!(),
			Statement::Goto { label } => unimplemented!(),
			Statement::Label { label } => unimplemented!(),
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
			Expression::Variable(var) => unimplemented!(),
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
