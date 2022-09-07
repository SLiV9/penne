/**/

use crate::common::*;

use anyhow::anyhow;
use anyhow::Context;

pub fn analyze(
	program: Vec<Declaration>,
) -> Result<Vec<Declaration>, anyhow::Error>
{
	let mut analyzer = Analyzer {
		variable_stack: Vec::new(),
		function_list: Vec::new(),
		resolution_id: 1,
	};
	for declaration in &program
	{
		let _unused: Identifier = declare(declaration, &mut analyzer)?;
	}
	program.iter().map(|x| x.analyze(&mut analyzer)).collect()
}

struct Analyzer
{
	variable_stack: Vec<Vec<Identifier>>,
	function_list: Vec<Identifier>,
	resolution_id: u32,
}

impl Analyzer
{
	fn declare_variable(
		&mut self,
		identifier: &Identifier,
	) -> Result<Identifier, anyhow::Error>
	{
		for scope in &self.variable_stack
		{
			if let Some(previous_identifier) =
				scope.iter().find(|x| x.name == identifier.name)
			{
				return Err(anyhow!(
					"previous declaration {}",
					previous_identifier.location.format()
				)
				.context(identifier.location.format())
				.context(format!(
					"a variable named '{}' is already defined in this scope",
					identifier.name
				)));
			}
		}

		let identifier = Identifier {
			name: identifier.name.clone(),
			location: identifier.location.clone(),
			resolution_id: self.resolution_id,
		};
		self.resolution_id += 1;

		if let Some(scope) = self.variable_stack.last_mut()
		{
			scope.push(identifier.clone());
		}
		else
		{
			self.variable_stack.push(vec![identifier.clone()]);
		}
		Ok(identifier)
	}

	fn use_variable(
		&self,
		identifier: &Identifier,
	) -> Result<Identifier, anyhow::Error>
	{
		for scope in &self.variable_stack
		{
			if let Some(previous_identifier) =
				scope.iter().find(|x| x.name == identifier.name)
			{
				return Ok(Identifier {
					resolution_id: previous_identifier.resolution_id,
					..identifier.clone()
				});
			}
		}

		Err(anyhow!("undefined reference")
			.context(identifier.location.format())
			.context(format!(
				"reference to undefined variable named '{}'",
				identifier.name
			)))
	}

	fn declare_function(
		&mut self,
		identifier: &Identifier,
	) -> Result<Identifier, anyhow::Error>
	{
		if let Some(previous_identifier) = self
			.function_list
			.iter()
			.find(|x| x.name == identifier.name)
		{
			return Err(anyhow!(
				"previous declaration {}",
				previous_identifier.location.format()
			)
			.context(identifier.location.format())
			.context(format!(
				"a function named '{}' is already defined",
				identifier.name
			)));
		}

		let identifier = Identifier {
			name: identifier.name.clone(),
			location: identifier.location.clone(),
			resolution_id: self.resolution_id,
		};
		self.resolution_id += 1;

		self.function_list.push(identifier.clone());
		Ok(identifier)
	}

	fn use_function(
		&self,
		identifier: &Identifier,
	) -> Result<Identifier, anyhow::Error>
	{
		if let Some(declaration_identifier) = self
			.function_list
			.iter()
			.find(|x| x.name == identifier.name)
		{
			Ok(Identifier {
				resolution_id: declaration_identifier.resolution_id,
				..identifier.clone()
			})
		}
		else
		{
			Err(anyhow!("undefined reference")
				.context(identifier.location.format())
				.context(format!(
					"reference to undefined function named '{}'",
					identifier.name
				)))
		}
	}

	fn create_anonymous_resolution_id(&mut self) -> u32
	{
		let id = self.resolution_id;
		self.resolution_id += 1;
		id
	}

	fn push_scope(&mut self)
	{
		self.variable_stack.push(Vec::new());
	}

	fn pop_scope(&mut self)
	{
		self.variable_stack.pop();
	}
}

fn declare(
	declaration: &Declaration,
	analyzer: &mut Analyzer,
) -> Result<Identifier, anyhow::Error>
{
	match declaration
	{
		Declaration::Constant {
			name,
			value: _,
			value_type: _,
			flags: _,
		} =>
		{
			// Constants have to be declared from top to bottom, just to avoid
			// having to deal with cyclical definitions.
			Ok(name.clone())
		}
		Declaration::Function {
			name,
			parameters: _,
			body: _,
			return_type: _,
			flags: _,
		} => analyzer.declare_function(name),
		Declaration::FunctionHead {
			name,
			parameters: _,
			return_type: _,
			flags: _,
		} => analyzer.declare_function(name),
	}
}

trait Analyzable
{
	type Item;

	fn analyze(
		&self,
		analyzer: &mut Analyzer,
	) -> Result<Self::Item, anyhow::Error>;
}

impl Analyzable for Declaration
{
	type Item = Declaration;

	fn analyze(
		&self,
		analyzer: &mut Analyzer,
	) -> Result<Self::Item, anyhow::Error>
	{
		match self
		{
			Declaration::Constant {
				name,
				value,
				value_type,
				flags,
			} =>
			{
				let value = value.analyze(analyzer)?;
				// Declare the variable after analyzing its definition, just
				// to disallow reflexive definitions.
				let name = analyzer.declare_variable(name)?;
				let declaration = Declaration::Constant {
					name,
					value,
					value_type: value_type.clone(),
					flags: *flags,
				};
				Ok(declaration)
			}
			Declaration::Function {
				name,
				parameters,
				body,
				return_type,
				flags,
			} =>
			{
				let name = analyzer.use_function(name)?;

				analyzer.push_scope();
				let parameters: Result<Vec<Parameter>, anyhow::Error> =
					parameters.iter().map(|x| x.analyze(analyzer)).collect();
				let parameters = parameters?;
				let body = body.analyze(analyzer)?;
				analyzer.pop_scope();

				let function = Declaration::Function {
					name,
					parameters,
					body,
					return_type: return_type.clone(),
					flags: *flags,
				};
				Ok(function)
			}
			Declaration::FunctionHead {
				name,
				parameters,
				return_type,
				flags,
			} =>
			{
				let name = analyzer.use_function(name)?;

				analyzer.push_scope();
				let parameters: Result<Vec<Parameter>, anyhow::Error> =
					parameters.iter().map(|x| x.analyze(analyzer)).collect();
				let parameters = parameters?;
				analyzer.pop_scope();

				let function = Declaration::FunctionHead {
					name,
					parameters,
					return_type: return_type.clone(),
					flags: *flags,
				};
				Ok(function)
			}
		}
	}
}

impl Analyzable for Parameter
{
	type Item = Parameter;

	fn analyze(
		&self,
		analyzer: &mut Analyzer,
	) -> Result<Self::Item, anyhow::Error>
	{
		let name = analyzer.declare_variable(&self.name)?;
		Ok(Parameter {
			name,
			value_type: self.value_type.clone(),
		})
	}
}

impl Analyzable for FunctionBody
{
	type Item = FunctionBody;

	fn analyze(
		&self,
		analyzer: &mut Analyzer,
	) -> Result<Self::Item, anyhow::Error>
	{
		analyzer.push_scope();
		let statements: Result<Vec<Statement>, anyhow::Error> = self
			.statements
			.iter()
			.map(|x| x.analyze(analyzer))
			.collect();
		let statements = statements?;
		let return_value = self
			.return_value
			.as_ref()
			.map(|v| v.analyze(analyzer))
			.transpose()?;
		analyzer.pop_scope();

		let return_value_identifier =
			analyzer.use_function(&self.return_value_identifier)?;

		Ok(FunctionBody {
			statements,
			return_value,
			return_value_identifier,
		})
	}
}

impl Analyzable for Block
{
	type Item = Block;

	fn analyze(
		&self,
		analyzer: &mut Analyzer,
	) -> Result<Self::Item, anyhow::Error>
	{
		analyzer.push_scope();
		let statements: Result<Vec<Statement>, anyhow::Error> = self
			.statements
			.iter()
			.map(|x| x.analyze(analyzer))
			.collect();
		let statements = statements?;
		analyzer.pop_scope();

		Ok(Block {
			statements,
			location: self.location.clone(),
		})
	}
}

impl Analyzable for Statement
{
	type Item = Statement;

	fn analyze(
		&self,
		analyzer: &mut Analyzer,
	) -> Result<Statement, anyhow::Error>
	{
		match self
		{
			Statement::Declaration {
				name,
				value: Some(value),
				value_type,
				location,
			} =>
			{
				let value = value
					.analyze(analyzer)
					.with_context(|| location.format())?;
				let name = analyzer.declare_variable(name)?;
				Ok(Statement::Declaration {
					name,
					value: Some(value),
					value_type: value_type.clone(),
					location: location.clone(),
				})
			}
			Statement::Declaration {
				name,
				value: None,
				value_type,
				location,
			} =>
			{
				let name = analyzer.declare_variable(name)?;
				Ok(Statement::Declaration {
					name,
					value: None,
					value_type: value_type.clone(),
					location: location.clone(),
				})
			}
			Statement::Assignment {
				reference,
				value,
				location,
			} =>
			{
				let value = value
					.analyze(analyzer)
					.with_context(|| location.format())?;
				let reference = reference
					.analyze(analyzer)
					.with_context(|| location.format())?;
				Ok(Statement::Assignment {
					reference,
					value,
					location: location.clone(),
				})
			}
			Statement::Loop { .. } => Ok(self.clone()),
			Statement::Goto { .. } => Ok(self.clone()),
			Statement::Label { .. } => Ok(self.clone()),
			Statement::If {
				condition,
				then_branch,
				else_branch,
				location,
			} =>
			{
				let condition = condition
					.analyze(analyzer)
					.with_context(|| location.format())?;
				let then_branch = {
					let branch = then_branch.analyze(analyzer)?;
					Box::new(branch)
				};
				let else_branch = match else_branch
				{
					Some(else_branch) =>
					{
						let branch = else_branch.analyze(analyzer)?;
						Some(Box::new(branch))
					}
					None => None,
				};
				Ok(Statement::If {
					condition,
					then_branch,
					else_branch,
					location: location.clone(),
				})
			}
			Statement::Block(block) =>
			{
				let block = block.analyze(analyzer)?;
				Ok(Statement::Block(block))
			}
		}
	}
}

impl Analyzable for Comparison
{
	type Item = Comparison;

	fn analyze(
		&self,
		analyzer: &mut Analyzer,
	) -> Result<Comparison, anyhow::Error>
	{
		let left = self
			.left
			.analyze(analyzer)
			.with_context(|| self.location.format())?;
		let right = self
			.right
			.analyze(analyzer)
			.with_context(|| self.location.format())?;
		Ok(Comparison {
			op: self.op,
			left,
			right,
			location: self.location.clone(),
		})
	}
}

impl Analyzable for Array
{
	type Item = Array;

	fn analyze(&self, analyzer: &mut Analyzer) -> Result<Array, anyhow::Error>
	{
		analyzer.push_scope();
		let elements: Result<Vec<Expression>, anyhow::Error> =
			self.elements.iter().map(|x| x.analyze(analyzer)).collect();
		let elements = elements?;
		analyzer.pop_scope();

		// Arrays need a resolution id to help with typing its elements.
		let resolution_id = analyzer.create_anonymous_resolution_id();

		Ok(Array {
			elements,
			location: self.location.clone(),
			resolution_id,
		})
	}
}

impl Analyzable for Expression
{
	type Item = Expression;

	fn analyze(
		&self,
		analyzer: &mut Analyzer,
	) -> Result<Expression, anyhow::Error>
	{
		match self
		{
			Expression::Binary {
				op,
				left,
				right,
				location,
			} =>
			{
				let left = left
					.analyze(analyzer)
					.with_context(|| location.format())?;
				let right = right
					.analyze(analyzer)
					.with_context(|| location.format())?;
				Ok(Expression::Binary {
					op: *op,
					left: Box::new(left),
					right: Box::new(right),
					location: location.clone(),
				})
			}
			Expression::PrimitiveLiteral(_lit) => Ok(self.clone()),
			Expression::NakedIntegerLiteral { .. } => Ok(self.clone()),
			Expression::BitIntegerLiteral { .. } => Ok(self.clone()),
			Expression::ArrayLiteral {
				array,
				element_type,
			} =>
			{
				let array = array.analyze(analyzer)?;
				Ok(Expression::ArrayLiteral {
					array,
					element_type: element_type.clone(),
				})
			}
			Expression::StringLiteral(_lit) => Ok(self.clone()),
			Expression::Deref {
				reference,
				ref_type,
				deref_type,
			} =>
			{
				let reference = reference.analyze(analyzer)?;
				Ok(Expression::Deref {
					reference,
					ref_type: ref_type.clone(),
					deref_type: deref_type.clone(),
				})
			}
			Expression::LengthOfArray { reference } =>
			{
				let reference = reference.analyze(analyzer)?;
				Ok(Expression::LengthOfArray { reference })
			}
			Expression::FunctionCall {
				name,
				arguments,
				return_type,
			} =>
			{
				let name = analyzer.use_function(name)?;
				let arguments: Result<Vec<Expression>, anyhow::Error> =
					arguments.iter().map(|a| a.analyze(analyzer)).collect();
				let arguments = arguments?;
				Ok(Expression::FunctionCall {
					name,
					arguments,
					return_type: return_type.clone(),
				})
			}
		}
	}
}

impl Analyzable for Reference
{
	type Item = Reference;

	fn analyze(
		&self,
		analyzer: &mut Analyzer,
	) -> Result<Self::Item, anyhow::Error>
	{
		match &self
		{
			Reference::Identifier(name) =>
			{
				let name = analyzer.use_variable(name)?;
				Ok(Reference::Identifier(name))
			}
			Reference::ArrayElement { name, argument } =>
			{
				let argument = argument.analyze(analyzer)?;
				let name = analyzer.use_variable(name)?;
				Ok(Reference::ArrayElement {
					name: name.clone(),
					argument: Box::new(argument),
				})
			}
		}
	}
}
