//
// Part of penne
// Copyright (c) 2020-2023 Sander in 't Veld
// License: MIT
//

//! During the import expansion stage, public symbols are shared between
//! compilation units.

use crate::common::Declaration;
use crate::common::DeclarationFlag;
use crate::error::Error;

use enumset::EnumSet;
use std::collections::HashSet;

pub fn expand(modules: &mut [(std::path::PathBuf, Vec<Declaration>)])
{
	let keys: Vec<std::path::PathBuf> =
		modules.iter().map(|(k, _v)| k.clone()).collect();
	let mut imports = HashSet::new();

	for (offset_of_includer, module) in modules.iter_mut().enumerate()
	{
		let (path_of_includer, declarations) = module;

		// Sort import declarations (-1) before other declarations (0).
		// It is important that this sort is stable for non-import declarations.
		declarations.sort_by_key(|x| -1 * i32::from(is_import(x)));

		// Process the import declarations.
		let k = declarations.partition_point(|x| is_import(x));
		for declaration in &mut declarations[0..k]
		{
			match declaration
			{
				Declaration::Import { filename, location } =>
				{
					match get_key_offset(filename, &keys, path_of_includer)
					{
						Some(offset) =>
						{
							imports.insert((offset_of_includer, offset));
						}
						None =>
						{
							let error = Error::UnresolvedImport {
								filename: filename.to_string(),
								location: location.clone(),
							};
							*declaration = Declaration::Poison(error.into());
						}
					}
				}
				_ => unreachable!(),
			}
		}
	}

	for (_, declarations) in modules.iter_mut()
	{
		// Filter out all the import declarations that we processed.
		declarations.retain(|x| !is_import(x));
	}

	// Splice imported declarations back into each other.
	// This is a bit ugly but it can happen in any order because imported
	// declarations are never pub, hence they are not reexported.
	// This would break if we had something like "pub use".
	imports.retain(|(from, to)| from != to);
	for (offset_of_includer, offset_of_includee) in imports
	{
		let (_, declarations) = &modules[offset_of_includee];
		let imported_declarations: Vec<Declaration> =
			declarations.iter().filter_map(|x| export(x)).collect();
		let (_, declarations) = &mut modules[offset_of_includer];
		declarations.splice(0..0, imported_declarations);
	}
}

pub fn expand_one(
	filename: &str,
	declarations: Vec<Declaration>,
) -> Vec<Declaration>
{
	let filepath = filename.parse().unwrap_or_default();
	let mut modules = [(filepath, declarations)];
	expand(&mut modules[..]);
	let [(_, declarations)] = modules;
	declarations
}

fn is_import(declaration: &Declaration) -> bool
{
	match declaration
	{
		Declaration::Import { .. } => true,
		_ => false,
	}
}

fn get_key_offset(
	filename: &str,
	keys: &[std::path::PathBuf],
	path_of_includer: &std::path::Path,
) -> Option<usize>
{
	path_of_includer
		.parent()
		.map(|path| path.join(filename.clone()))
		.and_then(|path| keys.iter().position(|x| x == &path))
}

fn export(declaration: &Declaration) -> Option<Declaration>
{
	match declaration
	{
		Declaration::Constant {
			name,
			value,
			value_type,
			flags,
			location_of_declaration,
			location_of_type,
		} => extract_public(flags).map(|flags| Declaration::Constant {
			name: name.clone(),
			value: value.clone(),
			value_type: value_type.clone(),
			flags,
			location_of_declaration: location_of_declaration.clone(),
			location_of_type: location_of_type.clone(),
		}),
		Declaration::Function {
			name,
			parameters,
			return_type,
			body: _,
			flags,
			location_of_declaration,
			location_of_return_type,
		} => extract_public(flags).map(|flags| Declaration::FunctionHead {
			name: name.clone(),
			parameters: parameters.clone(),
			return_type: return_type.clone(),
			flags,
			location_of_declaration: location_of_declaration.clone(),
			location_of_return_type: location_of_return_type.clone(),
		}),
		Declaration::FunctionHead {
			name,
			parameters,
			return_type,
			flags,
			location_of_declaration,
			location_of_return_type,
		} => extract_public(flags).map(|flags| Declaration::FunctionHead {
			name: name.clone(),
			parameters: parameters.clone(),
			return_type: return_type.clone(),
			flags,
			location_of_declaration: location_of_declaration.clone(),
			location_of_return_type: location_of_return_type.clone(),
		}),
		Declaration::Structure {
			name,
			members,
			structural_type,
			depth,
			flags,
			location_of_declaration,
		} => extract_public(flags).map(|flags| Declaration::Structure {
			name: name.clone(),
			members: members.clone(),
			structural_type: structural_type.clone(),
			depth: depth.clone(),
			flags,
			location_of_declaration: location_of_declaration.clone(),
		}),
		Declaration::Import { .. } => None,
		Declaration::Poison(_) => None,
	}
}

fn extract_public(
	flags: &EnumSet<DeclarationFlag>,
) -> Option<EnumSet<DeclarationFlag>>
{
	let mut flags = flags.clone();
	if flags.remove(DeclarationFlag::Public)
	{
		Some(flags)
	}
	else
	{
		None
	}
}
