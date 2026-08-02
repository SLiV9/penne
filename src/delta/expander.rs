use core::num;
use std::collections::{HashMap, HashSet};

use crate::delta::{lexer::tokens::Tokens, parser::parse_tree::ParseTree};

pub fn expand(
	source_paths: &[std::path::PathBuf],
	sources: &[String],
	tokens: &[Tokens],
	modules: &mut [ParseTree],
	headers: &[ParseTree],
)
{
	let num_modules = modules.len();
	assert_eq!(source_paths.len(), num_modules);
	assert_eq!(sources.len(), num_modules);
	assert_eq!(tokens.len(), num_modules);
	assert_eq!(headers.len(), num_modules);

	let mut import_matrix = vec![0u8; num_modules * num_modules];

	for importer in 0..num_modules
	{
		modules[importer].process_imports(
			&tokens[importer],
			&sources[importer],
			|import| match get_key_offset(
				import,
				source_paths,
				&source_paths[importer],
			)
			{
				Some(x) if x == importer => Err(()),
				Some(importee) =>
				{
					assert_ne!(importer, importee);
					import_matrix[importer * num_modules + importee] = 1;
					for i in 0..num_modules
					{
						import_matrix[importer * num_modules + i] |=
							import_matrix[importee * num_modules + i];
					}
					for i in 0..num_modules
					{
						import_matrix[i * num_modules + importee] |=
							import_matrix[i * num_modules + importer];
					}
					Ok(())
				}
				None => Err(()),
			},
		);
	}

	for importer in 0..num_modules
	{
		for importee in 0..num_modules
		{
			if importee == importer
			{
				continue;
			}
			if import_matrix[importer * num_modules + importee] == 0
			{
				continue;
			}
			modules[importer].append_header(&headers[importee]);
		}
	}
}

fn get_key_offset(
	import: &std::path::Path,
	keys: &[std::path::PathBuf],
	path_of_includer: &std::path::Path,
) -> Option<usize>
{
	(keys.iter().position(|x| x == import))
		.or_else(|| {
			path_of_includer
				.parent()
				.map(|path| path.join(import))
				.and_then(|path| keys.iter().position(|x| x == &path))
		})
		.filter(|i| &keys[*i] != path_of_includer)
}
