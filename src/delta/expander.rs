use crate::delta::{lexer::tokens::Tokens, parser::parse_tree::ParseTree};

pub fn expand(
	source_paths: &[std::path::PathBuf],
	sources: &[String],
	tokens: &[Tokens],
	modules: &mut [ParseTree],
	headers: &[ParseTree],
)
{
	assert_eq!(modules.len(), source_paths.len());
	assert_eq!(modules.len(), headers.len());

	let mut imported_modules = Vec::with_capacity(modules.len());

	for importer in 0..modules.len()
	{
		imported_modules.clear();

		modules[importer].process_imports(
			&tokens[importer],
			&sources[importer],
			|import| match source_paths.iter().position(|x| x == import)
			{
				Some(x) if x == importer => Err(()),
				Some(importee) =>
				{
					imported_modules.push(importee);
					Ok(())
				}
				None => Err(()),
			},
		);

		dbg!(&imported_modules);
		for importee in imported_modules.drain(..)
		{
			assert_ne!(importer, importee);
			modules[importer].append_header(&headers[importee]);
		}
	}
}
