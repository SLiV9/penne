use crate::delta::parser::parse_tree::ParseTree;

pub fn expand(
	source_paths: &[std::path::PathBuf],
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

		// TODO find all import statements
		// TODO check not pub import

		for importee in imported_modules
		{
			if importee == importer
			{
				// TODO insert error somehow
				break;
			}

			modules[importer].append_all(headers[importee]);
		}
	}
}
