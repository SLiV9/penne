//
// Part of penne
// Copyright (c) 2022 Sander in 't Veld
// License: MIT
//

//! The source files for core and vendor libraries are embedded within the
//! compiler binary, in order to simplify installation.

use include_dir::include_dir;

static CORE: include_dir::Dir = include_dir!("./core/");
static VENDOR: include_dir::Dir = include_dir!("./vendor/");

/// If the source URI used in an import statement
/// is recognized as a core or vendor library,
/// return the parent URI that can be added as a compiler argument
/// to include that library.
pub fn source_name_hint(filepath: &str) -> Option<&str>
{
	let filepath = std::path::Path::new(filepath);
	let component: std::path::Component = filepath.components().next()?;
	match component
	{
		std::path::Component::Prefix(_) => None,
		std::path::Component::RootDir => None,
		std::path::Component::CurDir => None,
		std::path::Component::ParentDir => None,
		std::path::Component::Normal(name) =>
		{
			name.to_str().filter(|name| find(name).is_some())
		}
	}
}

/// Look for an included core or vendor library with the specified source URI.
pub fn find(
	filepath: &str,
) -> Option<(&'static include_dir::DirEntry<'static>, &'static str)>
{
	get_included_dir(filepath, &CORE, "core:")
		.or_else(|| get_included_dir(filepath, &VENDOR, "vendor:"))
}

fn get_included_dir(
	filepath: &str,
	included_dir: &'static include_dir::Dir,
	scheme_prefix: &'static str,
) -> Option<(&'static include_dir::DirEntry<'static>, &'static str)>
{
	if let Some(subpath) = filepath.strip_prefix(scheme_prefix)
	{
		match included_dir.get_entry(subpath)
		{
			Some(entry) => Some((&entry, scheme_prefix)),
			None => None,
		}
	}
	else
	{
		None
	}
}
