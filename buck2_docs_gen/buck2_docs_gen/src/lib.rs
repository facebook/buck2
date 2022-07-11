//!
//! Documentation generation utilities

use std::path::PathBuf;

pub use buck2_docs_gen_derive::Buck2Docs;
use serde::Deserialize;
use serde::Serialize;
use starlark::values::docs::DocItem;

/// A logical grouping of documentation, generally used in the output path.
///
/// This directory should not start with a "/", and each component is separated by
/// a "/"
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize, Default)]
pub struct OutputDirectory {
    components: Vec<String>,
}

/// Errors when trying to resolve strings to [`OutputDirectory`] objects
#[derive(Debug, thiserror::Error)]
pub enum OutputDirectoryParseError {
    #[error("directory `{}` cannot be absolute", .0)]
    AbsolutePath(String),
    #[error("directory `{}` cannot contain `..` or `.` components", .0)]
    PathTraversal(String),
}

impl OutputDirectory {
    /// Attempt to parse an [`OutputDirectory`] from a string that is a relative path.
    ///
    /// Absolute paths and path traversals will result in an error.
    pub fn try_from_string(dir: &str) -> Result<Self, OutputDirectoryParseError> {
        let p = dir.trim();

        if p.starts_with('/') {
            Err(OutputDirectoryParseError::AbsolutePath(dir.to_owned()))
        } else {
            let components = p
                .split('/')
                .filter(|c| !c.is_empty())
                .map(|c| match c {
                    "." | ".." => Err(OutputDirectoryParseError::PathTraversal(dir.to_owned())),
                    c => Ok(c.to_owned()),
                })
                .collect::<Result<Vec<String>, _>>()?;
            Ok(Self { components })
        }
    }

    /// Get the relative path for this [`OutputDirectory`] that can be appended to e.g. output paths.
    pub fn path(&self) -> PathBuf {
        PathBuf::from(self.components.join("/"))
    }
}

/// Like [`StarlarkObject`], but with the DocItem resolved
pub struct StarlarkObjectDoc {
    pub name: String,
    pub directory: OutputDirectory,
    pub item: DocItem,
}

/// A starlark object to be documented
pub struct StarlarkObject {
    pub name: &'static str,
    pub directory: OutputDirectory,
    pub module: Box<dyn Fn() -> DocItem>,
}

impl StarlarkObject {
    /// Gets the docitem to this starlark object
    fn get_docs(&self) -> DocItem {
        (self.module)()
    }

    /// Gets all the documents from registered Starlark objects
    pub fn all_docs() -> impl Iterator<Item = StarlarkObjectDoc> {
        inventory::iter::<StarlarkObject>
            .into_iter()
            .map(|obj| StarlarkObjectDoc {
                name: obj.name.to_owned(),
                directory: obj.directory.clone(),
                item: obj.get_docs(),
            })
    }
}

inventory::collect!(StarlarkObject);

/// __derive_refs allows us to reference other crates in buck2_docs_gen_derive without users
/// needing to be aware of those dependencies. We make them public here and then can reference
/// them like `buck2_docs_gen::__derive_refs::foo`.
#[doc(hidden)]
pub mod __derive_refs {
    pub use inventory;
    pub use starlark;
}

#[cfg(test)]
mod test {
    use std::path::PathBuf;

    use crate::OutputDirectory;

    #[test]
    fn parses_output_directories() {
        assert_eq!(
            PathBuf::from("foo/bar"),
            OutputDirectory::try_from_string("foo/bar").unwrap().path()
        );
        assert_eq!(
            PathBuf::from("foo/bar"),
            OutputDirectory::try_from_string(" foo/bar").unwrap().path()
        );
        assert_eq!(
            PathBuf::from("foo/bar"),
            OutputDirectory::try_from_string("foo//bar").unwrap().path()
        );
        assert_eq!(
            PathBuf::new(),
            OutputDirectory::try_from_string("").unwrap().path()
        );
        assert!(OutputDirectory::try_from_string("/foo").is_err());
        assert!(OutputDirectory::try_from_string("foo/../bar").is_err());
        assert!(OutputDirectory::try_from_string("foo/./bar").is_err());
    }
}
