//! Minimal stub of the `buck-resources` crate. The real crate lives on
//! [docs.rs](https://docs.rs/buck-resources); this local stub exists only
//! so that the example workspace has a target to compile. In the merged
//! rustdoc tree, cross-crate references to [`get`] from consumer crates
//! resolve to `https://docs.rs/buck-resources/1.0.0/`
//! thanks to `rustdoc_html_root_url` on the `rust_library` target.

use std::path::PathBuf;

/// Resolve a resource path by its buck2 target label.
pub fn get(_name: &str) -> Result<PathBuf, Error> {
    Ok(PathBuf::new())
}

/// Error returned by [`get`] when a resource cannot be located.
#[derive(Debug)]
pub struct Error;
