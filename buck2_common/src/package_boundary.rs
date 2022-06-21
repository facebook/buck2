/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::collections::HashMap;
use std::sync::Arc;

use anyhow::Context as _;
use async_trait::async_trait;
use buck2_core::cells::paths::CellPath;
use buck2_core::cells::paths::CellRelativePath;
use buck2_core::cells::CellName;
use buck2_core::fs::paths::FileNameBuf;
use buck2_core::fs::paths::ForwardRelativePath;
use buck2_core::fs::paths::ForwardRelativePathBuf;
use buck2_core::result::SharedResult;
use derive_more::Display;
use dice::DiceComputations;
use dice::Key;
use gazebo::prelude::*;
use ref_cast::RefCast;

use crate::legacy_configs::dice::HasLegacyConfigs;
use crate::legacy_configs::LegacyBuckConfigs;

#[derive(PartialEq)]
pub struct PackageBoundaryExceptions(HashMap<CellName, CellPackageBoundaryExceptions>);

#[derive(PartialEq)]
struct CellPackageBoundaryExceptions {
    // The reason we avoid a trie is that there's not a convenient `TrieSet` implementation to use,
    // and tries will likely have worse performance because most exception paths are very short.
    // Instead, we use a HashMap of first directory of the path to the rest of the path.
    prefix_to_subpaths: HashMap<FileNameBuf, Vec<ForwardRelativePathBuf>>,
    // Sometimes we want to say everything is allowed
    allow_everything: bool,
}

impl CellPackageBoundaryExceptions {
    fn new(s: &str) -> anyhow::Result<Self> {
        let mut prefix_to_subpaths = HashMap::new();
        let mut allow_everything = false;
        for path_str in s.split(',') {
            let path_str = path_str.trim();

            if path_str == "." {
                allow_everything = true;
            } else {
                let path = ForwardRelativePath::new(path_str)?;
                // path.split_first() only returns None if the path is empty.
                // In the case of the buckconfig `project.package_boundary_exceptions`,
                // we only get an empty path if there is an extra newline, in which case
                // we can just ignore.
                if let Some((prefix, subpath)) = path.split_first() {
                    let subpaths = prefix_to_subpaths
                        .entry(prefix.to_owned())
                        .or_insert_with(Vec::new);
                    subpaths.push(subpath.to_owned());
                }
            }
        }
        Ok(Self {
            prefix_to_subpaths,
            allow_everything,
        })
    }

    fn contains(&self, path: &CellRelativePath) -> bool {
        if self.allow_everything {
            return true;
        }
        let path: &ForwardRelativePath = path.as_ref();
        if let Some((package_prefix, package_subpath)) = path.split_first() {
            if let Some(subpaths) = self.prefix_to_subpaths.get(package_prefix) {
                return subpaths.iter().any(|p| package_subpath.starts_with(p));
            }
        }
        false
    }
}

impl PackageBoundaryExceptions {
    fn new(configs: &LegacyBuckConfigs) -> anyhow::Result<Self> {
        Ok(Self(
            configs
                .iter()
                .filter_map(|(name, cell_configs)| {
                    cell_configs
                        .get("project", "package_boundary_exceptions")
                        .map(|v| {
                            let e = CellPackageBoundaryExceptions::new(v).with_context(
                                || format!("When parsing `project.package_boundary_exceptions` key from cell `{}`", name)
                            )?;
                            Ok((name.clone(), e))
                        })
                })
                .collect::<anyhow::Result<_>>()?,
        ))
    }

    pub fn contains(&self, path: &CellPath) -> bool {
        if let Some(exceptions) = self.0.get(path.cell()) {
            exceptions.contains(path.path())
        } else {
            false
        }
    }
}

#[async_trait]
pub trait HasPackageBoundaryExceptions {
    async fn get_package_boundary_exceptions(&self)
    -> SharedResult<Arc<PackageBoundaryExceptions>>;

    async fn get_package_boundary_exception(&self, path: &CellPath) -> SharedResult<bool>;
}

#[async_trait]
impl HasPackageBoundaryExceptions for DiceComputations {
    async fn get_package_boundary_exceptions(
        &self,
    ) -> SharedResult<Arc<PackageBoundaryExceptions>> {
        #[derive(Hash, Eq, PartialEq, Clone, Dupe, Display, Debug)]
        #[display(fmt = "{:?}", self)]
        struct PackageBoundaryExceptionsKey;

        #[async_trait]
        impl Key for PackageBoundaryExceptionsKey {
            type Value = SharedResult<Arc<PackageBoundaryExceptions>>;

            async fn compute(&self, ctx: &DiceComputations) -> Self::Value {
                Ok(Arc::new(PackageBoundaryExceptions::new(
                    &ctx.get_legacy_configs().await,
                )?))
            }

            fn validity(x: &Self::Value) -> bool {
                x.is_ok()
            }

            fn equality(x: &Self::Value, y: &Self::Value) -> bool {
                match (x, y) {
                    (Ok(x), Ok(y)) => x == y,
                    _ => false,
                }
            }
        }

        self.compute(&PackageBoundaryExceptionsKey).await
    }

    async fn get_package_boundary_exception(&self, path: &CellPath) -> SharedResult<bool> {
        #[derive(Hash, Eq, PartialEq, Clone, Display, Debug, RefCast)]
        #[display(fmt = "{}", .0)]
        #[repr(transparent)]
        struct PackageBoundaryExceptionKey(CellPath);

        #[async_trait]
        impl Key for PackageBoundaryExceptionKey {
            type Value = SharedResult<bool>;

            async fn compute(&self, ctx: &DiceComputations) -> Self::Value {
                Ok(ctx
                    .get_package_boundary_exceptions()
                    .await?
                    .contains(&self.0))
            }

            fn validity(x: &Self::Value) -> bool {
                x.is_ok()
            }

            fn equality(x: &Self::Value, y: &Self::Value) -> bool {
                match (x, y) {
                    (Ok(x), Ok(y)) => x == y,
                    _ => false,
                }
            }
        }

        self.compute(PackageBoundaryExceptionKey::ref_cast(path))
            .await
    }
}

#[cfg(test)]
mod tests {
    use buck2_core::cells::paths::CellRelativePath;

    use super::*;

    #[test]
    pub fn test_package_boundary_exceptions() {
        let exceptions =
            CellPackageBoundaryExceptions::new("foo/bar/foo,foo/bar/bar,foo/bar/baz,foo/baz,baz")
                .unwrap();

        assert!(exceptions.contains(CellRelativePath::unchecked_new("foo/bar/foo")));
        assert!(exceptions.contains(CellRelativePath::unchecked_new("foo/bar/bar")));
        assert!(exceptions.contains(CellRelativePath::unchecked_new("foo/bar/baz")));
        assert!(exceptions.contains(CellRelativePath::unchecked_new("foo/bar/baz/test")));
        assert!(exceptions.contains(CellRelativePath::unchecked_new("foo/baz/util")));
        assert!(exceptions.contains(CellRelativePath::unchecked_new("foo/baz/util")));
        assert!(exceptions.contains(CellRelativePath::unchecked_new("baz/foo/bar")));

        assert!(!exceptions.contains(CellRelativePath::unchecked_new("foo/bar/scripts")));
        assert!(!exceptions.contains(CellRelativePath::unchecked_new("foo/scripts")));
        assert!(!exceptions.contains(CellRelativePath::unchecked_new("qux")));
    }

    #[test]
    pub fn test_package_boundary_dot() {
        let exceptions = CellPackageBoundaryExceptions::new("").unwrap();
        assert!(!exceptions.contains(CellRelativePath::unchecked_new("foo/bar/foo")));

        let exceptions = CellPackageBoundaryExceptions::new(",foo").unwrap();
        assert!(exceptions.contains(CellRelativePath::unchecked_new("foo/bar/foo")));
        assert!(!exceptions.contains(CellRelativePath::unchecked_new("bar/foo")));

        let exceptions = CellPackageBoundaryExceptions::new(".").unwrap();
        assert!(exceptions.contains(CellRelativePath::unchecked_new("foo/bar/foo")));
        assert!(exceptions.contains(CellRelativePath::unchecked_new("bar/foo")));
    }
}
