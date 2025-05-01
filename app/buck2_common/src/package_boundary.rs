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

use allocative::Allocative;
use async_trait::async_trait;
use buck2_core::cells::cell_path::CellPath;
use buck2_core::cells::cell_path::CellPathRef;
use buck2_core::cells::name::CellName;
use buck2_core::cells::paths::CellRelativePath;
use buck2_core::cells::paths::CellRelativePathBuf;
use buck2_core::fs::paths::file_name::FileNameBuf;
use buck2_core::fs::paths::forward_rel_path::ForwardRelativePath;
use buck2_core::fs::paths::forward_rel_path::ForwardRelativePathBuf;
use buck2_futures::cancellation::CancellationContext;
use derive_more::Display;
use dice::DiceComputations;
use dice::Key;
use dupe::Dupe;
use ref_cast::RefCast;

use crate::legacy_configs::dice::HasLegacyConfigs;
use crate::legacy_configs::key::BuckconfigKeyRef;

#[derive(PartialEq, Allocative)]
pub struct PackageBoundaryExceptions(HashMap<CellName, CellPackageBoundaryExceptions>);

#[derive(PartialEq, Allocative)]
struct CellPackageBoundaryExceptions {
    // The reason we avoid a trie is that there's not a convenient `TrieSet` implementation to use,
    // and tries will likely have worse performance because most exception paths are very short.
    // Instead, we use a HashMap of first directory of the path to the rest of the path.
    prefix_to_subpaths: HashMap<FileNameBuf, Vec<ForwardRelativePathBuf>>,
    // Sometimes we want to say everything is allowed
    allow_everything: bool,
}

impl CellPackageBoundaryExceptions {
    fn new(s: &str) -> buck2_error::Result<Self> {
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

    fn get_package_boundary_exception_path(
        &self,
        path: &CellRelativePath,
    ) -> Option<CellRelativePathBuf> {
        if self.allow_everything {
            return Some(CellRelativePathBuf::unchecked_new("".to_owned()));
        }
        let path: &ForwardRelativePath = path.as_ref();
        if let Some((package_prefix, package_subpath)) = path.split_first() {
            if let Some(subpaths) = self.prefix_to_subpaths.get(package_prefix) {
                return subpaths
                    .iter()
                    .find(|p| package_subpath.starts_with(p))
                    .map(|p| {
                        CellRelativePath::new(<&ForwardRelativePath>::from(package_prefix)).join(p)
                    });
            }
        }
        None
    }
}

#[derive(Hash, Eq, PartialEq, Clone, Dupe, Display, Debug, Allocative)]
#[display("{:?}", self)]
struct CellPackageBoundaryExceptionsKey(CellName);

#[async_trait]
impl Key for CellPackageBoundaryExceptionsKey {
    type Value = buck2_error::Result<Option<Arc<CellPackageBoundaryExceptions>>>;

    async fn compute(
        &self,
        ctx: &mut DiceComputations,
        _cancellations: &CancellationContext,
    ) -> Self::Value {
        let s = ctx
            .get_legacy_config_property(
                self.0,
                BuckconfigKeyRef {
                    section: "project",
                    property: "package_boundary_exceptions",
                },
            )
            .await?;
        if let Some(s) = s {
            Ok(Some(Arc::new(CellPackageBoundaryExceptions::new(&s)?)))
        } else {
            Ok(None)
        }
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

#[async_trait]
pub trait HasPackageBoundaryExceptions {
    async fn get_package_boundary_exception(
        &mut self,
        path: CellPathRef<'async_trait>,
    ) -> buck2_error::Result<Option<Arc<CellPath>>>;
}

#[async_trait]
impl HasPackageBoundaryExceptions for DiceComputations<'_> {
    async fn get_package_boundary_exception(
        &mut self,
        path: CellPathRef<'async_trait>,
    ) -> buck2_error::Result<Option<Arc<CellPath>>> {
        #[derive(Hash, Eq, PartialEq, Clone, Display, Debug, RefCast, Allocative)]
        #[repr(transparent)]
        struct PackageBoundaryExceptionKey(CellPath);

        #[async_trait]
        impl Key for PackageBoundaryExceptionKey {
            type Value = buck2_error::Result<Option<Arc<CellPath>>>;

            async fn compute(
                &self,
                ctx: &mut DiceComputations,
                _cancellations: &CancellationContext,
            ) -> Self::Value {
                let Some(exceptions) = ctx
                    .compute(&CellPackageBoundaryExceptionsKey(self.0.cell()))
                    .await??
                else {
                    return Ok(None);
                };
                Ok(exceptions
                    .get_package_boundary_exception_path(self.0.path())
                    .map(|p| Arc::new(CellPath::new(self.0.cell(), p))))
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

        self.compute(&PackageBoundaryExceptionKey(path.to_owned()))
            .await?
    }
}

#[cfg(test)]
mod tests {

    use super::*;

    fn get_package_boundary_exception_path(
        exceptions: &CellPackageBoundaryExceptions,
        s: &str,
    ) -> Option<CellRelativePathBuf> {
        exceptions.get_package_boundary_exception_path(CellRelativePath::unchecked_new(s))
    }

    fn package_boundary_allowlist_path(s: &str) -> Option<CellRelativePathBuf> {
        Some(CellRelativePathBuf::unchecked_new(s.to_owned()))
    }

    #[test]
    fn test_package_boundary_exceptions() {
        let exceptions =
            CellPackageBoundaryExceptions::new("foo/bar/foo,foo/bar/bar,foo/bar/baz,foo/baz,baz")
                .unwrap();

        assert_eq!(
            get_package_boundary_exception_path(&exceptions, "foo/bar/foo"),
            package_boundary_allowlist_path("foo/bar/foo"),
        );
        assert_eq!(
            get_package_boundary_exception_path(&exceptions, "foo/bar/bar"),
            package_boundary_allowlist_path("foo/bar/bar"),
        );
        assert_eq!(
            get_package_boundary_exception_path(&exceptions, "foo/bar/baz"),
            package_boundary_allowlist_path("foo/bar/baz"),
        );
        assert_eq!(
            get_package_boundary_exception_path(&exceptions, "foo/bar/baz/test"),
            package_boundary_allowlist_path("foo/bar/baz"),
        );
        assert_eq!(
            get_package_boundary_exception_path(&exceptions, "foo/baz/util"),
            package_boundary_allowlist_path("foo/baz"),
        );
        assert_eq!(
            get_package_boundary_exception_path(&exceptions, "foo/baz/util"),
            package_boundary_allowlist_path("foo/baz"),
        );
        assert_eq!(
            get_package_boundary_exception_path(&exceptions, "baz/foo/bar"),
            package_boundary_allowlist_path("baz"),
        );

        assert_eq!(
            get_package_boundary_exception_path(&exceptions, "foo/bar/scripts"),
            None,
        );
        assert_eq!(
            get_package_boundary_exception_path(&exceptions, "foo/scripts"),
            None,
        );
        assert_eq!(
            get_package_boundary_exception_path(&exceptions, "qux"),
            None,
        );
    }

    #[test]
    fn test_package_boundary_dot() {
        let exceptions = CellPackageBoundaryExceptions::new("").unwrap();
        assert!(get_package_boundary_exception_path(&exceptions, "foo/bar/foo").is_none());

        let exceptions = CellPackageBoundaryExceptions::new(",foo").unwrap();
        assert_eq!(
            get_package_boundary_exception_path(&exceptions, "foo/bar/foo"),
            package_boundary_allowlist_path("foo"),
        );
        assert_eq!(
            exceptions
                .get_package_boundary_exception_path(CellRelativePath::unchecked_new("bar/foo")),
            None,
        );

        let exceptions = CellPackageBoundaryExceptions::new(".").unwrap();
        assert_eq!(
            get_package_boundary_exception_path(&exceptions, "foo/bar/foo"),
            package_boundary_allowlist_path(""),
        );
        assert_eq!(
            get_package_boundary_exception_path(&exceptions, "bar/foo"),
            package_boundary_allowlist_path(""),
        );
    }
}
