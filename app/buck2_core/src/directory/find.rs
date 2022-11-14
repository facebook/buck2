/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use thiserror::Error;

use super::Directory;
use super::DirectoryEntry;
use super::FingerprintedDirectory;
use super::PathAccumulator;
use crate::fs::paths::file_name::FileName;
use crate::fs::paths::file_name::FileNameBuf;
use crate::fs::paths::forward_rel_path::ForwardRelativePath;

#[derive(Debug, Error)]
pub enum DirectoryFindError {
    #[error("Path is empty")]
    EmptyPath,

    #[error("Find would traverse a leaf at path: `{}`", .path)]
    CannotTraverseLeaf { path: PathAccumulator },
}

macro_rules! impl_find {
    (
        $dir_ty: ident,
        $find_name: ident,
        $mod: ident,
    ) => {
        mod $mod {
            use super::*;

            pub fn $find_name<'a, L, H, D: $dir_ty<L, H>>(
                dir: &'a D,
                path: impl IntoIterator<Item = &'a FileName> + 'a,
            ) -> Result<Option<DirectoryEntry<&'a dyn $dir_ty<L, H>, &'a L>>, DirectoryFindError>
            {
                let mut path = path.into_iter();

                let path_needle = match path.next() {
                    Some(path_needle) => path_needle,
                    None => return Err(DirectoryFindError::EmptyPath),
                };

                find_inner(dir, path_needle, path)
                    .map_err(|path| DirectoryFindError::CannotTraverseLeaf { path })
            }

            fn find_inner<'a, 'b, L, H>(
                dir: &'a dyn $dir_ty<L, H>,
                path_needle: &'b FileName,
                mut path_rest: impl Iterator<Item = &'b FileName> + 'b,
            ) -> Result<Option<DirectoryEntry<&'a dyn $dir_ty<L, H>, &'a L>>, PathAccumulator> {
                let entry = match dir.get(path_needle) {
                    Some(entry) => entry,
                    None => return Ok(None),
                };

                let next_path_needle = match path_rest.next() {
                    Some(next_path_needle) => next_path_needle,
                    None => return Ok(Some(entry)),
                };

                if let DirectoryEntry::Dir(dir) = entry {
                    return find_inner(dir, next_path_needle, path_rest)
                        .map_err(|acc| acc.with(path_needle));
                }

                Err(PathAccumulator::new(path_needle))
            }
        }

        pub use $mod::$find_name;
    };
}

impl_find!(
    FingerprintedDirectory,
    find_fingerprinted,
    impl_find_fingerprinted,
);
impl_find!(Directory, find, impl_find,);
