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
use crate::fs::paths::forward_rel_path::ForwardRelativePathBuf;

#[derive(Debug, Error)]
pub enum DirectoryFindError {
    #[error("Path is empty")]
    EmptyPath,

    #[error("Find would traverse a leaf at path: `{}`", .path)]
    CannotTraverseLeaf { path: PathAccumulator },
}

trait FindConflict<'a, L> {
    fn new<'b>(
        path: &'b FileName,
        remaining: impl Iterator<Item = &'b FileName>,
        leaf: &'a L,
    ) -> Self;

    fn with<'b>(self, path: &'b FileName) -> Self;
}

impl<'a, L> FindConflict<'a, L> for PathAccumulator {
    fn new<'b>(
        path: &'b FileName,
        _remaining: impl Iterator<Item = &'b FileName>,
        _leaf: &'a L,
    ) -> Self {
        PathAccumulator::new(path)
    }

    fn with<'b>(self, path: &'b FileName) -> Self {
        PathAccumulator::with(self, path)
    }
}

struct PrefixLookupContainer<'a, L> {
    leaf: &'a L,
    path: ForwardRelativePathBuf,
}

impl<'a, L> FindConflict<'a, L> for PrefixLookupContainer<'a, L> {
    fn new<'b>(
        path: &'b FileName,
        remaining: impl Iterator<Item = &'b FileName>,
        leaf: &'a L,
    ) -> Self {
        Self {
            leaf,
            path: std::iter::once(path)
                .chain(remaining)
                .collect::<Option<ForwardRelativePathBuf>>()
                .expect("We know there is at least one path component"),
        }
    }

    fn with<'b>(self, _path: &'b FileName) -> Self {
        self
    }
}

macro_rules! impl_find {
    (
        $dir_ty: ident,
        $find_name: ident,
        $find_prefix_name: ident,
        $mod: ident,
    ) => {
        mod $mod {
            use super::*;

            pub fn $find_name<'a, 'b, L, H, D: $dir_ty<L, H>>(
                dir: &'a D,
                path: impl IntoIterator<Item = &'b FileName>,
            ) -> Result<Option<DirectoryEntry<&'a dyn $dir_ty<L, H>, &'a L>>, DirectoryFindError>
            {
                let mut path = path.into_iter();

                let path_needle = match path.next() {
                    Some(path_needle) => path_needle,
                    None => return Err(DirectoryFindError::EmptyPath),
                };

                find_inner::<_, _, PathAccumulator>(dir, path_needle, path)
                    .map_err(|path| DirectoryFindError::CannotTraverseLeaf { path })
            }

            pub fn $find_prefix_name<'a, 'b, L, H, D: $dir_ty<L, H>>(
                dir: &'a D,
                path: impl IntoIterator<Item = &'b FileName>,
            ) -> Result<
                Option<(
                    DirectoryEntry<&'a dyn $dir_ty<L, H>, &'a L>,
                    Option<ForwardRelativePathBuf>,
                )>,
                DirectoryFindError,
            > {
                let mut path = path.into_iter();

                let path_needle = match path.next() {
                    Some(path_needle) => path_needle,
                    None => return Err(DirectoryFindError::EmptyPath),
                };

                match find_inner::<_, _, PrefixLookupContainer<L>>(dir, path_needle, path) {
                    Ok(maybe_leaf) => Ok((maybe_leaf.map(|l| (l, None)))),
                    Err(PrefixLookupContainer { leaf, path }) => {
                        Ok(Some((DirectoryEntry::Leaf(leaf), Some(path))))
                    }
                }
            }

            fn find_inner<'a, 'b, L, H, A>(
                dir: &'a dyn $dir_ty<L, H>,
                path_needle: &'b FileName,
                mut path_rest: impl Iterator<Item = &'b FileName>,
            ) -> Result<Option<DirectoryEntry<&'a dyn $dir_ty<L, H>, &'a L>>, A>
            where
                A: FindConflict<'a, L>,
            {
                let entry = match dir.get(path_needle) {
                    Some(entry) => entry,
                    None => return Ok(None),
                };

                let next_path_needle = match path_rest.next() {
                    Some(next_path_needle) => next_path_needle,
                    None => return Ok(Some(entry)),
                };

                match entry {
                    DirectoryEntry::Dir(dir) => {
                        find_inner::<_, _, A>(dir, next_path_needle, path_rest)
                            .map_err(|acc| acc.with(path_needle))
                    }
                    DirectoryEntry::Leaf(leaf) => Err(A::new(next_path_needle, path_rest, leaf)),
                }
            }
        }

        pub use $mod::$find_name;
        pub use $mod::$find_prefix_name;
    };
}

impl_find!(
    FingerprintedDirectory,
    find_fingerprinted,
    find_prefix_fingerprinted,
    impl_find_fingerprinted,
);
impl_find!(Directory, find, find_prefix, impl_find,);
