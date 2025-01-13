/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use buck2_core::fs::paths::file_name::FileName;

use crate::directory::directory_ref::DirectoryRef;
use crate::directory::entry::DirectoryEntry;
use crate::directory::path_accumulator::PathAccumulator;

#[derive(Debug, buck2_error::Error)]
#[buck2(tag = Tier0)]
pub enum DirectoryFindError {
    #[error("Find would traverse a leaf at path: `{}`", .path)]
    CannotTraverseLeaf { path: PathAccumulator },
}

trait FindConflict<T> {
    fn new<'b>(path: &'b FileName, remaining: impl Iterator<Item = &'b FileName>, leaf: T) -> Self;

    fn with<'b>(self, path: &'b FileName) -> Self;
}

impl<T> FindConflict<T> for PathAccumulator {
    fn new<'b>(
        path: &'b FileName,
        _remaining: impl Iterator<Item = &'b FileName>,
        _leaf: T,
    ) -> Self {
        PathAccumulator::new(path)
    }

    fn with<'b>(self, path: &'b FileName) -> Self {
        PathAccumulator::with(self, path)
    }
}

#[cfg(test)]
struct PrefixLookupContainer<T> {
    leaf: T,
    path: buck2_core::fs::paths::forward_rel_path::ForwardRelativePathBuf,
}

#[cfg(test)]
impl<T> FindConflict<T> for PrefixLookupContainer<T> {
    fn new<'b>(path: &'b FileName, remaining: impl Iterator<Item = &'b FileName>, leaf: T) -> Self {
        Self {
            leaf,
            path: std::iter::once(path).chain(remaining).collect(),
        }
    }

    fn with<'b>(self, _path: &'b FileName) -> Self {
        self
    }
}

pub fn find<'a, 'b, D: DirectoryRef<'a>>(
    dir: D,
    path: impl IntoIterator<Item = &'b FileName>,
) -> Result<Option<DirectoryEntry<D, &'a D::Leaf>>, DirectoryFindError> {
    let mut path = path.into_iter();

    let path_needle = match path.next() {
        Some(path_needle) => path_needle,
        None => return Ok(Some(DirectoryEntry::Dir(dir))),
    };

    find_inner::<_, PathAccumulator>(dir, path_needle, path)
        .map_err(move |path| DirectoryFindError::CannotTraverseLeaf { path })
}

#[cfg(test)] // Dead code.
pub(crate) fn find_prefix<'a, 'b, D: DirectoryRef<'a>>(
    dir: D,
    path: impl IntoIterator<Item = &'b FileName>,
) -> Result<
    Option<(
        DirectoryEntry<D, &'a D::Leaf>,
        // Remaining path.
        buck2_core::fs::paths::forward_rel_path::ForwardRelativePathBuf,
    )>,
    DirectoryFindError,
> {
    let mut path = path.into_iter();

    let path_needle = match path.next() {
        Some(path_needle) => path_needle,
        None => {
            return Ok(Some((
                DirectoryEntry::Dir(dir),
                buck2_core::fs::paths::forward_rel_path::ForwardRelativePathBuf::default(),
            )));
        }
    };

    match find_inner::<_, PrefixLookupContainer<&'a D::Leaf>>(dir, path_needle, path) {
        Ok(maybe_leaf) => Ok(maybe_leaf.map(|l| {
            (
                l,
                buck2_core::fs::paths::forward_rel_path::ForwardRelativePathBuf::default(),
            )
        })),
        Err(PrefixLookupContainer { leaf, path }) => Ok(Some((DirectoryEntry::Leaf(leaf), path))),
    }
}

fn find_inner<'a, 'b, D: DirectoryRef<'a>, A>(
    dir: D,
    path_needle: &'b FileName,
    mut path_rest: impl Iterator<Item = &'b FileName>,
) -> Result<Option<DirectoryEntry<D, &'a D::Leaf>>, A>
where
    A: FindConflict<&'a D::Leaf>,
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
        DirectoryEntry::Dir(dir) => find_inner::<_, A>(dir, next_path_needle, path_rest)
            .map_err(|acc| acc.with(path_needle)),
        DirectoryEntry::Leaf(leaf) => Err(A::new(next_path_needle, path_rest, leaf)),
    }
}
