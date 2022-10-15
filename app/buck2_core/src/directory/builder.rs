/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use derivative::Derivative;
use either::Either;
use gazebo::prelude::*;
use starlark_map::small_map::Entry;
use starlark_map::small_map::SmallMap;
use thiserror::Error;

use super::Directory;
use super::DirectoryData;
use super::DirectoryEntries;
use super::DirectoryEntry;
use super::DirectoryHasher;
use super::ExclusiveDirectory;
use super::FingerprintedDirectory;
use super::HasDirectoryDigest;
use super::ImmutableDirectory;
use super::PathAccumulator;
use super::UnorderedDirectoryWalk;
use crate::fs::paths::FileName;
use crate::fs::paths::FileNameBuf;
use crate::fs::paths::ForwardRelativePath;
use crate::fs::paths::ForwardRelativePathBuf;
use crate::fs::paths::IntoFileNameBufIterator;
use crate::fs::project::ProjectRelativePath;
use crate::fs::project::ProjectRelativePathBuf;

#[derive(Debug, Error)]
pub enum DirectoryInsertError {
    #[error("Path is empty")]
    EmptyPath,

    #[error("Insert conflicts with an existing leaf at path: `{}`", .path)]
    CannotTraverseLeaf { path: PathAccumulator },
}

#[derive(Debug, Error)]
pub enum DirectoryMkdirError {
    #[error("Mkdir conflicts with an existing leaf at path: `{}`", .path)]
    CannotTraverseLeaf { path: PathAccumulator },
}

#[derive(Debug, Error)]
pub enum DirectoryMergeError {
    #[error("Merge conflicts wiht an existing leaf at path: `{}`", .path)]
    CannotTraverseLeaf { path: PathAccumulator },
}

/// A copy-on-write DirectoryBuilder.
#[derive(Derivative)]
#[derivative(Debug(bound = "L: ::std::fmt::Debug"))]
#[derivative(Clone(bound = "L: ::std::clone::Clone"))]
pub enum DirectoryBuilder<L, H>
where
    H: HasDirectoryDigest,
{
    /// This has a dedicated copy and we can mutate it.
    Mutable(SmallMap<FileNameBuf, DirectoryEntry<DirectoryBuilder<L, H>, L>>),
    Immutable(ImmutableDirectory<L, H>),
}

impl<L, H> DirectoryBuilder<L, H>
where
    H: HasDirectoryDigest,
{
    pub fn empty() -> Self {
        Self::Mutable(Default::default())
    }

    fn entries(
        &self,
    ) -> impl Iterator<Item = (&'_ FileName, DirectoryEntry<&'_ dyn Directory<L, H>, &'_ L>)> {
        match self {
            Self::Mutable(e) => {
                let it = e.iter().map(|(k, v)| {
                    let k = k.as_ref();
                    let v = v.as_ref().map_dir(|v| v as &dyn Directory<L, H>);
                    (k, v)
                });
                Either::Left(it)
            }
            Self::Immutable(e) => Either::Right(Directory::entries(e)),
        }
    }
}

impl<L, H> DirectoryBuilder<L, H>
where
    L: Clone,
    H: HasDirectoryDigest,
{
    /// Insert the entry `val` at `path`.
    ///
    /// If this replaces a portion of the tree, Ok(Some) is returned. For example inserting a file
    /// at `a/b` when the tree contains `a/b/c` would return a directory containining `c`, which is
    /// the node that was replaced at `a/b`. No path is returned under those circumstances since
    /// this can only happen at the input path.
    ///
    /// If this would conflict with an existing portion of the tree, Err is returned. This happens
    /// when inserting at a path that traverses through an existing file. For example, inserting at
    /// `a/b/c` when the current directory contains a file at `a/b` will return an error. The error
    /// indicates the path where the conflict occured.
    pub fn insert(
        &mut self,
        path: impl IntoFileNameBufIterator,
        val: DirectoryEntry<DirectoryBuilder<L, H>, L>,
    ) -> Result<Option<DirectoryEntry<DirectoryBuilder<L, H>, L>>, DirectoryInsertError> {
        let mut path = path.into_iter();

        let path_needle = match path.next() {
            Some(path_needle) => path_needle,
            None => return Err(DirectoryInsertError::EmptyPath),
        };

        self.insert_inner(path_needle, path, val)
            .map_err(|path| DirectoryInsertError::CannotTraverseLeaf { path })
    }

    fn insert_inner(
        &mut self,
        path_needle: FileNameBuf,
        mut path_rest: impl Iterator<Item = FileNameBuf>,
        val: DirectoryEntry<DirectoryBuilder<L, H>, L>,
    ) -> Result<Option<DirectoryEntry<DirectoryBuilder<L, H>, L>>, PathAccumulator> {
        let entries = self.as_mut();

        let next_path_needle = path_rest.next();

        match next_path_needle {
            Some(next_path_needle) => match entries.entry(path_needle) {
                Entry::Occupied(mut entry) => match entry.get_mut() {
                    DirectoryEntry::Dir(d) => d
                        .insert_inner(next_path_needle, path_rest, val)
                        .map_err(|acc| acc.with(entry.key())),
                    _ => Err(PathAccumulator::new(entry.key())),
                },
                Entry::Vacant(entry) => {
                    let mut dir = DirectoryBuilder::empty();
                    dir.insert_inner(next_path_needle, path_rest, val)
                        .map_err(|acc| acc.with(entry.key()))?;
                    entry.insert(DirectoryEntry::Dir(dir));
                    Ok(None)
                }
            },
            None => Ok(entries.insert(path_needle, val)),
        }
    }

    /// Create a directory at path. If the directory already exists, this does nothing. If this
    /// would overwrite a leaf, it fails.
    pub fn mkdir(&mut self, path: impl IntoFileNameBufIterator) -> Result<(), DirectoryMkdirError> {
        let mut path = path.into_iter();

        self.mkdir_inner(path)
            .map_err(|path| DirectoryMkdirError::CannotTraverseLeaf { path })
    }

    fn mkdir_inner(
        &mut self,
        mut path: impl Iterator<Item = FileNameBuf>,
    ) -> Result<(), PathAccumulator> {
        let entries = self.as_mut();

        let path_needle = match path.next() {
            Some(p) => p,
            None => return Ok(()),
        };

        match entries.entry(path_needle) {
            Entry::Occupied(mut entry) => match entry.get_mut() {
                DirectoryEntry::Dir(d) => {
                    d.mkdir_inner(path).map_err(|acc| acc.with(entry.key()))?
                }
                _ => return Err(PathAccumulator::new(entry.key())),
            },
            Entry::Vacant(entry) => {
                let mut dir = DirectoryBuilder::empty();
                dir.mkdir_inner(path).map_err(|acc| acc.with(entry.key()))?;
                entry.insert(DirectoryEntry::Dir(dir));
            }
        };

        Ok(())
    }

    pub fn merge(&mut self, mut other: Self) -> Result<(), DirectoryMergeError> {
        self.merge_inner(other)
            .map_err(|path| DirectoryMergeError::CannotTraverseLeaf { path })
    }

    fn merge_inner(&mut self, mut other: Self) -> Result<(), PathAccumulator> {
        match (&self, &other) {
            (Self::Immutable(d1), Self::Immutable(d2)) if d1.fingerprint() == d2.fingerprint() => {
                return Ok(());
            }
            _ => {}
        }

        let other = std::mem::take(other.as_mut());

        let entries = self.as_mut();

        for (k, v) in other.into_iter() {
            match entries.entry(k) {
                Entry::Occupied(mut entry) => match (entry.get_mut(), v) {
                    (DirectoryEntry::Dir(d), DirectoryEntry::Dir(o)) => {
                        d.merge_inner(o).map_err(|e| e.with(entry.key()))?;
                    }
                    (entry, DirectoryEntry::Leaf(o)) => {
                        *entry = DirectoryEntry::Leaf(o);
                    }
                    _ => return Err(PathAccumulator::new(entry.key())),
                },
                Entry::Vacant(entry) => {
                    entry.insert(v);
                }
            }
        }

        Ok(())
    }

    pub(super) fn as_mut(
        &mut self,
    ) -> &mut SmallMap<FileNameBuf, DirectoryEntry<DirectoryBuilder<L, H>, L>> {
        if let Self::Mutable(ref mut dir) = self {
            return dir;
        };

        let entries = match std::mem::replace(self, DirectoryBuilder::Mutable(Default::default())) {
            Self::Immutable(d) => d.into_entries::<SmallMap<_, _>>(),
            Self::Mutable(..) => unreachable!(),
        };

        match self {
            Self::Mutable(ref mut e) => {
                *e = entries;
                e
            }
            Self::Immutable(..) => unreachable!(),
        }
    }
}

impl<L, H> Directory<L, H> for DirectoryBuilder<L, H>
where
    H: HasDirectoryDigest,
{
    fn entries(&self) -> DirectoryEntries<'_, L, H> {
        box self.entries()
    }

    fn get<'a>(
        &'a self,
        needle: &'_ FileName,
    ) -> Option<DirectoryEntry<&'a dyn Directory<L, H>, &'a L>> {
        match self {
            Self::Mutable(ref dir) => dir
                .get(needle)
                .map(|v| v.as_ref().map_dir(|d| d as &dyn Directory<L, H>)),
            Self::Immutable(ref dir) => Directory::get(dir, needle),
        }
    }

    fn to_builder(&self) -> DirectoryBuilder<L, H>
    where
        L: Clone,
    {
        self.clone()
    }
}

impl<L, H> DirectoryBuilder<L, H>
where
    H: DirectoryHasher<L>,
{
    pub fn fingerprint(self) -> ImmutableDirectory<L, H> {
        match self {
            Self::Mutable(entries) => {
                let entries = entries
                    .into_iter()
                    .map(|(k, v)| (k, v.map_dir(|v| v.fingerprint())))
                    .collect();
                ImmutableDirectory::Exclusive(ExclusiveDirectory {
                    data: DirectoryData::new(entries),
                })
            }
            Self::Immutable(c) => c,
        }
    }
}
