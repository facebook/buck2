/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::mem;

use allocative::Allocative;
use buck2_core::directory_digest::DirectoryDigest;
use buck2_core::fs::paths::file_name::FileName;
use buck2_core::fs::paths::file_name::FileNameBuf;
use buck2_core::fs::paths::forward_rel_path::ForwardRelativePath;
use buck2_core::fs::paths::IntoFileNameBufIterator;
use derivative::Derivative;
use dupe::Clone_;
use dupe::Copy_;
use starlark_map::small_map::Entry;
use starlark_map::small_map::SmallMap;

use crate::directory::directory::Directory;
use crate::directory::directory_data::DirectoryData;
use crate::directory::directory_hasher::DirectoryHasher;
use crate::directory::directory_mut::DirectoryMut;
use crate::directory::directory_ref::DirectoryRef;
use crate::directory::entry::DirectoryEntry;
use crate::directory::exclusive_directory::ExclusiveDirectory;
use crate::directory::find::find;
use crate::directory::find::DirectoryFindError;
use crate::directory::fingerprinted_directory::FingerprintedDirectory;
use crate::directory::immutable_directory::ImmutableDirectory;
use crate::directory::immutable_or_exclusive::ImmutableOrExclusiveDirectoryEntries;
use crate::directory::immutable_or_exclusive::ImmutableOrExclusiveDirectoryRef;
use crate::directory::path_accumulator::PathAccumulator;

#[derive(Debug, buck2_error::Error)]
#[buck2(tag = Environment)]
pub enum DirectoryInsertError {
    #[error("Path is empty")]
    EmptyPath,

    #[error("Insert conflicts with an existing leaf at path: `{}`", .path)]
    CannotTraverseLeaf { path: PathAccumulator },
}

#[derive(Debug, buck2_error::Error)]
#[buck2(tag = Environment)]
pub enum DirectoryMkdirError {
    #[error("Mkdir conflicts with an existing leaf at path: `{}`", .path)]
    CannotTraverseLeaf { path: PathAccumulator },
}

#[derive(Debug, buck2_error::Error)]
#[buck2(tag = Environment)]
pub enum DirectoryMergeError {
    #[error("Merge conflicts with an existing leaf at path: `{}`", .path)]
    CannotTraverseLeaf { path: PathAccumulator },
}

/// A copy-on-write DirectoryBuilder.
#[derive(Derivative, Allocative)]
#[derivative(Debug(bound = "L: ::std::fmt::Debug"))]
#[derivative(Clone(bound = "L: ::std::clone::Clone"))]
pub enum DirectoryBuilder<L, H>
where
    H: DirectoryDigest,
{
    /// This has a dedicated copy and we can mutate it.
    Mutable(SmallMap<FileNameBuf, DirectoryEntry<DirectoryBuilder<L, H>, L>>),
    Immutable(ImmutableDirectory<L, H>),
}

impl<L, H> DirectoryBuilder<L, H>
where
    H: DirectoryDigest,
{
    pub fn empty() -> Self {
        Self::Mutable(Default::default())
    }
}

impl<L, H> DirectoryBuilder<L, H>
where
    L: Clone,
    H: DirectoryDigest,
{
    /// Insert the entry `val` at `path`.
    ///
    /// If this replaces a portion of the tree, Ok(Some) is returned. For example inserting a file
    /// at `a/b` when the tree contains `a/b/c` would return a directory containing `c`, which is
    /// the node that was replaced at `a/b`. No path is returned under those circumstances since
    /// this can only happen at the input path.
    ///
    /// If this would conflict with an existing portion of the tree, Err is returned. This happens
    /// when inserting at a path that traverses through an existing file. For example, inserting at
    /// `a/b/c` when the current directory contains a file at `a/b` will return an error. The error
    /// indicates the path where the conflict occurred.
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
        path_rest: impl IntoIterator<Item = FileNameBuf>,
        val: DirectoryEntry<DirectoryBuilder<L, H>, L>,
    ) -> Result<Option<DirectoryEntry<DirectoryBuilder<L, H>, L>>, PathAccumulator> {
        let entries = self.as_mut();

        let mut path_rest = path_rest.into_iter();
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
        let path = path.into_iter();

        self.mkdir_inner(path)
            .map_err(|path| DirectoryMkdirError::CannotTraverseLeaf { path })
    }

    fn mkdir_inner(
        &mut self,
        path: impl IntoIterator<Item = FileNameBuf>,
    ) -> Result<(), PathAccumulator> {
        let entries = self.as_mut();

        let mut path = path.into_iter();

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

    pub fn merge(&mut self, other: Self) -> Result<(), DirectoryMergeError> {
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

    /// Remove everything under `path`.
    pub fn remove_prefix(
        &mut self,
        path: &ForwardRelativePath,
    ) -> Result<Option<DirectoryEntry<DirectoryBuilder<L, H>, L>>, DirectoryFindError> {
        // If this is already mut, we could skip `find` to avoid traversing twice.
        match find(self.as_ref(), path)? {
            None => Ok(None),
            Some(_) => Ok(Some(self.do_remove_prefix(path))),
        }
    }

    fn do_remove_prefix(
        &mut self,
        path: &ForwardRelativePath,
    ) -> DirectoryEntry<DirectoryBuilder<L, H>, L> {
        let Some((path, last)) = path.split_last() else {
            return DirectoryEntry::Dir(mem::replace(self, DirectoryBuilder::empty()));
        };
        let mut this = self;
        for name in path {
            this = match this.as_mut().get_mut(name).unwrap() {
                DirectoryEntry::Dir(d) => d,
                DirectoryEntry::Leaf(_) => unreachable!(),
            }
        }
        let removed = this.as_mut().shift_remove(last);
        removed.unwrap()
    }
}

pub enum DirectoryBuilderDirectoryEntries<'a, L, H>
where
    H: DirectoryDigest,
{
    Immutable(ImmutableOrExclusiveDirectoryEntries<'a, L, H>),
    Mutable(
        starlark_map::small_map::Iter<'a, FileNameBuf, DirectoryEntry<DirectoryBuilder<L, H>, L>>,
    ),
}

impl<'a, L, H> Iterator for DirectoryBuilderDirectoryEntries<'a, L, H>
where
    H: DirectoryDigest,
{
    type Item = (
        &'a FileName,
        DirectoryEntry<DirectoryBuilderDirectoryRef<'a, L, H>, &'a L>,
    );

    fn next(&mut self) -> Option<Self::Item> {
        match self {
            Self::Immutable(iter) => {
                let (name, entry) = iter.next()?;
                Some((name, entry.map_dir(DirectoryBuilderDirectoryRef::Immutable)))
            }
            Self::Mutable(iter) => {
                let (name, entry) = iter.next()?;
                Some((
                    name,
                    entry
                        .as_ref()
                        .map_dir(DirectoryBuilderDirectoryRef::Mutable),
                ))
            }
        }
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        match self {
            Self::Immutable(iter) => iter.size_hint(),
            Self::Mutable(iter) => iter.size_hint(),
        }
    }
}

#[derive(Copy_, Clone_, Derivative)]
#[derivative(Debug(bound = "L: ::std::fmt::Debug"))]
pub enum DirectoryBuilderDirectoryRef<'a, L, H>
where
    H: DirectoryDigest,
{
    Immutable(ImmutableOrExclusiveDirectoryRef<'a, L, H>),
    Mutable(&'a DirectoryBuilder<L, H>),
}

impl<'a, L, H> DirectoryRef<'a> for DirectoryBuilderDirectoryRef<'a, L, H>
where
    H: DirectoryDigest,
{
    type Leaf = L;
    type DirectoryDigest = H;
    type Entries = DirectoryBuilderDirectoryEntries<'a, L, H>;

    fn get(self, name: &FileName) -> Option<DirectoryEntry<Self, &'a Self::Leaf>> {
        match self {
            DirectoryBuilderDirectoryRef::Immutable(d) => Some(
                d.get(name)?
                    .map_dir(DirectoryBuilderDirectoryRef::Immutable),
            ),
            DirectoryBuilderDirectoryRef::Mutable(d) => match d {
                DirectoryBuilder::Mutable(d) => Some(
                    d.get(name)?
                        .as_ref()
                        .map_dir(|v| DirectoryBuilderDirectoryRef::Mutable(v)),
                ),
                DirectoryBuilder::Immutable(d) => Some(
                    d.as_ref()
                        .get(name)?
                        .map_dir(|v| DirectoryBuilderDirectoryRef::Immutable(v)),
                ),
            },
        }
    }

    fn entries(self) -> Self::Entries {
        match self {
            Self::Immutable(d) => DirectoryBuilderDirectoryEntries::Immutable(d.entries()),
            Self::Mutable(d) => match d {
                DirectoryBuilder::Mutable(d) => DirectoryBuilderDirectoryEntries::Mutable(d.iter()),
                DirectoryBuilder::Immutable(d) => DirectoryBuilderDirectoryEntries::Immutable(
                    ImmutableOrExclusiveDirectoryRef::from_immutable(d).entries(),
                ),
            },
        }
    }

    fn as_dyn(self) -> &'a dyn Directory<Self::Leaf, Self::DirectoryDigest> {
        match self {
            Self::Immutable(d) => d.as_dyn(),
            Self::Mutable(d) => d,
        }
    }
}

impl<L, H> Directory<L, H> for DirectoryBuilder<L, H>
where
    H: DirectoryDigest,
{
    type DirectoryRef<'a> = DirectoryBuilderDirectoryRef<'a, L, H>
    where Self: Sized + 'a,
          L: 'a;

    fn as_ref<'a>(&'a self) -> Self::DirectoryRef<'a>
    where
        Self: Sized + 'a,
    {
        DirectoryBuilderDirectoryRef::Mutable(self)
    }

    fn to_builder(&self) -> DirectoryBuilder<L, H>
    where
        L: Clone,
    {
        self.clone()
    }
}

impl<L, H> DirectoryMut<L, H> for DirectoryBuilder<L, H>
where
    H: DirectoryDigest,
    L: Clone,
{
    fn get_mut<'a>(
        &'a mut self,
        needle: &'_ FileName,
    ) -> Option<DirectoryEntry<&'a mut dyn DirectoryMut<L, H>, &'a mut L>> {
        self.as_mut()
            .get_mut(needle)
            .map(|v| v.as_mut().map_dir(|d| d as &mut dyn DirectoryMut<L, H>))
    }
}

impl<L, H> DirectoryBuilder<L, H>
where
    H: DirectoryDigest,
{
    pub fn fingerprint(self, hasher: &impl DirectoryHasher<L, H>) -> ImmutableDirectory<L, H> {
        match self {
            Self::Mutable(entries) => {
                let entries = entries
                    .into_iter()
                    .map(|(k, v)| (k, v.map_dir(|v| v.fingerprint(hasher))))
                    .collect();
                ImmutableDirectory::Exclusive(ExclusiveDirectory {
                    data: DirectoryData::new(entries, hasher),
                })
            }
            Self::Immutable(c) => c,
        }
    }
}
