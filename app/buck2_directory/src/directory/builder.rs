/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use std::mem;
use std::ops::ControlFlow;

use allocative::Allocative;
use buck2_core::directory_digest::DirectoryDigest;
use buck2_fs::paths::IntoFileNameBufIterator;
use buck2_fs::paths::file_name::FileName;
use buck2_fs::paths::file_name::FileNameBuf;
use buck2_fs::paths::forward_rel_path::ForwardRelativePath;
use buck2_fs::paths::forward_rel_path::ForwardRelativePathBuf;
use derivative::Derivative;
use dupe::Clone_;
use dupe::Copy_;
use dupe::Dupe;
use either::Either;
use starlark_map::small_map::Entry;
use starlark_map::small_map::SmallMap;

use crate::directory::builder_lazy::DirectoryBuilderLike;
use crate::directory::directory::Directory;
use crate::directory::directory_data::DirectoryData;
use crate::directory::directory_hasher::DirectoryDigester;
use crate::directory::directory_mut::DirectoryMut;
use crate::directory::directory_ref::DirectoryRef;
use crate::directory::entry::DirectoryEntry;
use crate::directory::exclusive_directory::ExclusiveDirectory;
use crate::directory::find::DirectoryFindError;
use crate::directory::find::find;
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
    L: Clone + PartialEq + Eq,
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

    /// Same as `merge`, but assumes that the two directories agree on where all the leaves are and
    /// also the values of those leaves.
    ///
    /// In other words, this cannot be used to "replace" any existing things in the directory
    pub fn merge_with_compatible_leaves(&mut self, other: Self) -> Result<(), DirectoryMergeError> {
        if !buck2_core::faster_directories::is_enabled() {
            return self.merge(other);
        }

        let v = std::mem::replace(self, DirectoryBuilder::empty());
        let v = v
            .merge_inner(other, true)
            .map_err(|path| DirectoryMergeError::CannotTraverseLeaf { path })?;
        *self = v;
        Ok(())
    }

    pub fn merge(&mut self, other: Self) -> Result<(), DirectoryMergeError> {
        if buck2_core::faster_directories::is_enabled() {
            let v = std::mem::replace(self, DirectoryBuilder::empty());
            let v = v
                .merge_inner(other, false)
                .map_err(|path| DirectoryMergeError::CannotTraverseLeaf { path })?;
            *self = v;
            Ok(())
        } else {
            self.merge_inner_old(other)
                .map_err(|path| DirectoryMergeError::CannotTraverseLeaf { path })
        }
    }

    fn merge_inner(
        mut self,
        mut other: Self,
        leaf_compatible: bool,
    ) -> Result<Self, PathAccumulator> {
        match (&self, &other) {
            (Self::Immutable(d1), Self::Immutable(d2)) if d1.fingerprint() == d2.fingerprint() => {
                return Ok(self);
            }
            (Self::Immutable(d1), d2) => {
                let d2_len = match d2 {
                    DirectoryBuilder::Mutable(m) => m.len(),
                    DirectoryBuilder::Immutable(im) => im.len(),
                };
                // Optimization: Merge the smaller directory into the larger one, since the work we
                // do is linear in the size of the RHS
                //
                // Only do this if the LHS is still immutable, as otherwise we'd just end up
                // spending O(len(new LHS)) to convert it into a SmallMap anyway
                //
                // Finally, we may also only do this if we know the leaves are compatible. Otherwise
                // we'll change which one overrides which
                if d1.len() < d2_len && leaf_compatible {
                    std::mem::swap(&mut self, &mut other);
                }
            }
            (Self::Mutable(m), _) if m.is_empty() => {
                return Ok(other);
            }
            _ => {}
        }

        // Use internal iteration instead of a for loop because of the `Either`s underlying this
        // iterator
        other.into_entries().try_for_each(|(k, v)| {
            self.map_entry(
                k,
                v,
                |v, dir_left, needle| match v {
                    DirectoryEntry::Dir(dir_right) => {
                        Ok(MapEntryOperation::Overwrite(DirectoryEntry::Dir(
                            dir_left
                                .merge_inner(dir_right, leaf_compatible)
                                .map_err(|e| e.with(needle))?,
                        )))
                    }
                    DirectoryEntry::Leaf(leaf_right) => Ok(MapEntryOperation::Overwrite(
                        DirectoryEntry::Leaf(leaf_right),
                    )),
                },
                |v, leaf_left, needle| match v {
                    DirectoryEntry::Dir(dir_right) => match leaf_left {
                        Some(_) => Err(PathAccumulator::new(needle)),
                        None => Ok(MapEntryOperation::Overwrite(DirectoryEntry::Dir(dir_right))),
                    },
                    DirectoryEntry::Leaf(leaf_right) => {
                        if leaf_compatible && leaf_left.is_some() {
                            Ok(MapEntryOperation::Keep(()))
                        } else {
                            Ok(MapEntryOperation::Overwrite(DirectoryEntry::Leaf(
                                leaf_right,
                            )))
                        }
                    }
                },
            )
        })?;

        Ok(self)
    }

    /// Old implementation of `merge_inner`, preserved for a/b test
    fn merge_inner_old(&mut self, mut other: Self) -> Result<(), PathAccumulator> {
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
                        d.merge_inner_old(o).map_err(|e| e.with(entry.key()))?;
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
}

enum MapEntryOperation<K, O> {
    Keep(K),
    Overwrite(O),
}

impl<T> MapEntryOperation<T, T> {
    fn into_inner(self) -> T {
        match self {
            Self::Keep(t) => t,
            Self::Overwrite(t) => t,
        }
    }
}

type BuilderEntry<L, H> = DirectoryEntry<DirectoryBuilder<L, H>, L>;

trait DirMapCallable<Ctx, L, H: DirectoryDigest, E> =
    FnOnce(
        Ctx,
        DirectoryBuilder<L, H>,
        &FileName,
    ) -> Result<MapEntryOperation<BuilderEntry<L, H>, BuilderEntry<L, H>>, E>;

trait LeafMapCallable<Ctx, L, H: DirectoryDigest, E> =
    FnOnce(Ctx, Option<&L>, &FileName) -> Result<MapEntryOperation<(), BuilderEntry<L, H>>, E>;

impl<L, H> DirectoryBuilder<L, H>
where
    H: DirectoryDigest,
    L: Clone + PartialEq + Eq,
{
    /// Map over an entry.
    ///
    /// Calls `map_dir` or `map_leaf` with the existing entry and performs the operation specified
    /// by the return value. `map_leaf` is called with `None` in the case where there isn't an
    /// existing entry.
    ///
    /// `ctx` is arbitrary data that the user can pass in, used to prove to the compiler that only
    /// one or the other closure is called
    ///
    /// If that sounds a bit awkward, it is.
    fn map_entry<E, Ctx>(
        &mut self,
        needle: FileNameBuf,
        ctx: Ctx,
        map_dir: impl DirMapCallable<Ctx, L, H, E>,
        map_leaf: impl LeafMapCallable<Ctx, L, H, E>,
    ) -> Result<(), E> {
        match self.map_entry_fasttrack(needle, ctx, map_dir, map_leaf) {
            ControlFlow::Break(v) => v,
            ControlFlow::Continue((needle, ctx, map_dir, map_leaf)) => {
                let s = self.as_mut();
                let entry = s.entry(needle);
                match entry {
                    Entry::Occupied(mut entry) => {
                        let (k, v) = entry.as_key_and_mut_value();
                        match v {
                            DirectoryEntry::Dir(v) => {
                                let vnew = map_dir(
                                    ctx,
                                    std::mem::replace(v, DirectoryBuilder::empty()),
                                    k,
                                )?;
                                *entry.get_mut() = vnew.into_inner();
                            }
                            DirectoryEntry::Leaf(l) => {
                                if let MapEntryOperation::Overwrite(lnew) =
                                    map_leaf(ctx, Some(l), k)?
                                {
                                    *entry.get_mut() = lnew;
                                }
                            }
                        }
                    }
                    Entry::Vacant(entry) => {
                        if let MapEntryOperation::Overwrite(enew) =
                            map_leaf(ctx, None, entry.key())?
                        {
                            entry.insert(enew);
                        }
                    }
                }
                Ok(())
            }
        }
    }

    /// Attempt a faster version of `map_entry`
    ///
    /// This is optimized to avoid requiring `as_mut` if nothing new is being inserted.
    fn map_entry_fasttrack<
        Ctx,
        E,
        MapD: DirMapCallable<Ctx, L, H, E>,
        MapL: LeafMapCallable<Ctx, L, H, E>,
    >(
        &mut self,
        needle: FileNameBuf,
        ctx: Ctx,
        map_dir: MapD,
        map_leaf: MapL,
    ) -> ControlFlow<Result<(), E>, (FileNameBuf, Ctx, MapD, MapL)> {
        let Self::Immutable(ImmutableDirectory::Shared(d)) = self else {
            return ControlFlow::Continue((needle, ctx, map_dir, map_leaf));
        };

        let Some(e) = d.get(&needle) else {
            return ControlFlow::Continue((needle, ctx, map_dir, map_leaf));
        };

        let new_entry = match e {
            DirectoryEntry::Leaf(l) => {
                let lnew = match map_leaf(ctx, Some(l), &needle) {
                    Ok(MapEntryOperation::Overwrite(lnew)) => lnew,
                    Ok(MapEntryOperation::Keep(())) => return ControlFlow::Break(Ok(())),
                    Err(e) => return ControlFlow::Break(Err(e)),
                };
                if let DirectoryEntry::Leaf(lnew) = &lnew {
                    if lnew == l {
                        return ControlFlow::Break(Ok(()));
                    }
                }
                lnew
            }
            DirectoryEntry::Dir(d) => {
                let fingerprint = d.fingerprint();
                let dnew = match map_dir(ctx, d.dupe().into_builder(), &needle) {
                    Ok(MapEntryOperation::Overwrite(dnew)) => dnew,
                    Ok(MapEntryOperation::Keep(_)) => return ControlFlow::Break(Ok(())),
                    Err(e) => return ControlFlow::Break(Err(e)),
                };
                if let DirectoryEntry::Dir(DirectoryBuilder::Immutable(
                    ImmutableDirectory::Shared(sharednew),
                )) = &dnew
                {
                    if sharednew.fingerprint() == fingerprint {
                        return ControlFlow::Break(Ok(()));
                    }
                }
                dnew
            }
        };

        let this = self.as_mut();
        this.insert(needle, new_entry);

        ControlFlow::Break(Ok(()))
    }
}

impl<L, H> DirectoryBuilder<L, H>
where
    L: Clone,
    H: DirectoryDigest,
{
    pub(super) fn as_mut(
        &mut self,
    ) -> &mut SmallMap<FileNameBuf, DirectoryEntry<DirectoryBuilder<L, H>, L>> {
        if let Self::Mutable(dir) = self {
            return dir;
        };

        let entries = match std::mem::replace(self, DirectoryBuilder::Mutable(Default::default())) {
            Self::Immutable(d) => d.collect_entries::<SmallMap<_, _>>(),
            Self::Mutable(..) => unreachable!(),
        };

        match self {
            Self::Mutable(e) => {
                *e = entries;
                e
            }
            Self::Immutable(..) => unreachable!(),
        }
    }

    fn into_entries(
        self,
    ) -> impl Iterator<Item = (FileNameBuf, DirectoryEntry<DirectoryBuilder<L, H>, L>)> {
        match self {
            Self::Mutable(entries) => Either::Left(entries.into_iter()),
            Self::Immutable(d) => Either::Right(d.into_entries()),
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
    type DirectoryRef<'a>
        = DirectoryBuilderDirectoryRef<'a, L, H>
    where
        Self: Sized + 'a,
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
    pub fn fingerprint(self, hasher: &impl DirectoryDigester<L, H>) -> ImmutableDirectory<L, H> {
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

impl<L, H> DirectoryBuilderLike<DirectoryBuilder<L, H>, L> for DirectoryBuilder<L, H>
where
    H: DirectoryDigest,
    L: Clone + PartialEq + Eq,
{
    fn insert(
        &mut self,
        path: ForwardRelativePathBuf,
        entry: DirectoryEntry<Self, L>,
    ) -> Result<(), DirectoryInsertError> {
        self.insert(&path, entry).map(|_| ())
    }

    fn merge(&mut self, dir: Self) -> Result<(), DirectoryMergeError> {
        self.merge_with_compatible_leaves(dir)
    }
}

#[cfg(test)]
mod tests {
    use std::cell::Cell;

    use assert_matches::assert_matches;
    use buck2_fs::paths::file_name::FileName;
    use buck2_fs::paths::forward_rel_path::ForwardRelativePath;
    use buck2_fs::paths::forward_rel_path::ForwardRelativePathBuf;

    use crate::directory::builder::DirectoryBuilder;
    use crate::directory::builder::DirectoryInsertError;
    use crate::directory::builder::DirectoryMergeError;
    use crate::directory::builder::DirectoryMkdirError;
    use crate::directory::dashmap_directory_interner::DashMapDirectoryInterner;
    use crate::directory::directory::Directory;
    use crate::directory::directory_hasher::DirectoryDigester;
    use crate::directory::directory_iterator::DirectoryIterator;
    use crate::directory::directory_ref::FingerprintedDirectoryRef;
    use crate::directory::entry::DirectoryEntry;
    use crate::directory::immutable_directory::ImmutableDirectory;
    use crate::directory::test::NoHasherDirectoryBuilder;
    use crate::directory::test::NopEntry;
    use crate::directory::test::TestDigest;
    use crate::directory::test::TestDirectoryBuilder;
    use crate::directory::test::TestHasher;
    use crate::directory::test::path;

    #[test]
    fn test_insert() -> buck2_error::Result<()> {
        let mut b = NoHasherDirectoryBuilder::empty();

        assert_matches!(
            b.insert(path("a/b"), DirectoryEntry::Leaf(NopEntry)),
            Ok(None)
        );

        assert_matches!(
            b.insert(path("a/b/c"), DirectoryEntry::Leaf(NopEntry)),
            Err(DirectoryInsertError::CannotTraverseLeaf { path }) => {
                assert_eq!(path.to_string(), "a/b");
            }
        );

        assert_matches!(
            b.insert(path("a"), DirectoryEntry::Leaf(NopEntry)),
            Ok(Some(DirectoryEntry::Dir(..)))
        );

        Ok(())
    }

    #[test]
    fn test_merge() -> buck2_error::Result<()> {
        let mut a = TestDirectoryBuilder::empty();
        a.insert(path("a/b"), DirectoryEntry::Leaf(NopEntry))?;

        let mut b = TestDirectoryBuilder::empty();
        b.insert(path("a/c"), DirectoryEntry::Leaf(NopEntry))?;

        a.merge(b)?;

        let mut it = a.ordered_walk().with_paths();

        assert_matches!(
            it.next(),
            Some((p, _)) => assert_eq!(p, path("a"))
        );

        assert_matches!(
            it.next(),
            Some((p, _)) => assert_eq!(p, path("a/b"))
        );

        assert_matches!(
            it.next(),
            Some((p, _)) => assert_eq!(p, path("a/c"))
        );

        assert_matches!(it.next(), None);

        Ok(())
    }

    #[test]
    fn test_merge_overwrite() -> buck2_error::Result<()> {
        let mut a = TestDirectoryBuilder::empty();
        a.insert(path("a/b"), DirectoryEntry::Leaf(NopEntry))?;

        let mut b = TestDirectoryBuilder::empty();
        b.insert(path("a"), DirectoryEntry::Leaf(NopEntry))?;

        a.merge(b)?;

        Ok(())
    }

    #[test]
    fn test_merge_conflict() -> buck2_error::Result<()> {
        let mut a = TestDirectoryBuilder::empty();
        a.insert(path("a"), DirectoryEntry::Leaf(NopEntry))?;

        let mut b = TestDirectoryBuilder::empty();
        b.insert(path("a/b"), DirectoryEntry::Leaf(NopEntry))?;

        assert_matches!(
            a.merge(b),
            Err(DirectoryMergeError::CannotTraverseLeaf { path }) => {
                assert_eq!(path.to_string(), "a");
            }
        );

        Ok(())
    }

    #[test]
    fn test_copy_on_write() -> buck2_error::Result<()> {
        let empty = TestDirectoryBuilder::empty().fingerprint(&TestHasher);

        let mut a = TestDirectoryBuilder::empty();
        a.insert(path("a"), DirectoryEntry::Dir(empty.into_builder()))?;

        a.insert(path("a/b"), DirectoryEntry::Leaf(NopEntry))?;

        let mut it = a.ordered_walk().with_paths();

        assert_matches!(
            it.next(),
            Some((p, _)) => assert_eq!(p, path("a"))
        );

        assert_matches!(
            it.next(),
            Some((p, _)) => assert_eq!(p, path("a/b"))
        );

        Ok(())
    }

    #[test]
    fn test_mkdir() -> buck2_error::Result<()> {
        let mut b = TestDirectoryBuilder::empty();
        b.mkdir(path("foo/bar"))?;
        b.mkdir(path("foo"))?;

        let mut it = b.ordered_walk().with_paths();

        assert_matches!(
            it.next(),
            Some((p, _)) => assert_eq!(p, path("foo"))
        );

        assert_matches!(
            it.next(),
            Some((p, _)) => assert_eq!(p, path("foo/bar"))
        );

        assert_matches!(it.next(), None);

        Ok(())
    }

    #[test]
    fn test_mkdir_overwrite() -> buck2_error::Result<()> {
        let mut b = TestDirectoryBuilder::empty();
        b.insert(path("a/b"), DirectoryEntry::Leaf(NopEntry))?;

        assert_matches!(
            b.mkdir(path("a/b/c")),
            Err(DirectoryMkdirError::CannotTraverseLeaf { path }) => {
                assert_eq!(path.to_string(), "a/b");
            }
        );

        Ok(())
    }

    #[test]
    fn test_remove_prefix_empty() {
        let mut b = TestDirectoryBuilder::empty();
        assert_eq!(
            Vec::<ForwardRelativePathBuf>::new(),
            b.ordered_walk_leaves().paths().collect::<Vec<_>>()
        );
        b.remove_prefix(path("")).unwrap();
        b.remove_prefix(path("a")).unwrap();
        b.remove_prefix(path("b/c")).unwrap();
        assert_eq!(
            Vec::<ForwardRelativePathBuf>::new(),
            b.ordered_walk_leaves().paths().collect::<Vec<_>>()
        );
    }

    #[test]
    fn test_remove_prefix_error() {
        let mut b = TestDirectoryBuilder::empty();
        b.insert(path("a/b"), DirectoryEntry::Leaf(NopEntry))
            .unwrap();
        assert!(b.remove_prefix(path("a/b/c")).is_err());
        assert_eq!(
            vec![path("a/b")],
            b.ordered_walk_leaves().paths().collect::<Vec<_>>()
        );
    }

    #[test]
    fn test_remove_prefix_leaf() {
        let mut b = TestDirectoryBuilder::empty();
        b.insert(path("a/b"), DirectoryEntry::Leaf(NopEntry))
            .unwrap();
        b.insert(path("a/x"), DirectoryEntry::Leaf(NopEntry))
            .unwrap();
        b.remove_prefix(path("a/b")).unwrap();
        assert_eq!(
            vec![path("a/x")],
            b.ordered_walk_leaves().paths().collect::<Vec<_>>()
        );
    }

    #[test]
    fn test_remove_prefix_tree() {
        let mut b = TestDirectoryBuilder::empty();
        b.insert(path("a/b/c"), DirectoryEntry::Leaf(NopEntry))
            .unwrap();
        b.insert(path("a/b/d"), DirectoryEntry::Leaf(NopEntry))
            .unwrap();
        b.insert(path("a/x"), DirectoryEntry::Leaf(NopEntry))
            .unwrap();
        b.remove_prefix(path("a/b")).unwrap();
        assert_eq!(
            vec![path("a/x")],
            b.ordered_walk_leaves().paths().collect::<Vec<_>>()
        );
    }

    struct CountingDigester(Cell<usize>, TestHasher);

    impl DirectoryDigester<NopEntry, TestDigest> for CountingDigester {
        fn hash_entries<'a, D, I>(&self, entries: I) -> TestDigest
        where
            I: IntoIterator<Item = (&'a FileName, DirectoryEntry<D, &'a NopEntry>)>,
            D: FingerprintedDirectoryRef<'a, Leaf = NopEntry, DirectoryDigest = TestDigest>,
        {
            self.0.set(self.0.get() + 1);
            self.1.hash_entries(entries)
        }

        fn leaf_size(&self, _leaf: &NopEntry) -> u64 {
            1
        }
    }

    fn make_directory(leaves: &[&'static str]) -> ImmutableDirectory<NopEntry, TestDigest> {
        let mut d = DirectoryBuilder::<NopEntry, TestDigest>::empty();
        for p in leaves {
            d.insert(
                ForwardRelativePath::new(*p).unwrap(),
                DirectoryEntry::Leaf(NopEntry),
            )
            .unwrap();
        }
        let interner = DashMapDirectoryInterner::new();
        ImmutableDirectory::Shared(d.fingerprint(&TestHasher).shared(&interner))
    }

    #[test]
    fn test_reuse_in_case_of_subset_merge() {
        let superset = make_directory(&["a/a", "b/b"]);
        let subset = make_directory(&["b/b"]);

        // Merge `subset` into `superset`
        let mut merged = superset.into_builder();
        merged.merge(subset.into_builder()).unwrap();
        // Compute the fingerprint of the merge and count the number of fingerprinting operations -
        // we use this as a way to measure whether `SharedDirectory`s are reused or not
        let digester = CountingDigester(Cell::new(0), TestHasher);
        merged.fingerprint(&digester);
        assert_eq!(0, digester.0.get());
    }

    #[test]
    fn test_reuse_in_case_of_superset_merge() {
        let superset = make_directory(&["a/a", "b/b"]);
        let subset = make_directory(&["b/b"]);

        // Merge `superset` into `subset`
        let mut merged = subset.into_builder();
        merged
            .merge_with_compatible_leaves(superset.into_builder())
            .unwrap();
        // Compute the fingerprint of the merge and count the number of fingerprinting operations -
        // we use this as a way to measure whether `ImmutableDirectory`s are reused or not
        let digester = CountingDigester(Cell::new(0), TestHasher);
        merged.fingerprint(&digester);
        // FIXME(JakobDegen): We should reuse the initial value
        assert_eq!(0, digester.0.get());
    }

    #[test]
    fn test_reuse_in_case_of_superset_on_leaves() {
        // Same as above, but this time there's no intermediate directories
        let superset = make_directory(&["a", "b"]);
        let subset = make_directory(&["b"]);

        let mut merged = superset.into_builder();
        merged.merge(subset.into_builder()).unwrap();
        let digester = CountingDigester(Cell::new(0), TestHasher);
        merged.fingerprint(&digester);
        assert_eq!(0, digester.0.get());
    }

    #[test]
    fn test_reuse_when_merging_into_empty_dir() {
        let mut merged = DirectoryBuilder::empty();
        merged.merge(make_directory(&["a"]).into_builder()).unwrap();

        let digester = CountingDigester(Cell::new(0), TestHasher);
        merged.fingerprint(&digester);
        assert_eq!(0, digester.0.get());
    }

    #[test]
    fn test_bounds() {
        fn assert_impls_debug<T: std::fmt::Debug>() {}
        fn assert_impls_clone<T: std::clone::Clone>() {}

        assert_impls_debug::<TestDirectoryBuilder>();
        assert_impls_clone::<TestDirectoryBuilder>();
    }
}
