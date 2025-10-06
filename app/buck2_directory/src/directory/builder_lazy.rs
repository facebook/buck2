/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use buck2_core::directory_digest::DirectoryDigest;
use buck2_core::fs::paths::IntoFileNameBufIterator;

use crate::directory::builder::DirectoryBuilder;
use crate::directory::builder::DirectoryInsertError;
use crate::directory::builder::DirectoryMergeError;
use crate::directory::entry::DirectoryEntry;
use crate::directory::shared_directory::SharedDirectory;

pub trait DirectoryBuilderLike<D, L> {
    fn merge(&mut self, dir: D) -> Result<(), DirectoryMergeError>;

    fn insert(
        &mut self,
        path: impl IntoFileNameBufIterator,
        entry: DirectoryEntry<D, L>,
    ) -> Result<(), DirectoryInsertError>;
}

/// A builder similar to a `DirectoryBuilder` but with slightly different performance
/// characteristics.
///
/// The differences with a standard `DirectoryBuilder` are:
///  1. This performs significantly better for complex merges.
///  2. This does not in itself act like a directory
///  3. This requires that the pieces being merged are compatible in the sense that they agree on
///     where and what the leaves are. If this is not the case one will be picked arbitrarily.
///
/// TODO(JakobDegen): Make the above actually true
pub struct LazyDirectoryBuilder<L, H>
where
    H: DirectoryDigest,
    L: Clone + PartialEq + Eq,
{
    builder: DirectoryBuilder<L, H>,
}

impl<L, H> LazyDirectoryBuilder<L, H>
where
    H: DirectoryDigest,
    L: Clone + PartialEq + Eq,
{
    pub fn empty() -> Self {
        Self {
            builder: DirectoryBuilder::empty(),
        }
    }

    /// Merge this directory in
    ///
    /// Supporting only `SharedDirectory`s is effectively a performance hint - we could allow any
    /// directory, but the fast paths in the directory builder can only be hit with shared
    /// directories.
    pub fn merge(&mut self, dir: SharedDirectory<L, H>) -> Result<(), DirectoryMergeError> {
        self.builder
            .merge_with_compatible_leaves(dir.into_builder())
    }

    pub fn insert(
        &mut self,
        path: impl IntoFileNameBufIterator,
        entry: DirectoryEntry<SharedDirectory<L, H>, L>,
    ) -> Result<(), DirectoryInsertError> {
        self.builder
            .insert(path, entry.map_dir(|d| d.into_builder()))
            .map(|_| ())
    }

    pub fn finalize(self) -> buck2_error::Result<DirectoryBuilder<L, H>> {
        Ok(self.builder)
    }
}

impl<L, H> DirectoryBuilderLike<SharedDirectory<L, H>, L> for LazyDirectoryBuilder<L, H>
where
    H: DirectoryDigest,
    L: Clone + PartialEq + Eq,
{
    fn insert(
        &mut self,
        path: impl IntoFileNameBufIterator,
        entry: DirectoryEntry<SharedDirectory<L, H>, L>,
    ) -> Result<(), DirectoryInsertError> {
        self.insert(path, entry)
    }

    fn merge(&mut self, dir: SharedDirectory<L, H>) -> Result<(), DirectoryMergeError> {
        self.merge(dir)
    }
}

#[cfg(test)]
mod tests {
    use assert_matches::assert_matches;

    use crate::directory::builder_lazy::LazyDirectoryBuilder;
    use crate::directory::dashmap_directory_interner::DashMapDirectoryInterner;
    use crate::directory::directory::Directory;
    use crate::directory::directory_iterator::DirectoryIterator;
    use crate::directory::entry::DirectoryEntry;
    use crate::directory::test::NopEntry;
    use crate::directory::test::TestDigest;
    use crate::directory::test::TestDirectoryBuilder;
    use crate::directory::test::TestHasher;
    use crate::directory::test::path;

    type LazyTestDirectoryBuilder = LazyDirectoryBuilder<NopEntry, TestDigest>;

    #[test]
    fn test_smoke() -> buck2_error::Result<()> {
        let interner = DashMapDirectoryInterner::new();

        let mut a = LazyTestDirectoryBuilder::empty();
        a.insert(path("a/b"), DirectoryEntry::Leaf(NopEntry))?;

        let b = {
            let mut b = TestDirectoryBuilder::empty();
            b.insert(path("a/c"), DirectoryEntry::Leaf(NopEntry))?;
            b.fingerprint(&TestHasher).shared(&interner)
        };

        a.merge(b)?;

        let a = a.finalize()?;
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
}
