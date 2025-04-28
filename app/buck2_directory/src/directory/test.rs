/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

#![cfg(test)]

use std::collections::hash_map::DefaultHasher;
use std::hash::Hash;
use std::hash::Hasher;

use allocative::Allocative;
use assert_matches::assert_matches;
use buck2_core::directory_digest::DirectoryDigest;
use buck2_core::directory_digest::InternableDirectoryDigest;
use buck2_core::fs::paths::file_name::FileName;
use buck2_core::fs::paths::forward_rel_path::ForwardRelativePath;
use buck2_core::fs::paths::forward_rel_path::ForwardRelativePathBuf;
use derive_more::Display;
use dupe::Dupe;

use crate::directory::builder::DirectoryBuilder;
use crate::directory::builder::DirectoryInsertError;
use crate::directory::builder::DirectoryMergeError;
use crate::directory::builder::DirectoryMkdirError;
use crate::directory::dashmap_directory_interner::DashMapDirectoryInterner;
use crate::directory::directory::Directory;
use crate::directory::directory_hasher::DirectoryHasher;
use crate::directory::directory_hasher::NoDigest;
use crate::directory::directory_iterator::DirectoryIterator;
use crate::directory::directory_iterator::DirectoryIteratorPathStack;
use crate::directory::directory_ref::FingerprintedDirectoryRef;
use crate::directory::directory_selector::DirectorySearchError;
use crate::directory::directory_selector::DirectorySelector;
use crate::directory::entry::DirectoryEntry;
use crate::directory::exclusive_directory::ExclusiveDirectory;
use crate::directory::find::find;
use crate::directory::find::find_prefix;
use crate::directory::immutable_directory::ImmutableDirectory;
use crate::directory::shared_directory::SharedDirectory;
use crate::directory::walk::ordered_entry_walk;

#[derive(Clone, Dupe, Debug, Eq, PartialEq, Hash)]
pub struct NopEntry;

pub struct TestHasher;

#[derive(Clone, Dupe, Debug, Eq, PartialEq, Hash, Allocative, Display)]
struct TestDigest(u64);

impl DirectoryDigest for TestDigest {}

impl InternableDirectoryDigest for TestDigest {}

impl DirectoryHasher<NopEntry, TestDigest> for TestHasher {
    fn hash_entries<'a, D, I>(&self, entries: I) -> TestDigest
    where
        I: IntoIterator<Item = (&'a FileName, DirectoryEntry<D, &'a NopEntry>)>,
        D: FingerprintedDirectoryRef<'a, Leaf = NopEntry, DirectoryDigest = TestDigest>,
    {
        let mut hasher = DefaultHasher::new();

        let mut entries = entries
            .into_iter()
            .map(|(name, entry)| {
                let entry = entry.map_dir(|d| d.as_fingerprinted_dyn().fingerprint());
                (name, entry)
            })
            .collect::<Vec<_>>();
        entries.sort_by_key(|(name, _)| *name);

        entries.hash(&mut hasher);
        TestDigest(hasher.finish())
    }
}

type TestDirectoryBuilder = DirectoryBuilder<NopEntry, TestDigest>;
type NoHasherDirectoryBuilder = DirectoryBuilder<NopEntry, NoDigest>;

fn path<'a>(s: &'a str) -> &'a ForwardRelativePath {
    ForwardRelativePath::unchecked_new(s)
}

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
fn test_walk() -> buck2_error::Result<()> {
    let mut b = TestDirectoryBuilder::empty();
    b.insert(path("a/b"), DirectoryEntry::Leaf(NopEntry))?;
    b.insert(
        path("b"),
        DirectoryEntry::Dir(TestDirectoryBuilder::empty()),
    )?;

    {
        let mut it = b.ordered_walk().with_paths();

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
            Some((p, _)) => assert_eq!(p, path("b"))
        );

        assert_matches!(it.next(), None);
    }

    {
        let it = b.unordered_walk().with_paths();
        let mut collected = it.collect::<Vec<_>>();
        collected.sort_by_key(|(name, _)| name.clone());
        let mut it = collected.into_iter();

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
            Some((p, _)) => assert_eq!(p, path("b"))
        );

        assert_matches!(it.next(), None);
    }

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
fn test_find() -> buck2_error::Result<()> {
    let mut a = TestDirectoryBuilder::empty();
    a.insert(path("a/b/c"), DirectoryEntry::Leaf(NopEntry))?;

    assert_matches!(
        find(a.as_ref(), path("a/b/c")),
        Ok(Some(DirectoryEntry::Leaf(..)))
    );

    assert_matches!(
        find(a.as_ref(), path("a/b")),
        Ok(Some(DirectoryEntry::Dir(..)))
    );

    assert_matches!(
        find(a.as_ref(), path("")),
        Ok(Some(DirectoryEntry::Dir(..)))
    );

    Ok(())
}

#[test]
fn test_find_prefix() -> buck2_error::Result<()> {
    let mut a = TestDirectoryBuilder::empty();
    a.insert(path("a/b/c"), DirectoryEntry::Leaf(NopEntry))?;

    assert_matches!(
        find_prefix(a.as_ref(), path("a/b/c")),
        Ok(Some((
            DirectoryEntry::Leaf(..),
            path
        ))) if path.is_empty()
    );
    assert_matches!(
        find_prefix(a.as_ref(), path("a/b")),
        Ok(Some((
            DirectoryEntry::Dir(..),
            path
        ))) if path.is_empty()
    );

    assert_matches!(
        find_prefix(a.as_ref(), path("a/b/c/d")),
        Ok(Some((DirectoryEntry::Leaf(..), rest))) => {
            assert_eq!(rest, path("d"));
        }
    );
    assert_matches!(
        find_prefix(a.as_ref(), path("a/b/c/d/e")),
        Ok(Some((DirectoryEntry::Leaf(..), rest))) => {
            assert_eq!(rest, path("d/e"));
        }
    );

    Ok(())
}

#[test]
fn test_search() -> buck2_error::Result<()> {
    let mut b = TestDirectoryBuilder::empty();
    b.insert(path("a/b"), DirectoryEntry::Leaf(NopEntry))?;
    b.insert(path("b/c"), DirectoryEntry::Leaf(NopEntry))?;
    let d = b.fingerprint(&TestHasher);

    {
        let mut selector = DirectorySelector::empty();
        selector.select(path("a/b"));

        let mut it = selector.ordered_search(&d).with_paths();

        assert_matches!(
            it.next(),
            Some((p, Ok(DirectoryEntry::Leaf(..)))) => assert_eq!(p, path("a/b"))
        );
        assert_matches!(it.next(), None)
    }

    {
        let mut selector = DirectorySelector::empty();
        selector.select(path("a/b/c"));
        selector.select(path("b/c"));

        let mut it = selector.ordered_search(&d).with_paths();

        assert_matches!(
            it.next(),
            Some((p, Err(DirectorySearchError::CannotTraverseLeaf { .. }))) => assert_eq!(p, path("a/b"))
        );
        assert_matches!(
            it.next(),
            Some((p, Ok(DirectoryEntry::Leaf(..)))) => assert_eq!(p, path("b/c"))
        );
        assert_matches!(it.next(), None)
    }

    {
        let mut selector = DirectorySelector::empty();
        selector.select(path("a"));

        let mut it = selector.ordered_search(&d).with_paths();
        assert_matches!(
            it.next(),
            Some((p, Ok(DirectoryEntry::Dir(..)))) => assert_eq!(p, path("a"))
        );
    }

    Ok(())
}

#[test]
fn test_filter() -> buck2_error::Result<()> {
    let mut b = TestDirectoryBuilder::empty();
    b.insert(path("a/aa"), DirectoryEntry::Leaf(NopEntry))?;
    b.insert(path("a/a"), DirectoryEntry::Leaf(NopEntry))?;
    b.insert(path("b/b"), DirectoryEntry::Leaf(NopEntry))?;
    b.insert(path("b/bb"), DirectoryEntry::Leaf(NopEntry))?;
    b.insert(path("c/c"), DirectoryEntry::Leaf(NopEntry))?;
    b.insert(path("c/cc"), DirectoryEntry::Leaf(NopEntry))?;

    let mut selector = DirectorySelector::empty();
    selector.select(path("a"));
    selector.select(path("b/b"));

    selector.filter(&mut b)?;

    let mut it = b.ordered_walk().with_paths();

    assert_matches!(
        it.next(),
        Some((p, _)) => assert_eq!(p, path("a"))
    );
    assert_matches!(
        it.next(),
        Some((p, _)) => assert_eq!(p, path("a/a"))
    );
    assert_matches!(
        it.next(),
        Some((p, _)) => assert_eq!(p, path("a/aa"))
    );
    assert_matches!(
        it.next(),
        Some((p, _)) => assert_eq!(p, path("b"))
    );
    assert_matches!(
        it.next(),
        Some((p, _)) => assert_eq!(p, path("b/b"))
    );

    assert_matches!(it.next(), None);

    Ok(())
}

#[test]
fn test_entry_walk() {
    {
        let e = DirectoryEntry::<TestDirectoryBuilder, _>::Leaf(NopEntry);
        let mut it = ordered_entry_walk(e.as_ref().map_dir(|d| d.as_ref()));

        assert_matches!(
            it.next(),
            Some((p, _)) => assert_eq!(p.get(), path(""))
        );

        assert_matches!(it.next(), None);
    }

    {
        let e = DirectoryEntry::<_, NopEntry>::Dir(TestDirectoryBuilder::empty());
        let mut it = ordered_entry_walk(e.as_ref().map_dir(|d| d.as_ref()));

        assert_matches!(it.next(), None);
    }
}

#[test]
fn test_bounds() {
    fn assert_impls_debug<T: std::fmt::Debug>() {}
    fn assert_impls_clone<T: std::clone::Clone>() {}
    fn assert_impls_eq<T: std::cmp::Eq>() {}

    assert_impls_debug::<TestDirectoryBuilder>();
    assert_impls_clone::<TestDirectoryBuilder>();
    assert_impls_eq::<DirectoryEntry<ExclusiveDirectory<NopEntry, TestDigest>, NopEntry>>();
    assert_impls_eq::<DirectoryEntry<SharedDirectory<NopEntry, TestDigest>, NopEntry>>();
    assert_impls_eq::<DirectoryEntry<ImmutableDirectory<NopEntry, TestDigest>, NopEntry>>();
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
fn test_directory_interner() -> buck2_error::Result<()> {
    let interner = DashMapDirectoryInterner::new();

    let d1 = {
        let mut b = TestDirectoryBuilder::empty();
        b.insert(path("a/b"), DirectoryEntry::Leaf(NopEntry))?;
        b.fingerprint(&TestHasher).shared(&interner)
    };

    let d2 = {
        let mut b = TestDirectoryBuilder::empty();
        b.insert(path("a/b"), DirectoryEntry::Leaf(NopEntry))?;
        b.fingerprint(&TestHasher).shared(&interner)
    };

    assert!(d1.ptr_eq(&d2));

    assert_eq!(interner.len(), 2);

    drop(d1);
    assert_eq!(interner.len(), 2);

    drop(d2);
    assert_eq!(interner.len(), 0);

    Ok(())
}

#[test]
fn test_directory_interner_deep() -> buck2_error::Result<()> {
    let interner = DashMapDirectoryInterner::new();

    let d1 = {
        let mut b = TestDirectoryBuilder::empty();
        b.insert(path("a/b"), DirectoryEntry::Leaf(NopEntry))?;
        b.fingerprint(&TestHasher).shared(&interner)
    };

    let _d2 = {
        let mut b = TestDirectoryBuilder::empty();
        b.insert(path("b"), DirectoryEntry::Leaf(NopEntry))?;
        b.fingerprint(&TestHasher).shared(&interner)
    };

    assert_eq!(interner.len(), 2);

    drop(d1);

    // Now we only have d2.
    assert_eq!(interner.len(), 1);

    Ok(())
}

#[test]
fn test_filter_continues_on_error() -> buck2_error::Result<()> {
    let mut b = TestDirectoryBuilder::empty();
    b.insert(path("a/aa/aaa"), DirectoryEntry::Leaf(NopEntry))?;
    b.insert(path("a/aa/bbb"), DirectoryEntry::Leaf(NopEntry))?;
    b.insert(path("a/bb"), DirectoryEntry::Leaf(NopEntry))?;
    b.insert(path("c"), DirectoryEntry::Leaf(NopEntry))?;

    let mut selector = DirectorySelector::empty();
    selector.select(path("a/aa"));
    selector.select(path("c/d"));

    assert_matches!(selector.filter(&mut b), Err(..));

    let mut it = b.ordered_walk().with_paths();

    assert_matches!(
        it.next(),
        Some((p, _)) => assert_eq!(p, path("a"))
    );
    assert_matches!(
        it.next(),
        Some((p, _)) => assert_eq!(p, path("a/aa"))
    );
    assert_matches!(
        it.next(),
        Some((p, _)) => assert_eq!(p, path("a/aa/aaa"))
    );
    assert_matches!(
        it.next(),
        Some((p, _)) => assert_eq!(p, path("a/aa/bbb"))
    );
    assert_matches!(
        it.next(),
        Some((p, _)) => assert_eq!(p, path("c"))
    );

    assert_matches!(it.next(), None);

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
