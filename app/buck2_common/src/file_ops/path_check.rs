/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use allocative::Allocative;
use async_trait::async_trait;
use buck2_core::fs::i_project_rel_path::IProjectRelativePath;
use buck2_core::soft_error;
use derive_more::Display;
use dice::CancellationContext;
use dice::DiceComputations;
use dice::EqualityBehavior;
use dice::Key;
use dice::OkPagableValueSerialize;
use dice::ValueSerialize;
use dupe::Dupe;
use dupe::ResultDupedErrExt;
use pagable::Pagable;
use pagable::pagable_typetag;

use crate::file_ops::baseline::HasFileSystemBaseline;
use crate::file_ops::io::read_dir_cached;
use crate::file_ops::metadata::FileType;
use crate::ignores::ignore_set::HasIgnoreSet;

/// What is at a project path, established component by component from the project root.
///
/// A path is present only if each of its components is an entry, spelled identically, of the
/// directory before it, so that the answer is the same on case insensitive file systems as on
/// case sensitive ones. Symlinks are leaves: nothing is found through one. An ignored path, and
/// everything under it, is not present. The project root itself is always a present directory.
#[derive(Copy, Clone, Dupe, Debug, PartialEq, Eq, Allocative, Pagable)]
pub enum PathCheck {
    Present(FileType),
    /// `at`, the path or a prefix of it, is not an entry of its parent.
    Missing {
        at: IProjectRelativePath,
    },
    /// `at`, the path or a prefix of it, matches the ignore set.
    Ignored {
        at: IProjectRelativePath,
    },
    /// `at`, a strict prefix of the path, is a symlink.
    ParentSymlink {
        at: IProjectRelativePath,
    },
    /// `at`, a strict prefix of the path, exists but is not a directory.
    ParentNotDirectory {
        at: IProjectRelativePath,
        file_type: FileType,
    },
}

pub async fn check_path(
    ctx: &mut DiceComputations<'_>,
    path: IProjectRelativePath,
) -> buck2_error::Result<PathCheck> {
    ctx.compute(&PathCheckKey(path)).await?.dupe()
}

/// What a read wanted the path to be; decides the wording of a [`PathCheckError`].
#[derive(Copy, Clone, Dupe, Debug)]
pub enum ReadKind {
    Directory,
    File,
}

/// A read refused because of what the path check found.
#[derive(Debug, buck2_error::Error)]
#[buck2(tag = Input)]
#[error("{}", self.describe())]
pub struct PathCheckError {
    path: IProjectRelativePath,
    check: PathCheck,
    /// The `project.ignore` pattern behind `PathCheck::Ignored`.
    pattern: Option<String>,
    kind: ReadKind,
}

impl PathCheckError {
    pub fn new(
        ctx: &DiceComputations<'_>,
        path: IProjectRelativePath,
        check: PathCheck,
        kind: ReadKind,
    ) -> Self {
        let pattern = match check {
            PathCheck::Ignored { at } => ctx
                .global_data()
                .get_ignore_set()
                .matches_candidate(&at)
                .map(ToOwned::to_owned),
            _ => None,
        };
        Self {
            path,
            check,
            pattern,
            kind,
        }
    }

    pub fn check(&self) -> PathCheck {
        self.check
    }

    fn describe(&self) -> String {
        let (noun, wanted) = match self.kind {
            ReadKind::Directory => ("Directory", FileType::Directory),
            ReadKind::File => ("File", FileType::File),
        };
        let path = self.path;
        let pattern = || {
            format!(
                "`project.ignore` contains `{}`",
                self.pattern.as_deref().unwrap_or("?")
            )
        };
        match self.check {
            PathCheck::Present(file_type) if file_type == wanted => {
                format!("{noun} `{path}` exists")
            }
            PathCheck::Present(FileType::Symlink) => {
                format!("`{path}` is a symlink, which buck2 does not follow")
            }
            PathCheck::Present(file_type) => format!(
                "`{path}` is a {}, not a {}",
                file_type.describe(),
                wanted.describe()
            ),
            PathCheck::Missing { at } if at == path => format!("{noun} `{path}` does not exist"),
            PathCheck::Missing { at } => {
                format!("{noun} `{path}` does not exist (`{at}` does not exist)")
            }
            PathCheck::Ignored { at } if at == path => {
                format!("{noun} `{path}` is ignored ({})", pattern())
            }
            PathCheck::Ignored { at } => {
                format!(
                    "{noun} `{path}` is ignored (`{at}` is ignored, {})",
                    pattern()
                )
            }
            PathCheck::ParentSymlink { at } => {
                format!("{noun} `{path}` is behind the symlink `{at}`, which buck2 does not follow")
            }
            PathCheck::ParentNotDirectory { at, file_type } => format!(
                "{noun} `{path}` does not exist (`{at}` is a {}, not a directory)",
                file_type.describe()
            ),
        }
    }
}

/// Reports a read through a symlink and lets the caller go ahead with it the way it did before
/// the check existed; under `BUCK2_HARD_ERROR` the report is the error, as for any soft error.
///
/// Reads through symlinks are refused eventually; until the soft errors show where they happen,
/// they are counted instead. Everything that calls this is a candidate for refusal.
pub(super) fn read_through_symlink(
    ctx: &DiceComputations<'_>,
    path: IProjectRelativePath,
    check: PathCheck,
    kind: ReadKind,
) -> buck2_error::Result<()> {
    let error = PathCheckError::new(ctx, path, check, kind).into();
    match check {
        PathCheck::ParentSymlink { .. } => soft_error!("path_check_symlink_in_parent", error),
        _ => soft_error!("path_check_symlink_at_path", error),
    }
    Ok(())
}

/// Checks are invalidated by the file watcher through the `FileChangeTracker`, for exactly the
/// paths it reports. A change to a directory's entries reaches the checks of the paths under them
/// through the dependency of each check on its parent's.
///
/// The listing of the parent is read through the per-transaction directory cache rather than
/// `IoReadDirKey`, so that checking a path records no dependency on its parent's listing. Were it
/// to, adding a file to a directory would invalidate every read under each of its siblings;
/// `tests/core/fs/invalidations/test_root_directory.py` guards against this.
#[derive(
    Clone, Dupe, Copy, Display, Debug, Eq, Hash, PartialEq, Allocative, Pagable
)]
#[pagable_typetag(dice::DiceKeyDyn)]
pub(super) struct PathCheckKey(pub(super) IProjectRelativePath);

#[async_trait]
impl Key for PathCheckKey {
    type Value = buck2_error::Result<PathCheck>;

    async fn compute(
        &self,
        ctx: &mut DiceComputations,
        _cancellations: &CancellationContext,
    ) -> Self::Value {
        ctx.get_file_system_baseline().await?;
        let Some((parent, name)) = self.0.split_last() else {
            return Ok(PathCheck::Present(FileType::Directory));
        };
        match *ctx
            .compute(&PathCheckKey(parent))
            .await?
            .as_ref()
            .duped_err()?
        {
            PathCheck::Present(FileType::Directory) => {}
            PathCheck::Present(FileType::Symlink) => {
                return Ok(PathCheck::ParentSymlink { at: parent });
            }
            PathCheck::Present(file_type) => {
                return Ok(PathCheck::ParentNotDirectory {
                    at: parent,
                    file_type,
                });
            }
            inherited @ (PathCheck::Missing { .. }
            | PathCheck::Ignored { .. }
            | PathCheck::ParentSymlink { .. }
            | PathCheck::ParentNotDirectory { .. }) => return Ok(inherited),
        }
        if ctx.global_data().get_ignore_set().is_match(&self.0) {
            return Ok(PathCheck::Ignored { at: self.0 });
        }
        Ok(match read_dir_cached(ctx, parent).await?.lookup(name) {
            Some(file_type) => PathCheck::Present(file_type),
            None => PathCheck::Missing { at: self.0 },
        })
    }

    fn equality_behavior() -> EqualityBehavior<Self::Value> {
        EqualityBehavior::Compare(|x, y| match (x, y) {
            (Ok(x), Ok(y)) => x == y,
            _ => false,
        })
    }

    fn validity(x: &Self::Value) -> bool {
        x.is_ok()
    }

    fn value_serialize() -> impl ValueSerialize<Value = Self::Value> {
        OkPagableValueSerialize::<Self::Value>::new()
    }
}

#[cfg(test)]
mod tests {
    use std::sync::Arc;

    use buck2_core::cells::CellResolver;
    use buck2_core::cells::cell_path::CellPath;
    use buck2_core::cells::cell_root_path::CellRootPathBuf;
    use buck2_core::cells::name::CellName;
    use buck2_core::fs::project::ProjectRootTemp;
    use buck2_core::fs::project_rel_path::ProjectRelativePath;
    use buck2_core::fs::project_rel_path::ProjectRelativePathBuf;
    use buck2_fs::fs_util;
    use dice::DetectCycles;
    use dice::Dice;
    use dice::DiceTransaction;
    use dice::UserComputationData;

    use super::*;
    use crate::dice::cells::SetCellResolver;
    use crate::dice::data::testing::SetTestingIoProvider;
    use crate::file_ops::baseline::FileSystemBaseline;
    use crate::file_ops::baseline::SetFileSystemBaseline;
    use crate::file_ops::dice::DiceFileComputations;
    use crate::file_ops::io::FileChangeTracker;
    use crate::file_ops::io::initialize_read_dir_cache;
    use crate::ignores::ignore_set::IgnoreSet;
    use crate::ignores::ignore_set::SetIgnoreSet;

    fn p(s: &str) -> IProjectRelativePath {
        IProjectRelativePath::intern_directly(ProjectRelativePath::new(s).unwrap())
    }

    struct Fixture {
        fs: ProjectRootTemp,
        dice: Arc<Dice>,
        root: dice::BranchId,
        baseline: FileSystemBaseline,
        cells: CellResolver,
    }

    impl Fixture {
        /// One cell, `root`, at the project root.
        async fn new(ignores: &str) -> buck2_error::Result<Self> {
            Self::with_cells(ignores, &[("root", "")]).await
        }

        /// The first cell is the root cell and gets the ignores.
        async fn with_cells(ignores: &str, cells: &[(&str, &str)]) -> buck2_error::Result<Self> {
            let fs = ProjectRootTemp::new()?;
            let root_cell = CellName::testing_new(cells[0].0);
            let cells = CellResolver::testing_with_names_and_paths(
                &cells
                    .iter()
                    .map(|(name, path)| {
                        (
                            CellName::testing_new(name),
                            CellRootPathBuf::new(ProjectRelativePathBuf::unchecked_new(
                                (*path).to_owned(),
                            )),
                        )
                    })
                    .collect::<Vec<_>>(),
            );
            let mut dice = Dice::builder();
            dice.set_testing_io_provider(&fs);
            dice.set_ignore_set(Arc::new(IgnoreSet::testing_new_single_cell(
                cells.dupe(),
                root_cell,
                ignores,
            )?));
            let dice = dice.build(DetectCycles::Enabled);
            let root = dice.new_root().await;
            Ok(Self {
                fs,
                dice,
                root,
                baseline: FileSystemBaseline::unique(),
                cells,
            })
        }

        /// A transaction that has had `changes` reported to it, all under one baseline, so that
        /// what one transaction read is reused by the next unless reported changed.
        async fn transaction(
            &self,
            changes: FileChangeTracker,
        ) -> buck2_error::Result<DiceTransaction> {
            let mut data = UserComputationData::new();
            initialize_read_dir_cache(&mut data);
            let mut updater = self.dice.updater_with_data(self.root, data);
            updater.set_cell_resolver(self.cells.dupe())?;
            updater.set_file_system_baseline(self.baseline.dupe())?;
            changes.write_to_dice(&mut updater)?;
            Ok(updater.commit().await)
        }

        fn symlink(&self, link: &str, target: &str) {
            fs_util::symlink(
                target,
                self.fs
                    .path()
                    .resolve(ProjectRelativePath::new(link).unwrap()),
            )
            .unwrap();
        }
    }

    async fn check(ctx: &DiceTransaction, path: &str) -> PathCheck {
        check_path(&mut ctx.ctx(), p(path)).await.unwrap()
    }

    /// The entry names of a cell-level `read_dir`, or the error's message.
    async fn read_dir(ctx: &DiceTransaction, path: &str) -> Result<Vec<String>, String> {
        DiceFileComputations::read_dir(&mut ctx.ctx(), CellPath::testing_new(path))
            .await
            .map(|out| out.included.iter().map(|n| n.as_str().to_owned()).collect())
            .map_err(|e| format!("{e:#}"))
    }

    fn names(names: &[&str]) -> Result<Vec<String>, String> {
        Ok(names.iter().map(|n| (*n).to_owned()).collect())
    }

    fn fails_with(res: Result<Vec<String>, String>, needle: &str) {
        match res {
            Err(msg) => assert!(msg.contains(needle), "`{msg}` does not contain `{needle}`"),
            Ok(listing) => panic!("expected a failure mentioning `{needle}`, got {listing:?}"),
        }
    }

    #[tokio::test]
    async fn present_and_missing() -> buck2_error::Result<()> {
        let f = Fixture::new("").await?;
        f.fs.write_file("a/b/f", "");
        let ctx = f.transaction(FileChangeTracker::new()).await?;

        assert_eq!(
            check(&ctx, "").await,
            PathCheck::Present(FileType::Directory)
        );
        assert_eq!(
            check(&ctx, "a").await,
            PathCheck::Present(FileType::Directory)
        );
        assert_eq!(
            check(&ctx, "a/b").await,
            PathCheck::Present(FileType::Directory)
        );
        assert_eq!(
            check(&ctx, "a/b/f").await,
            PathCheck::Present(FileType::File)
        );
        assert_eq!(
            check(&ctx, "a/b/g").await,
            PathCheck::Missing { at: p("a/b/g") }
        );
        assert_eq!(
            check(&ctx, "a/x/y/z").await,
            PathCheck::Missing { at: p("a/x") }
        );
        assert_eq!(
            check(&ctx, "nope").await,
            PathCheck::Missing { at: p("nope") }
        );
        Ok(())
    }

    /// Spelling must match exactly, whatever the file system thinks.
    #[tokio::test]
    async fn case_must_match() -> buck2_error::Result<()> {
        let f = Fixture::new("").await?;
        f.fs.write_file("a/b/f", "");
        let ctx = f.transaction(FileChangeTracker::new()).await?;

        assert_eq!(
            check(&ctx, "a/B/f").await,
            PathCheck::Missing { at: p("a/B") }
        );
        assert_eq!(
            check(&ctx, "a/b/F").await,
            PathCheck::Missing { at: p("a/b/F") }
        );
        Ok(())
    }

    #[tokio::test]
    async fn symlinks_are_leaves() -> buck2_error::Result<()> {
        let f = Fixture::new("").await?;
        f.fs.write_file("a/b/f", "");
        f.symlink("l", "a");
        f.symlink("a/dangling", "nowhere");
        let ctx = f.transaction(FileChangeTracker::new()).await?;

        assert_eq!(
            check(&ctx, "l").await,
            PathCheck::Present(FileType::Symlink)
        );
        assert_eq!(
            check(&ctx, "l/b").await,
            PathCheck::ParentSymlink { at: p("l") }
        );
        assert_eq!(
            check(&ctx, "l/b/f").await,
            PathCheck::ParentSymlink { at: p("l") }
        );
        assert_eq!(
            check(&ctx, "a/dangling").await,
            PathCheck::Present(FileType::Symlink)
        );
        assert_eq!(
            check(&ctx, "a/dangling/x").await,
            PathCheck::ParentSymlink {
                at: p("a/dangling")
            }
        );
        Ok(())
    }

    #[tokio::test]
    async fn file_in_parent() -> buck2_error::Result<()> {
        let f = Fixture::new("").await?;
        f.fs.write_file("a/f", "");
        let ctx = f.transaction(FileChangeTracker::new()).await?;

        assert_eq!(
            check(&ctx, "a/f/x/y").await,
            PathCheck::ParentNotDirectory {
                at: p("a/f"),
                file_type: FileType::File
            }
        );
        Ok(())
    }

    /// Ignored paths are ignored whether or not they exist, and so is everything under them.
    #[tokio::test]
    async fn ignores() -> buck2_error::Result<()> {
        let f = Fixture::new("ign, **/*.pyc").await?;
        f.fs.write_file("ign/f", "");
        f.fs.write_file("a/x.pyc", "");
        let ctx = f.transaction(FileChangeTracker::new()).await?;

        assert_eq!(
            check(&ctx, "ign").await,
            PathCheck::Ignored { at: p("ign") }
        );
        assert_eq!(
            check(&ctx, "ign/f").await,
            PathCheck::Ignored { at: p("ign") }
        );
        assert_eq!(
            check(&ctx, "ign/g/h").await,
            PathCheck::Ignored { at: p("ign") }
        );
        assert_eq!(
            check(&ctx, "a").await,
            PathCheck::Present(FileType::Directory)
        );
        assert_eq!(
            check(&ctx, "a/x.pyc").await,
            PathCheck::Ignored { at: p("a/x.pyc") }
        );
        assert_eq!(
            check(&ctx, "a/x.pyc/y").await,
            PathCheck::Ignored { at: p("a/x.pyc") }
        );
        assert_eq!(
            check(&ctx, "buck-out/v2").await,
            PathCheck::Ignored { at: p("buck-out") }
        );
        Ok(())
    }

    /// A check is recomputed when the file watcher reports its path, and when it reports a
    /// prefix of it.
    #[tokio::test]
    async fn invalidation() -> buck2_error::Result<()> {
        let f = Fixture::new("").await?;
        f.fs.write_file("a/b/f", "");
        let ctx = f.transaction(FileChangeTracker::new()).await?;
        assert_eq!(
            check(&ctx, "a/b/f").await,
            PathCheck::Present(FileType::File)
        );
        assert_eq!(
            check(&ctx, "a/b/g").await,
            PathCheck::Missing { at: p("a/b/g") }
        );
        assert_eq!(check(&ctx, "c/d").await, PathCheck::Missing { at: p("c") });
        drop(ctx);

        fs_util::remove_file(
            f.fs.path()
                .resolve(ProjectRelativePath::new("a/b/f").unwrap()),
        )
        .unwrap();
        f.fs.write_file("a/b/g", "");
        f.fs.write_file("c/d", "");

        // Unreported changes are not seen.
        let ctx = f.transaction(FileChangeTracker::new()).await?;
        assert_eq!(
            check(&ctx, "a/b/f").await,
            PathCheck::Present(FileType::File)
        );
        assert_eq!(
            check(&ctx, "a/b/g").await,
            PathCheck::Missing { at: p("a/b/g") }
        );
        assert_eq!(check(&ctx, "c/d").await, PathCheck::Missing { at: p("c") });
        drop(ctx);

        let mut changes = FileChangeTracker::new();
        changes.file_added_or_removed(p("a/b/f"));
        changes.file_added_or_removed(p("a/b/g"));
        changes.dir_added_or_removed(p("c"));
        let ctx = f.transaction(changes).await?;
        assert_eq!(
            check(&ctx, "a/b/f").await,
            PathCheck::Missing { at: p("a/b/f") }
        );
        assert_eq!(
            check(&ctx, "a/b/g").await,
            PathCheck::Present(FileType::File)
        );
        assert_eq!(check(&ctx, "c/d").await, PathCheck::Present(FileType::File));
        Ok(())
    }

    /// Listing a directory goes through the check, and leaves out ignored entries.
    #[tokio::test]
    async fn read_dir_through_the_check() -> buck2_error::Result<()> {
        let f = Fixture::new("ign, **/*.pyc").await?;
        f.fs.write_file("a/b/f", "");
        f.fs.write_file("a/x.pyc", "");
        f.fs.write_file("ign/f", "");
        f.symlink("l", "a");
        let ctx = f.transaction(FileChangeTracker::new()).await?;

        assert_eq!(read_dir(&ctx, "root//a").await, names(&["b"]));
        assert_eq!(read_dir(&ctx, "root//").await, names(&["a", "l"]));
        fails_with(
            read_dir(&ctx, "root//ign").await,
            "is ignored (`project.ignore` contains `ign`)",
        );
        fails_with(
            read_dir(&ctx, "root//ign/g").await,
            "(`ign` is ignored, `project.ignore` contains `ign`)",
        );
        // Read through, with a soft error, until `read_through_symlink` is retired.
        assert_eq!(read_dir(&ctx, "root//l").await, names(&["b"]));
        assert_eq!(read_dir(&ctx, "root//l/b").await, names(&["f"]));
        fails_with(
            read_dir(&ctx, "root//a/b/f").await,
            "is a file, not a directory",
        );
        fails_with(
            read_dir(&ctx, "root//a/b/f/g").await,
            "(`a/b/f` is a file, not a directory)",
        );
        fails_with(
            read_dir(&ctx, "root//nope").await,
            "Directory `nope` does not exist",
        );
        fails_with(
            read_dir(&ctx, "root//a/B").await,
            "Directory `a/B` does not exist",
        );
        fails_with(
            read_dir(&ctx, "root//a/B/c").await,
            "(`a/B` does not exist)",
        );
        Ok(())
    }

    /// Nested cell roots are not entries of the enclosing cell, and not reachable through it.
    #[tokio::test]
    async fn read_dir_and_nested_cells() -> buck2_error::Result<()> {
        let f = Fixture::with_cells("", &[("root", ""), ("sub", "sub")]).await?;
        f.fs.write_file("sub/f", "");
        f.fs.write_file("x/f", "");
        let ctx = f.transaction(FileChangeTracker::new()).await?;

        assert_eq!(read_dir(&ctx, "root//").await, names(&["x"]));
        assert_eq!(read_dir(&ctx, "sub//").await, names(&["f"]));
        fails_with(
            read_dir(&ctx, "root//sub").await,
            "file is part of cell `sub`",
        );
        fails_with(
            read_dir(&ctx, "root//sub/f").await,
            "file is part of cell `sub`",
        );
        Ok(())
    }

    #[tokio::test]
    async fn exists_matching_exact_case() -> buck2_error::Result<()> {
        let f = Fixture::with_cells("ign", &[("root", ""), ("sub", "sub")]).await?;
        f.fs.write_file("a/b/f", "");
        f.fs.write_file("ign/f", "");
        f.fs.write_file("sub/f", "");
        f.symlink("l", "a");
        let ctx = f.transaction(FileChangeTracker::new()).await?;

        let exists = async |path: &str| {
            DiceFileComputations::exists_matching_exact_case(
                &mut ctx.ctx(),
                CellPath::testing_new(path),
            )
            .await
            .unwrap()
        };
        assert!(exists("root//a/b/f").await);
        assert!(exists("root//a/b").await);
        assert!(exists("root//l").await);
        assert!(exists("sub//f").await);
        assert!(!exists("root//a/B/f").await);
        assert!(!exists("root//a/b/F").await);
        // Read through, with a soft error, until `read_through_symlink` is retired.
        assert!(exists("root//l/b").await);
        assert!(!exists("root//l/nope").await);
        assert!(!exists("root//ign/f").await);
        assert!(!exists("root//missing/f").await);
        assert!(!exists("root//sub/f").await);
        Ok(())
    }
}
