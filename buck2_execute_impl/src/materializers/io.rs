/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::collections::HashMap;
use std::path::PathBuf;

use buck2_core::directory::DirectoryEntry;
use buck2_core::fs::fs_util;
use buck2_core::fs::paths::AbsPath;
use buck2_core::fs::paths::AbsPathBuf;
use buck2_core::fs::project::ProjectRelativePathBuf;
use buck2_core::fs::project::ProjectRoot;
use buck2_execute::directory::ActionDirectory;
use buck2_execute::directory::ActionDirectoryEntry;
use buck2_execute::directory::ActionDirectoryMember;
use buck2_execute::directory::ActionSharedDirectory;
use buck2_execute::execute::blocking::IoRequest;

pub struct MaterializeTreeStructure {
    pub path: ProjectRelativePathBuf,
    pub entry: ActionDirectoryEntry<ActionSharedDirectory>,
}

impl IoRequest for MaterializeTreeStructure {
    fn execute(self: Box<Self>, project_fs: &ProjectRoot) -> anyhow::Result<()> {
        materialize_dirs_and_syms(self.entry.as_ref(), &project_fs.root().join(&self.path))?;

        Ok(())
    }
}

/// Materializes the entry at `dest`.
///
/// - `materialize_dirs_and_syms`: if `true`, materializes directories and
///   symlinks.
/// - `file_src`: takes the destination path of a file, and returns its
///   source path (where it should be copied from). If it returns [`None`],
///   the file is not materialized.
fn materialize<F, D>(
    entry: DirectoryEntry<&D, &ActionDirectoryMember>,
    dest: &AbsPath,
    materialize_dirs_and_syms: bool,
    mut file_src: F,
) -> anyhow::Result<()>
where
    F: FnMut(&AbsPath) -> Option<AbsPathBuf>,
    D: ActionDirectory,
{
    let mut dest = dest.to_path_buf();
    if materialize_dirs_and_syms {
        // create the directory where we'll materialize the entry
        if let Some(parent) = dest.parent() {
            fs_util::create_dir_all(parent)?;
        }
    }
    materialize_recursively(entry, &mut dest, materialize_dirs_and_syms, &mut file_src)
}

/// Materializes the directories and symlinks of an entry at `dest`. Files
/// are not materialized.
pub(crate) fn materialize_dirs_and_syms<P, D>(
    entry: DirectoryEntry<&D, &ActionDirectoryMember>,
    dest: P,
) -> anyhow::Result<()>
where
    P: AsRef<AbsPath>,
    D: ActionDirectory,
{
    materialize(entry, dest.as_ref(), true, |_: &AbsPath| None)
}

/// Materializes the files of an the entry rooted at `dest`.
///
/// Files are copied from `src`. In other words, if a file would be
/// materialized at `dest/p`, then it's copied from `src/p`.
pub(crate) fn materialize_files<P, D>(
    entry: DirectoryEntry<&D, &ActionDirectoryMember>,
    src: P,
    dest: P,
) -> anyhow::Result<()>
where
    P: AsRef<AbsPath>,
    D: ActionDirectory,
{
    let src = src.as_ref();
    let dest = dest.as_ref();
    let file_src = |d: &AbsPath| {
        // It's safe to unwrap because `materialize_impl` always gives us a
        // path inside `dest`.
        let subpath = d.strip_prefix(dest).unwrap();
        if subpath.as_str().is_empty() {
            // `dest` itself is a file
            Some(src.to_buf())
        } else {
            Some(src.join(subpath))
        }
    };
    materialize(entry, dest, false, file_src)
}

/// Materializes the files of an entry rooted at `dest`.
///
/// For a file at path `file_dest` in the entry, if `file_dest` exists in
/// `srcs` with value `file_src`, the file is copied from `file_src` to
/// `file_dest`. It's then removed from `srcs`.
fn _materialize_files_from_map<P, D>(
    entry: DirectoryEntry<&D, &ActionDirectoryMember>,
    srcs: &mut HashMap<AbsPathBuf, AbsPathBuf>,
    dest: P,
) -> anyhow::Result<()>
where
    P: AsRef<AbsPath>,
    D: ActionDirectory,
{
    let file_src = |d: &AbsPath| srcs.remove(d);
    materialize(entry, dest.as_ref(), false, file_src)
}

fn materialize_recursively<F, D>(
    entry: DirectoryEntry<&D, &ActionDirectoryMember>,
    dest: &mut PathBuf,
    materialize_dirs_and_syms: bool,
    file_src: &mut F,
) -> anyhow::Result<()>
where
    F: FnMut(&AbsPath) -> Option<AbsPathBuf>,
    D: ActionDirectory + ?Sized,
{
    match entry {
        DirectoryEntry::Dir(d) => {
            if materialize_dirs_and_syms {
                fs_util::create_dir_all(&dest)?;
            }
            for (name, entry) in d.entries() {
                dest.push(name);
                materialize_recursively(entry, dest, materialize_dirs_and_syms, file_src)?;
                dest.pop();
            }
            Ok(())
        }
        DirectoryEntry::Leaf(ActionDirectoryMember::File(_)) => {
            if let Some(src) = file_src(AbsPath::unchecked_new(&dest)) {
                if fs_util::symlink_metadata(&dest).is_err() {
                    fs_util::copy(src, dest)?;
                }
            }
            Ok(())
        }
        DirectoryEntry::Leaf(ActionDirectoryMember::Symlink(s)) => {
            if materialize_dirs_and_syms && fs_util::symlink_metadata(&dest).is_err() {
                fs_util::symlink(s.target().as_str(), dest)?;
            }
            Ok(())
        }
        DirectoryEntry::Leaf(ActionDirectoryMember::ExternalSymlink(s)) => {
            if materialize_dirs_and_syms && fs_util::symlink_metadata(&dest).is_err() {
                fs_util::symlink(s.target(), dest)?;
            }
            Ok(())
        }
    }
}
