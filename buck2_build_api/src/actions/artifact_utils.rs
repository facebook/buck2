/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::borrow::Cow;
use std::collections::HashMap;
use std::path::PathBuf;
use std::sync::Arc;

use anyhow::Context;
use buck2_core::directory::DirectoryEntry;
use buck2_core::fs::anyhow as fs;
use buck2_core::fs::paths::AbsPath;
use buck2_core::fs::paths::AbsPathBuf;
use buck2_core::fs::paths::RelativePath;
use buck2_core::fs::paths::RelativePathBuf;
use buck2_core::fs::project::ProjectFilesystem;
use buck2_core::fs::project::ProjectRelativePath;
use gazebo::prelude::*;

use crate::actions::artifact::ArtifactValue;
use crate::actions::directory::extract_artifact_value;
use crate::actions::directory::insert_artifact;
use crate::actions::directory::insert_entry;
use crate::actions::directory::new_symlink;
use crate::actions::directory::relativize_directory;
use crate::actions::directory::ActionDirectory;
use crate::actions::directory::ActionDirectoryBuilder;
use crate::actions::directory::ActionDirectoryEntry;
use crate::actions::directory::ActionDirectoryMember;
use crate::actions::directory::ActionSharedDirectory;
use crate::actions::directory::INTERNER;

pub struct ArtifactValueBuilder<'a> {
    /// Only used to relativize paths; no disk operations performed!
    project_fs: &'a ProjectFilesystem,
    builder: ActionDirectoryBuilder,
}

impl<'a> ArtifactValueBuilder<'a> {
    pub fn new(project_fs: &'a ProjectFilesystem) -> Self {
        Self {
            project_fs,
            builder: ActionDirectoryBuilder::empty(),
        }
    }

    pub fn add_entry(
        &mut self,
        path: &ProjectRelativePath,
        entry: ActionDirectoryEntry<ActionDirectoryBuilder>,
    ) -> anyhow::Result<()> {
        insert_entry(&mut self.builder, path.as_ref(), entry)
    }

    /// Inserts an input to the tree, which will be required when following
    /// symlinks to calculate the `deps` of the `ArtifactValue`.
    pub fn add_input_value(
        &mut self,
        path: &ProjectRelativePath,
        value: &ArtifactValue,
    ) -> anyhow::Result<()> {
        insert_artifact(&mut self.builder, path.as_ref(), value)
    }

    /// Takes an input `src_value`, adds it to the builder at `src`. Then
    /// creates a symlink to `src`, adds it to the builder at `dest` and
    /// returns it.
    pub fn add_symlinked(
        &mut self,
        src_value: &ArtifactValue,
        src: &ProjectRelativePath,
        dest: &ProjectRelativePath,
    ) -> anyhow::Result<()> {
        insert_artifact(&mut self.builder, src.as_ref(), src_value)?;
        let entry = DirectoryEntry::Leaf(new_symlink(&self.project_fs.relative_path(src, dest)));
        self.builder.insert(dest, entry)?;
        Ok(())
    }

    /// Takes an input `src_value`, adds it to the builder at `src`. Then
    /// creates a copy of `src_value`'s entry relativized as if it had been
    /// copied from `src` to `dest`, adds it to the builder at `dest` and
    /// returns it.
    pub fn add_copied(
        &mut self,
        src_value: &ArtifactValue,
        src: &ProjectRelativePath,
        dest: &ProjectRelativePath,
    ) -> anyhow::Result<ActionDirectoryEntry<ActionSharedDirectory>> {
        insert_artifact(&mut self.builder, src.as_ref(), src_value)?;

        let entry = match src_value.entry() {
            DirectoryEntry::Dir(directory) => {
                let mut builder = directory.dupe().into_builder();
                relativize_directory(&mut builder, src.as_ref(), dest.as_ref())?;
                DirectoryEntry::Dir(builder.fingerprint())
            }
            DirectoryEntry::Leaf(ActionDirectoryMember::Symlink(s)) => {
                let reldest = self
                    .project_fs
                    .relative_path(src.parent().context("Symlink has no dir parent")?, dest);
                // RelativePathBuf converts platform specific path separators.
                let reldest = if cfg!(windows) {
                    Cow::Owned(RelativePathBuf::from_path(&reldest)?)
                } else {
                    Cow::Borrowed(RelativePath::from_path(&reldest)?)
                };
                let s = s.relativized(reldest);
                DirectoryEntry::Leaf(ActionDirectoryMember::Symlink(Arc::new(s)))
            }
            DirectoryEntry::Leaf(ActionDirectoryMember::ExternalSymlink(s)) => {
                DirectoryEntry::Leaf(ActionDirectoryMember::ExternalSymlink(s.with_full_target()))
            }
            DirectoryEntry::Leaf(ActionDirectoryMember::File(f)) => {
                DirectoryEntry::Leaf(ActionDirectoryMember::File(f.dupe()))
            }
        };

        let entry = entry.map_dir(|d| d.shared(&*INTERNER));

        self.builder
            .insert(dest, entry.dupe().map_dir(|d| d.into_builder()))?;

        Ok(entry)
    }

    /// Builds the `ArtifactValue`. Since `self.builder` is rooted at the
    /// project root, `output` must be passed to specify the path of the value
    /// being built.
    pub fn build(&self, output: &ProjectRelativePath) -> anyhow::Result<ArtifactValue> {
        match extract_artifact_value(&self.builder, output.as_ref())? {
            Some(v) => Ok(v),
            None => {
                tracing::debug!("Extracting {} produces empty directory!", output);
                Ok(ArtifactValue::empty_dir())
            }
        }
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
            fs::create_dir_all(parent)?;
        }
    }
    materialize_recursively(entry, &mut dest, materialize_dirs_and_syms, &mut file_src)
}

/// Materializes the directories and symlinks of an entry at `dest`. Files
/// are not materialized.
pub fn materialize_dirs_and_syms<P, D>(
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
pub fn materialize_files<P, D>(
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
            Some(src.join_unnormalized(subpath))
        }
    };
    materialize(entry, dest, false, file_src)
}

/// Materializes the files of an entry rooted at `dest`.
///
/// For a file at path `file_dest` in the entry, if `file_dest` exists in
/// `srcs` with value `file_src`, the file is copied from `file_src` to
/// `file_dest`. It's then removed from `srcs`.
pub fn materialize_files_from_map<P, D>(
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
                fs::create_dir_all(&dest)?;
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
                if fs::symlink_metadata(&dest).is_err() {
                    fs::copy(src, dest)?;
                }
            }
            Ok(())
        }
        DirectoryEntry::Leaf(ActionDirectoryMember::Symlink(s)) => {
            if materialize_dirs_and_syms && fs::symlink_metadata(&dest).is_err() {
                fs::symlink(s.target().as_str(), dest)?;
            }
            Ok(())
        }
        DirectoryEntry::Leaf(ActionDirectoryMember::ExternalSymlink(s)) => {
            if materialize_dirs_and_syms && fs::symlink_metadata(&dest).is_err() {
                fs::symlink(s.target(), dest)?;
            }
            Ok(())
        }
    }
}

#[cfg(test)]
mod tests {
    use buck2_core::fs::project::ProjectFilesystemTemp;

    use super::*;
    use crate::actions::directory::Symlink;

    fn path<'a>(s: &'a str) -> &'a ProjectRelativePath {
        ProjectRelativePath::new(s).unwrap()
    }

    fn get_symlink(s: &str) -> Arc<Symlink> {
        Arc::new(Symlink::new(s.into()))
    }

    fn get_symlink_artifact_value(s: &str) -> ArtifactValue {
        let symlink = DirectoryEntry::Leaf(ActionDirectoryMember::Symlink(get_symlink(s)));
        ArtifactValue::new(symlink, None)
    }

    #[test]
    fn copy_relativized_symlink() -> anyhow::Result<()> {
        // /
        // |-d1/
        // | |-d2/
        // | | |-d3/
        // | | |  |-d4/
        // | | |  | |-link -> ../../../d6/target
        // | |-d5/
        // | | |-new_link
        // |-d6/
        // | |-target

        let entry = {
            let fs = ProjectFilesystemTemp::new().unwrap();
            let mut builder = ArtifactValueBuilder::new(fs.path());
            builder.add_copied(
                &get_symlink_artifact_value("../../../d6/target"),
                path("d1/d2/d3/d4/link"),
                path("d1/d5/new_link"),
            )?
        };

        let new_symlink = match entry.as_ref() {
            DirectoryEntry::Leaf(ActionDirectoryMember::Symlink(s)) => s,
            _ => panic!("Symlink type is expected!"),
        };

        assert_eq!(
            new_symlink,
            &get_symlink("../d6/target"),
            "Symlinks are different"
        );

        Ok(())
    }
}
