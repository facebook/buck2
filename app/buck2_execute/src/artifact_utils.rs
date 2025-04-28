/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::sync::Arc;

use buck2_core::fs::fs_util;
use buck2_core::fs::project::ProjectRoot;
use buck2_core::fs::project_rel_path::ProjectRelativePath;
use buck2_directory::directory::entry::DirectoryEntry;
use buck2_error::BuckErrorContext;
use dupe::Dupe;

use crate::artifact_value::ArtifactValue;
use crate::digest_config::DigestConfig;
use crate::directory::ActionDirectoryBuilder;
use crate::directory::ActionDirectoryEntry;
use crate::directory::ActionDirectoryMember;
use crate::directory::ActionSharedDirectory;
use crate::directory::INTERNER;
use crate::directory::extract_artifact_value;
use crate::directory::insert_artifact;
use crate::directory::insert_entry;
use crate::directory::new_symlink;
use crate::directory::relativize_directory;

pub struct ArtifactValueBuilder<'a> {
    /// Only used to relativize paths; no disk operations performed!
    project_fs: &'a ProjectRoot,
    builder: ActionDirectoryBuilder,
    digest_config: DigestConfig,
}

impl<'a> ArtifactValueBuilder<'a> {
    pub fn new(project_fs: &'a ProjectRoot, digest_config: DigestConfig) -> Self {
        Self {
            project_fs,
            builder: ActionDirectoryBuilder::empty(),
            digest_config,
        }
    }

    pub fn add_entry(
        &mut self,
        path: &ProjectRelativePath,
        entry: ActionDirectoryEntry<ActionDirectoryBuilder>,
    ) -> buck2_error::Result<()> {
        insert_entry(&mut self.builder, path, entry)
    }

    /// Inserts an input to the tree, which will be required when following
    /// symlinks to calculate the `deps` of the `ArtifactValue`.
    pub fn add_input_value(
        &mut self,
        path: &ProjectRelativePath,
        value: &ArtifactValue,
    ) -> buck2_error::Result<()> {
        insert_artifact(&mut self.builder, path, value)
    }

    /// Takes an input `src_value`, adds it to the builder at `src`. Then
    /// creates a symlink to `src`, adds it to the builder at `dest` and
    /// returns it.
    pub fn add_symlinked(
        &mut self,
        src_value: &ArtifactValue,
        src: &ProjectRelativePath,
        dest: &ProjectRelativePath,
    ) -> buck2_error::Result<()> {
        insert_artifact(&mut self.builder, src, src_value)?;
        let entry = DirectoryEntry::Leaf(new_symlink(self.project_fs.relative_path(src, dest))?);
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
    ) -> buck2_error::Result<ActionDirectoryEntry<ActionSharedDirectory>> {
        insert_artifact(&mut self.builder, src, src_value)?;

        let entry = match src_value.entry() {
            DirectoryEntry::Dir(directory) => {
                let mut builder = directory.dupe().into_builder();
                relativize_directory(&mut builder, src, dest)?;
                DirectoryEntry::Dir(
                    builder.fingerprint(self.digest_config.as_directory_serializer()),
                )
            }
            DirectoryEntry::Leaf(ActionDirectoryMember::Symlink(s)) => {
                // TODO: This seems like it normally shouldn't need to be normalizing anything.
                let reldest = self.project_fs.relative_path(
                    src.parent()
                        .buck_error_context("Symlink has no dir parent")?,
                    dest,
                );
                // RelativePathBuf converts platform specific path separators.
                let reldest = fs_util::relative_path_from_system(&reldest)?;
                let s = s.relativized(reldest);
                DirectoryEntry::Leaf(ActionDirectoryMember::Symlink(Arc::new(s)))
            }
            DirectoryEntry::Leaf(ActionDirectoryMember::ExternalSymlink(s)) => {
                DirectoryEntry::Leaf(ActionDirectoryMember::ExternalSymlink(
                    s.with_full_target()?,
                ))
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
    pub fn build(&self, output: &ProjectRelativePath) -> buck2_error::Result<ArtifactValue> {
        match extract_artifact_value(&self.builder, output, self.digest_config)? {
            Some(v) => Ok(v),
            None => {
                tracing::debug!("Extracting {} produces empty directory!", output);
                Ok(ArtifactValue::dir(self.digest_config.empty_directory()))
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use buck2_core::fs::project::ProjectRootTemp;

    use super::*;
    use crate::directory::Symlink;

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
    fn copy_relativized_symlink() -> buck2_error::Result<()> {
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
            let fs = ProjectRootTemp::new().unwrap();
            let mut builder = ArtifactValueBuilder::new(fs.path(), DigestConfig::testing_default());
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
