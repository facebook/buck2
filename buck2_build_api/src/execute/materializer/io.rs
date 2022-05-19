/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use buck2_core::fs::project::{ProjectFilesystem, ProjectRelativePathBuf};

use crate::{
    actions::{
        artifact_utils::materialize_dirs_and_syms,
        directory::{ActionDirectoryEntry, ActionSharedDirectory},
    },
    execute::blocking::IoRequest,
};

pub struct MaterializeTreeStructure {
    pub path: ProjectRelativePathBuf,
    pub entry: ActionDirectoryEntry<ActionSharedDirectory>,
}

impl IoRequest for MaterializeTreeStructure {
    fn execute(self: Box<Self>, project_fs: &ProjectFilesystem) -> anyhow::Result<()> {
        materialize_dirs_and_syms(
            self.entry.as_ref(),
            &project_fs.root.join_unnormalized(&self.path),
        )?;

        Ok(())
    }
}
