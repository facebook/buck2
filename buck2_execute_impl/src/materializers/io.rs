/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use buck2_core::fs::project::ProjectRelativePathBuf;
use buck2_core::fs::project::ProjectRoot;
use buck2_execute::artifact_utils::materialize_dirs_and_syms;
use buck2_execute::directory::ActionDirectoryEntry;
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
