/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use buck2_common::file_ops::FileMetadata;
use buck2_core::directory::DirectoryEntry;
use gazebo::dupe::Dupe;

use crate::artifact::fs::ArtifactFs;
use crate::directory::ActionDirectoryBuilder;
use crate::directory::ActionDirectoryMember;
use crate::execute::request::CommandExecutionInput;

pub fn inputs_directory(
    inputs: &[CommandExecutionInput],
    fs: &ArtifactFs,
) -> anyhow::Result<ActionDirectoryBuilder> {
    let mut builder = ActionDirectoryBuilder::empty();
    for input in inputs {
        match input {
            CommandExecutionInput::Artifact(group) => {
                group.add_to_directory(&mut builder, fs)?;
            }
            CommandExecutionInput::ActionMetadata(metadata) => {
                let path = fs.buck_out_path_resolver().resolve_gen(&metadata.path);
                builder.insert(
                    &path,
                    DirectoryEntry::Leaf(ActionDirectoryMember::File(FileMetadata {
                        digest: metadata.digest.dupe(),
                        is_executable: false,
                    })),
                )?;
            }
        };
    }
    Ok(builder)
}
