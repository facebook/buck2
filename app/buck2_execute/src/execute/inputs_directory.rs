/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use buck2_common::file_ops::metadata::FileMetadata;
use buck2_core::fs::artifact_path_resolver::ArtifactFs;
use buck2_directory::directory::directory_ref::DirectoryRef;
use buck2_directory::directory::entry::DirectoryEntry;
use dupe::Dupe;

use crate::directory::ActionDirectoryBuilder;
use crate::directory::ActionDirectoryMember;
use crate::execute::request::CommandExecutionInput;

pub fn inputs_directory(
    inputs: &[CommandExecutionInput],
    fs: &ArtifactFs,
) -> buck2_error::Result<ActionDirectoryBuilder> {
    let mut builder = ActionDirectoryBuilder::empty();
    for input in inputs {
        match input {
            CommandExecutionInput::Artifact(group) => {
                group.add_to_directory(&mut builder, fs)?;
            }
            CommandExecutionInput::ActionMetadata(metadata) => {
                let path = fs
                    .buck_out_path_resolver()
                    .resolve_gen(&metadata.path, Some(&metadata.content_hash))?;
                builder.insert(
                    &path,
                    DirectoryEntry::Leaf(ActionDirectoryMember::File(FileMetadata {
                        digest: metadata.digest.dupe(),
                        is_executable: false,
                    })),
                )?;
            }
            CommandExecutionInput::ScratchPath(path) => {
                let path = fs.buck_out_path_resolver().resolve_scratch(path)?;
                builder.insert(&path, DirectoryEntry::Dir(ActionDirectoryBuilder::empty()))?;
            }
            CommandExecutionInput::IncrementalRemoteOutput(path, entry) => match entry {
                DirectoryEntry::Dir(d) => {
                    builder.insert(path, DirectoryEntry::Dir(d.to_builder()))?;
                }
                DirectoryEntry::Leaf(m) => {
                    builder.insert(path, DirectoryEntry::Leaf(m.dupe()))?;
                }
            },
        };
    }
    Ok(builder)
}
