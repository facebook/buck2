/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

//! Translation between buck core data and the test spec data types

use anyhow::Context;
use buck2_common::file_ops::FileDigest;
use buck2_core::cells::CellResolver;
use buck2_core::provider::label::ConfiguredProvidersLabel;
use buck2_data::ToProtoMessage;
use buck2_error::BuckErrorContext;
use buck2_execute::artifact_value::ArtifactValue;
use buck2_execute::directory::ActionDirectoryEntry;
use buck2_execute::directory::ActionDirectoryMember;
use buck2_execute::directory::ActionSharedDirectory;
use buck2_test_api::data::ConfiguredTarget;
use buck2_test_api::data::RemoteObject;

use crate::session::TestSession;

pub(crate) fn build_configured_target_handle(
    target: ConfiguredProvidersLabel,
    session: &TestSession,
    cell_resolver: &CellResolver,
) -> anyhow::Result<ConfiguredTarget> {
    let label = target.target().unconfigured();
    let cell = label.pkg().cell_name().to_string();
    let package = label.pkg().cell_relative_path().to_string();
    let target_name = if cfg!(fbcode_build) {
        label.name().to_string()
    } else {
        label.name().to_string() + &target.name().to_string()
    };
    let configuration = target.cfg().to_string();
    let package_project_relative_path = cell_resolver
        .resolve_path(label.pkg().as_cell_path())
        .buck_error_context_anyhow("Failed to resolve the project relative path of package")?;

    Ok(ConfiguredTarget {
        handle: session.register(target),
        cell,
        package,
        target: target_name,
        configuration,
        package_project_relative_path: package_project_relative_path.into(),
    })
}

pub(crate) fn convert_test_result(
    test_result: buck2_test_api::data::TestResult,
    session: &TestSession,
) -> anyhow::Result<buck2_data::TestResult> {
    let buck2_test_api::data::TestResult {
        name,
        status,
        msg,
        duration,
        details,
        target: test_target,
        max_memory_used_bytes,
    } = test_result;

    let test_target = session.get(test_target)?;

    Ok(buck2_data::TestResult {
        name,
        status: status.try_into().context("Invalid `status`")?,
        msg: msg.map(|msg| buck2_data::test_result::OptionalMsg { msg }),
        duration: duration.and_then(|d| d.try_into().ok()),
        details,
        target_label: Some(test_target.target().as_proto()),
        max_memory_used_bytes,
    })
}

/// Convert a named [`ArtifactValue`] into a test API's [`RemoteObject`].
///
/// Note that artifact trees containing symlinks currently can't be converted.
/// Test outputs are unlikely to contain symlinks, and if they do, we'd rather
/// fall back to materializing them on disk.
pub(crate) fn convert_artifact(name: String, artifact: &ArtifactValue) -> Option<RemoteObject> {
    // deps represent artifacts that symlinks depend on. Bail when present.
    if artifact.deps().is_some() {
        return None;
    }

    convert_directory_entry(name, artifact.entry())
}

fn convert_digest(digest: &FileDigest) -> buck2_test_api::data::CasDigest {
    let hash = format!("{}", digest.raw_digest());
    buck2_test_api::data::CasDigest {
        hash,
        size_bytes: digest.size() as i64,
    }
}

fn convert_directory_entry(
    name: String,
    entry: &ActionDirectoryEntry<ActionSharedDirectory>,
) -> Option<RemoteObject> {
    match entry {
        ActionDirectoryEntry::Leaf(
            ActionDirectoryMember::Symlink(..) | ActionDirectoryMember::ExternalSymlink(..),
        ) => None,
        ActionDirectoryEntry::Leaf(ActionDirectoryMember::File(f)) => {
            Some(RemoteObject::file(name, convert_digest(f.digest.data())))
        }
        ActionDirectoryEntry::Dir(dir) => {
            let children = dir
                .entries()
                .into_iter()
                .map(|(name, entry)| convert_directory_entry(name.to_string(), entry))
                .collect::<Option<Vec<_>>>()?;
            let digest = convert_digest(dir.fingerprint().data());
            Some(RemoteObject::dir(name, digest, children))
        }
    }
}
