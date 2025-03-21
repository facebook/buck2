/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use buck2_artifact::artifact::build_artifact::BuildArtifact;
use buck2_build_api::actions::execute::action_executor::ActionOutputs;
use buck2_build_api::actions::ActionExecutionCtx;
use buck2_common::file_ops::FileDigestConfig;
use buck2_core::fs::project_rel_path::ProjectRelativePathBuf;
use buck2_execute::artifact_value::ArtifactValue;
use buck2_execute::directory::INTERNER;
use buck2_execute::entry::build_entry_from_disk;
use buck2_execute::materialize::materializer::CopiedArtifact;
use dupe::Dupe;

/// Declares a copy materialization to copy the output BuildArtifact to the
/// offline cache for use in an offline build. Returns the project-relative path
/// to the offline cached file.
pub(crate) async fn declare_copy_to_offline_output_cache(
    ctx: &mut dyn ActionExecutionCtx,
    output: &BuildArtifact,
    value: ArtifactValue,
) -> buck2_error::Result<ProjectRelativePathBuf> {
    let build_path = ctx.fs().resolve_build(output.get_path());
    let offline_cache_path = ctx
        .fs()
        .resolve_offline_output_cache_path(output.get_path());
    declare_copy_materialization(ctx, build_path, offline_cache_path.clone(), value).await?;

    Ok(offline_cache_path)
}

/// Declares a copy materialization to copy the offline-cached BuildArtifact
/// output to the build output; effectively the inverse of `declare_copy_to_offline_output_cache`.
/// Used only during offline builds to ensure buck does not make any network
/// requests.
pub(crate) async fn declare_copy_from_offline_cache(
    ctx: &mut dyn ActionExecutionCtx,
    output: &BuildArtifact,
) -> buck2_error::Result<ActionOutputs> {
    let offline_cache_path = ctx
        .fs()
        .resolve_offline_output_cache_path(output.get_path());

    let (value, _hashing_time) = build_entry_from_disk(
        ctx.fs().fs().resolve(&offline_cache_path),
        FileDigestConfig::build(ctx.digest_config().cas_digest_config()),
        ctx.blocking_executor(),
        ctx.fs().fs().root(),
    )
    .await?;

    let entry = value
        .ok_or_else(|| {
            buck2_error::buck2_error!(
                buck2_error::ErrorTag::Tier0,
                "Missing offline cache entry: `{}`",
                offline_cache_path
            )
        })?
        .map_dir(|dir| {
            dir.fingerprint(ctx.digest_config().as_directory_serializer())
                .shared(&*INTERNER)
        });
    let value = ArtifactValue::from(entry);

    let build_path = ctx.fs().resolve_build(output.get_path());
    declare_copy_materialization(ctx, offline_cache_path, build_path, value.dupe()).await?;

    Ok(ActionOutputs::from_single(output.get_path().dupe(), value))
}

/// Declares a generic copy materialization from src to dest.
async fn declare_copy_materialization(
    ctx: &dyn ActionExecutionCtx,
    src: ProjectRelativePathBuf,
    dest: ProjectRelativePathBuf,
    value: ArtifactValue,
) -> buck2_error::Result<()> {
    let immutable_entry = value.entry().dupe().map_dir(|d| d.as_immutable());
    ctx.materializer()
        .declare_copy(
            dest.clone(),
            value,
            vec![CopiedArtifact::new(src, dest, immutable_entry)],
            ctx.cancellation_context(),
        )
        .await
}
