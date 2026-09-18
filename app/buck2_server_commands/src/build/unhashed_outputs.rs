/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use buck2_artifact::artifact::artifact_type::BaseArtifactKind;
use buck2_build_api::build::BuildProviderType;
use buck2_build_api::build::ProviderArtifacts;
use buck2_cli_proto::build_request::Materializations;
use buck2_core::fs::artifact_path_resolver::ArtifactFs;
use buck2_core::fs::project_rel_path::ProjectRelativePathBuf;
use buck2_error::BuckErrorContext;
use buck2_error::ErrorTag;
use buck2_execute::artifact_utils::ArtifactValueBuilder;
use buck2_execute::artifact_value::ArtifactValue;
use buck2_execute::digest_config::DigestConfig;
use buck2_execute::materialize::materializer::MaterializationPurpose;
use buck2_execute::materialize::materializer::Materializer;
use buck2_query::__derive_refs::indexmap::IndexMap;
use buck2_util::future::try_join_all;
use itertools::Itertools;
use tracing::info;

type UnhashedOutputLinks =
    IndexMap<ProjectRelativePathBuf, IndexMap<ProjectRelativePathBuf, ArtifactValue>>;

fn unhashed_output_links(
    provider_artifacts: Vec<ProviderArtifacts>,
    artifact_fs: &ArtifactFs,
) -> buck2_error::Result<UnhashedOutputLinks> {
    let mut unhashed_to_hashed: UnhashedOutputLinks = IndexMap::new();
    for provider_artifact in provider_artifacts {
        if !matches!(provider_artifact.provider_type, BuildProviderType::Default) {
            continue;
        }

        if let Ok((artifact, value)) = provider_artifact.values.iter().exactly_one()
            && let (BaseArtifactKind::Build(build), _projected_path) = artifact.as_parts()
            && let Some(unhashed_path) = artifact_fs.retrieve_unhashed_location(build.get_path())
        {
            let path = artifact_fs.resolve_build(
                build.get_path(),
                if build.get_path().is_content_based_path() {
                    Some(value.content_based_path_hash())
                } else {
                    None
                }
                .as_ref(),
            )?;
            unhashed_to_hashed
                .entry(unhashed_path)
                .or_default()
                .insert(path, value.clone());
        }
    }
    Ok(unhashed_to_hashed)
}

pub(crate) async fn create_unhashed_outputs_via_materializer(
    provider_artifacts: Vec<ProviderArtifacts>,
    artifact_fs: &ArtifactFs,
    digest_config: DigestConfig,
    materializer: &dyn Materializer,
    materializations: Materializations,
) -> buck2_error::Result<()> {
    create_unhashed_outputs_via_materializer_impl(
        provider_artifacts,
        artifact_fs,
        digest_config,
        materializer,
        materializations,
    )
    .await
    .tag(ErrorTag::UnhashedOutputSymlink)
    .with_buck_error_context(|| "while creating materializer-managed unhashed output symlinks")
}

async fn create_unhashed_outputs_via_materializer_impl(
    provider_artifacts: Vec<ProviderArtifacts>,
    artifact_fs: &ArtifactFs,
    digest_config: DigestConfig,
    materializer: &dyn Materializer,
    materializations: Materializations,
) -> buck2_error::Result<()> {
    let unhashed_to_hashed = unhashed_output_links(provider_artifacts, artifact_fs)?;
    let mut declarations = Vec::new();

    for (unhashed, hashed_set) in unhashed_to_hashed {
        if let Ok((hashed, _)) = hashed_set.iter().exactly_one() {
            let mut builder = ArtifactValueBuilder::new(artifact_fs.fs(), digest_config);
            builder.add_symlinked(
                &ArtifactValue::dir(digest_config.empty_directory()),
                hashed.clone(),
                &unhashed,
            )?;
            let symlink_value = builder.build(&unhashed)?;
            declarations.push((unhashed, symlink_value));
        } else {
            info!(
                "The following outputs have a conflicting unhashed path at {}: {:?}",
                unhashed, hashed_set
            );
        }
    }
    let unhashed_paths: Vec<_> = declarations.iter().map(|(path, _)| path.clone()).collect();
    try_join_all(
        declarations
            .into_iter()
            .map(|(path, value)| materializer.declare_copy(path, value, Vec::new())),
    )
    .await?;
    match materializations {
        Materializations::Skip => {}
        Materializations::Default => {
            try_join_all(
                unhashed_paths
                    .into_iter()
                    .map(|path| materializer.try_materialize_final_artifact(path)),
            )
            .await?;
        }
        Materializations::Materialize => {
            materializer
                .ensure_materialized(unhashed_paths, MaterializationPurpose::FinalOutput)
                .await?;
        }
    }

    Ok(())
}
