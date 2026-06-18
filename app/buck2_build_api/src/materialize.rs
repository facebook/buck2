/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use std::sync::Arc;

use buck2_artifact::artifact::artifact_type::BaseArtifactKind;
use buck2_artifact::artifact::build_artifact::BuildArtifact;
use buck2_build_signals::env::WaitingCategory;
use buck2_build_signals::env::WaitingData;
use buck2_cli_proto::build_request::Materializations;
use buck2_cli_proto::build_request::Uploads;
use buck2_common::legacy_configs::dice::HasLegacyConfigs;
use buck2_common::legacy_configs::key::BuckconfigKeyRef;
use buck2_common::legacy_configs::view::LegacyBuckConfigView;
use buck2_core::execution_types::executor_config::RemoteExecutorUseCase;
use buck2_core::fs::project_rel_path::ProjectRelativePath;
use buck2_error::BuckErrorContext;
use buck2_error::ErrorTag;
use buck2_execute::artifact::artifact_dyn::ArtifactDyn;
use buck2_execute::artifact_utils::ArtifactValueBuilder;
use buck2_execute::artifact_value::ArtifactValue;
use buck2_execute::digest_config::HasDigestConfig;
use buck2_execute::directory::ActionDirectoryBuilder;
use buck2_execute::execute::blobs::ActionBlobs;
use buck2_execute::materialize::materializer::HasMaterializer;
use buck2_hash::BuckDashSet;
use dice::DiceComputations;
use dice::UserComputationData;
use dice_futures::spawn::spawn_dropcancel;
use dupe::Dupe;
use futures::FutureExt;

use crate::actions::artifact::get_artifact_fs::GetArtifactFs;
use crate::actions::artifact::materializer::ArtifactMaterializer;
use crate::actions::execute::dice_data::GetReClient;
use crate::actions::impls::run_action_knobs::HasRunActionKnobs;
use crate::actions::rewind::ActionRewindRequest;
use crate::artifact_groups::ArtifactGroup;
use crate::artifact_groups::ArtifactGroupValues;
use crate::artifact_groups::calculation::ArtifactGroupCalculation;

pub async fn materialize_and_upload_artifact_group(
    ctx: &mut DiceComputations<'_>,
    artifact_group: &ArtifactGroup,
    contexts: MaterializationAndUploadContext,
    queue_tracker: &Arc<BuckDashSet<BuildArtifact>>,
) -> buck2_error::Result<ArtifactGroupValues> {
    let (values, _) = {
        let fut = ctx.try_compute2(
            |ctx| {
                let group = artifact_group;
                async move {
                    materialize_artifact_group(ctx, group, contexts.0, queue_tracker).await
                }
                .boxed()
            },
            |ctx| {
                let group = artifact_group;
                async move {
                    match contexts.1 {
                        UploadContext::Skip => Ok(()),
                        UploadContext::Upload => {
                            ensure_uploaded(ctx, group, contexts.0, queue_tracker).await
                        }
                    }
                }
                .boxed()
            },
        );

        tokio::task::unconstrained(fut).await?
    };

    Ok(values)
}

async fn materialize_artifact_group(
    ctx: &mut DiceComputations<'_>,
    artifact_group: &ArtifactGroup,
    materialization_context: MaterializationContext,
    queue_tracker: &Arc<BuckDashSet<BuildArtifact>>,
) -> buck2_error::Result<ArtifactGroupValues> {
    let values = ctx.ensure_artifact_group(artifact_group).await?;

    let mut waiting_data = WaitingData::new();

    if let MaterializationContext::Materialize { force } = materialization_context {
        waiting_data.start_waiting_category_now(WaitingCategory::MaterializerPrepare);
        let artifact_fs = ctx.get_artifact_fs().await?;
        let digest_config = ctx.global_data().get_digest_config();

        let data = ctx.data();
        let shared_data = Arc::new((
            data.dupe(),
            artifact_fs.clone(),
            ctx.per_transaction_data().get_materializer(),
        ));

        {
            let mut materialize_futs = Vec::new();

            for (artifact, value) in values.iter() {
                if let BaseArtifactKind::Build(artifact) = artifact.as_parts().0 {
                    if !queue_tracker.insert(artifact.dupe()) {
                        // We've already requested this artifact, no use requesting it again.
                        continue;
                    }

                    let fut = {
                        let waiting_data = waiting_data.clone();
                        let artifact = artifact.dupe();
                        let value = value.dupe();
                        let shared_data = shared_data.dupe();
                        let artifact_group = artifact_group.dupe();

                        async move {
                            let (data, artifact_fs, materializer) = &*shared_data;

                            let configuration_hash_path = artifact_fs
                                .resolve_build_configuration_hash_path(artifact.get_path())?;

                            if artifact.get_path().is_content_based_path() {
                                let content_based_path = artifact_fs.resolve_build(
                                    artifact.get_path(),
                                    Some(&value.content_based_path_hash()),
                                )?;
                                let mut builder =
                                    ArtifactValueBuilder::new(artifact_fs.fs(), digest_config);
                                builder.add_symlinked(
                                    // The materializer doesn't care about the `src_value`.
                                    &ArtifactValue::dir(digest_config.empty_directory()),
                                    content_based_path,
                                    &configuration_hash_path,
                                )?;
                                let symlink_value = builder.build(&configuration_hash_path)?;

                                materializer
                            .declare_copy(configuration_hash_path.clone(), symlink_value, Vec::new(), None)
                            .await
                            .buck_error_context(
                                "Failed to declare configuration path to content-based path symlinks",
                            )?;
                            }

                            data.try_materialize_requested_artifact(
                                &artifact,
                                waiting_data,
                                force,
                                configuration_hash_path,
                                &artifact_group,
                            )
                            .await
                            .buck_error_context("Failed to materialize artifacts")?;
                            buck2_error::Ok(())
                        }
                    };
                    materialize_futs.push(spawn_dropcancel(
                        move |_cancellations| fut.boxed(),
                        &*data.per_transaction_data().spawner,
                        data.per_transaction_data(),
                    ));
                }
            }

            match buck2_util::future::try_join_all(materialize_futs).await {
                Ok(_) => {}
                Err(error) if error.tags().contains(&ErrorTag::ReNotFound) => {
                    remove_materialization_queue_entries(&values, queue_tracker);
                    return Err(match artifact_group_values_rewind_request(&values) {
                        Some(request) => error.context(request),
                        None => error,
                    });
                }
                Err(error) => return Err(error),
            }
        }
    }

    Ok(values)
}

fn artifact_group_values_rewind_request(
    values: &ArtifactGroupValues,
) -> Option<ActionRewindRequest> {
    let mut producers = Vec::new();

    for (artifact, _) in values.iter() {
        if let BaseArtifactKind::Build(artifact) = artifact.as_parts().0 {
            if !producers.iter().any(|key| key == artifact.key()) {
                producers.push(artifact.key().dupe());
            }
        }
    }

    if producers.is_empty() {
        return None;
    }

    for producer in &producers {
        tracing::info!(
            producer_action = %producer,
            "Requesting DICE graph rewind after a requested output disappeared from remote CAS"
        );
    }

    Some(ActionRewindRequest::new(producers))
}

fn remove_materialization_queue_entries(
    values: &ArtifactGroupValues,
    queue_tracker: &Arc<BuckDashSet<BuildArtifact>>,
) {
    for (artifact, _) in values.iter() {
        if let BaseArtifactKind::Build(artifact) = artifact.as_parts().0 {
            queue_tracker.remove(artifact);
        }
    }
}

async fn ensure_uploaded(
    ctx: &mut DiceComputations<'_>,
    artifact_group: &ArtifactGroup,
    materialization_context: MaterializationContext,
    queue_tracker: &Arc<BuckDashSet<BuildArtifact>>,
) -> buck2_error::Result<()> {
    let digest_config = ctx.global_data().get_digest_config();
    let artifact_fs = ctx.get_artifact_fs().await?;
    let mut dir = ActionDirectoryBuilder::empty();
    let values = ctx.ensure_artifact_group(artifact_group).await?;
    for (artifact, value) in values.iter() {
        let path = artifact.resolve_path(
            &artifact_fs,
            if artifact.path_resolution_requires_artifact_value() {
                Some(value.content_based_path_hash())
            } else {
                None
            }
            .as_ref(),
        )?;
        buck2_execute::directory::insert_artifact(&mut dir, path, value)?;
    }
    let dir = dir.fingerprint(digest_config.as_directory_serializer());
    let re_use_case = ctx
        .get_legacy_root_config_on_dice()
        .await
        .and_then(|cfg| {
            cfg.view(ctx).get(BuckconfigKeyRef {
                section: "build",
                property: "default_remote_execution_use_case",
            })
        })
        .ok()
        .flatten()
        .map_or_else(RemoteExecutorUseCase::buck2_default, |v| {
            RemoteExecutorUseCase::new((*v).to_owned())
        });
    match ctx
        .per_transaction_data()
        .get_re_client()
        .with_use_case(re_use_case)
        .upload(
            artifact_fs.fs(),
            &ctx.per_transaction_data().get_materializer(),
            &ActionBlobs::new(digest_config),
            ProjectRelativePath::empty(),
            &dir,
            None,
            digest_config,
            ctx.per_transaction_data()
                .get_run_action_knobs()
                .deduplicate_get_digests_ttl_calls,
        )
        .await
    {
        Ok(_) => Ok(()),
        Err(error) if error.tags().contains(&ErrorTag::ReNotFound) => {
            // Upload and materialization run through try_compute2. If upload
            // requests a rewind first, materialization can be dropped before
            // it removes the artifacts it already queued.
            if matches!(
                materialization_context,
                MaterializationContext::Materialize { .. }
            ) {
                remove_materialization_queue_entries(&values, queue_tracker);
            }
            Err(match artifact_group_values_rewind_request(&values) {
                Some(request) => error.context(request),
                None => error,
            })
        }
        Err(error) => Err(error),
    }
}

#[derive(Clone, Dupe, Copy)]
enum MaterializationContext {
    Skip,
    Materialize {
        /// Whether we should force the materialization of requested artifacts, or defer to the
        /// config.
        force: bool,
    },
}
impl From<Materializations> for MaterializationContext {
    fn from(value: Materializations) -> Self {
        match value {
            Materializations::Skip => MaterializationContext::Skip,
            Materializations::Default => MaterializationContext::Materialize { force: false },
            Materializations::Materialize => MaterializationContext::Materialize { force: true },
        }
    }
}

#[derive(Clone, Dupe, Copy)]
enum UploadContext {
    Skip,
    Upload,
}
impl From<Uploads> for UploadContext {
    fn from(value: Uploads) -> Self {
        match value {
            Uploads::Always => UploadContext::Upload,
            Uploads::Never => UploadContext::Skip,
        }
    }
}

#[derive(Clone, Dupe, Copy)]
pub struct MaterializationAndUploadContext(MaterializationContext, UploadContext);
impl MaterializationAndUploadContext {
    pub fn skip() -> Self {
        Self(MaterializationContext::Skip, UploadContext::Skip)
    }
    pub fn materialize() -> Self {
        Self(
            MaterializationContext::Materialize { force: true },
            UploadContext::Skip,
        )
    }
}
impl From<(Materializations, Uploads)> for MaterializationAndUploadContext {
    fn from(value: (Materializations, Uploads)) -> Self {
        Self(value.0.into(), value.1.into())
    }
}

/// This map contains all the artifacts that we enqueued for materialization. This ensures
/// we don't enqueue the same thing more than once. Should be shared across work done
/// in a single DICE transaction.
pub struct MaterializationQueueTrackerHolder(Arc<BuckDashSet<BuildArtifact>>);

pub trait HasMaterializationQueueTracker {
    fn init_materialization_queue_tracker(&mut self);

    fn clear_materialization_queue_tracker(&self);

    fn get_materialization_queue_tracker(&self) -> Arc<BuckDashSet<BuildArtifact>>;
}

impl HasMaterializationQueueTracker for UserComputationData {
    fn init_materialization_queue_tracker(&mut self) {
        self.data.set(MaterializationQueueTrackerHolder(Arc::new(
            BuckDashSet::default(),
        )));
    }

    fn clear_materialization_queue_tracker(&self) {
        self.data
            .get::<MaterializationQueueTrackerHolder>()
            .expect("MaterializationQueueTracker should be set")
            .0
            .clear();
    }

    fn get_materialization_queue_tracker(&self) -> Arc<BuckDashSet<BuildArtifact>> {
        self.data
            .get::<MaterializationQueueTrackerHolder>()
            .expect("MaterializationQueueTracker should be set")
            .0
            .dupe()
    }
}

#[cfg(test)]
mod tests {
    use buck2_artifact::actions::key::ActionIndex;
    use buck2_artifact::artifact::artifact_type::Artifact;
    use buck2_artifact::artifact::artifact_type::testing::BuildArtifactTestingExt;
    use buck2_core::configuration::data::ConfigurationData;
    use buck2_core::target::configured_target_label::ConfiguredTargetLabel;
    use buck2_execute::digest_config::DigestConfig;

    use super::*;

    fn build_artifact(path: &str) -> BuildArtifact {
        let target =
            ConfiguredTargetLabel::testing_parse("cell//pkg:foo", ConfigurationData::testing_new());
        BuildArtifact::testing_new(target, path, ActionIndex::new(0))
    }

    #[test]
    fn remove_materialization_queue_entries_clears_build_artifacts() {
        let artifact = build_artifact("out");
        let values = ArtifactGroupValues::from_artifact(
            Artifact::from(artifact.dupe()),
            ArtifactValue::file(DigestConfig::testing_default().empty_file()),
        );
        let queue_tracker = Arc::new(BuckDashSet::default());
        queue_tracker.insert(artifact.dupe());

        remove_materialization_queue_entries(&values, &queue_tracker);

        assert!(!queue_tracker.contains(&artifact));
    }

    #[test]
    fn clear_materialization_queue_tracker_drops_all_entries() {
        let artifact = build_artifact("out");
        let mut data = UserComputationData::new();
        data.init_materialization_queue_tracker();
        let queue_tracker = data.get_materialization_queue_tracker();
        queue_tracker.insert(artifact.dupe());

        data.clear_materialization_queue_tracker();

        assert!(!queue_tracker.contains(&artifact));
    }
}
