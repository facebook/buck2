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
use buck2_execute::artifact::artifact_dyn::ArtifactDyn;
use buck2_execute::artifact_utils::ArtifactValueBuilder;
use buck2_execute::artifact_value::ArtifactValue;
use buck2_execute::digest_config::HasDigestConfig;
use buck2_execute::directory::ActionDirectoryBuilder;
use buck2_execute::execute::blobs::ActionBlobs;
use buck2_execute::materialize::materializer::HasMaterializer;
use dashmap::DashSet;
use dice::DiceComputations;
use dice::DiceComputationsData;
use dice::UserComputationData;
use dice_futures::spawn::spawn_dropcancel;
use dupe::Dupe;
use futures::FutureExt;
use futures::future::BoxFuture;

use crate::actions::artifact::get_artifact_fs::GetArtifactFs;
use crate::actions::artifact::materializer::ArtifactMaterializer;
use crate::actions::execute::dice_data::GetReClient;
use crate::actions::impls::run_action_knobs::HasRunActionKnobs;
use crate::artifact_groups::ArtifactGroup;
use crate::artifact_groups::ArtifactGroupValues;
use crate::artifact_groups::calculation::ArtifactGroupCalculation;

/// Configuration for experimental faster materialization mode.
#[derive(Clone, Copy)]
pub struct MaterializerFastRolloutConfig {
    /// If true, spawn materialization futures early rather than running inline.
    pub spawn: bool,
    /// If true, use `tokio::task::unconstrained` when polling materialization futures.
    pub unconstrained: bool,
}

pub trait HasMaterializerFastRolloutConfig {
    fn get_materializer_fast_rollout_config(&self) -> MaterializerFastRolloutConfig;
}

impl HasMaterializerFastRolloutConfig for UserComputationData {
    fn get_materializer_fast_rollout_config(&self) -> MaterializerFastRolloutConfig {
        const DEFAULT: MaterializerFastRolloutConfig = MaterializerFastRolloutConfig {
            spawn: false,
            unconstrained: false,
        };
        *self
            .data
            .get::<MaterializerFastRolloutConfig>()
            .unwrap_or(&DEFAULT)
    }
}

fn maybe_spawned<T: Send + 'static>(
    data: &DiceComputationsData,
    should_spawn: bool,
    f: BoxFuture<'static, T>,
) -> impl Future<Output = T> {
    if should_spawn {
        spawn_dropcancel(
            move |_cancellations| f,
            &*data.per_transaction_data().spawner,
            data.per_transaction_data(),
        )
        .left_future()
    } else {
        f.right_future()
    }
}

pub async fn materialize_and_upload_artifact_group(
    ctx: &mut DiceComputations<'_>,
    artifact_group: &ArtifactGroup,
    contexts: MaterializationAndUploadContext,
    queue_tracker: &Arc<DashSet<BuildArtifact>>,
) -> buck2_error::Result<ArtifactGroupValues> {
    let config = ctx
        .per_transaction_data()
        .get_materializer_fast_rollout_config();
    let (values, _) = {
        let fut = ctx.try_compute2(
            |mut ctx| {
                let group = &artifact_group;
                async move {
                    materialize_artifact_group(
                        &mut ctx,
                        config.spawn,
                        group,
                        contexts.0,
                        queue_tracker,
                    )
                    .await
                }
                .boxed()
            },
            |mut ctx| {
                let group = &artifact_group;
                async move {
                    match contexts.1 {
                        UploadContext::Skip => Ok(()),
                        UploadContext::Upload => ensure_uploaded(&mut ctx, group).await,
                    }
                }
                .boxed()
            },
        );

        if config.unconstrained {
            tokio::task::unconstrained(fut).await?
        } else {
            fut.await?
        }
    };

    Ok(values)
}

async fn materialize_artifact_group(
    ctx: &mut DiceComputations<'_>,
    should_spawn: bool,
    artifact_group: &ArtifactGroup,
    materialization_context: MaterializationContext,
    queue_tracker: &Arc<DashSet<BuildArtifact>>,
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

                    async move {
                        let (data, artifact_fs, materializer) = &*shared_data;

                        let configuration_hash_path = artifact_fs
                            .resolve_build_configuration_hash_path(&artifact.get_path())?;

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
                            .declare_copy(configuration_hash_path.clone(), symlink_value, Vec::new())
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
                        )
                        .await
                        .buck_error_context("Failed to materialize artifacts")?;
                        buck2_error::Ok(())
                    }
                };
                materialize_futs.push(maybe_spawned(&data, should_spawn, fut.boxed()));
            }
        }

        buck2_util::future::try_join_all(materialize_futs).await?;
    }

    Ok(values)
}

async fn ensure_uploaded(
    ctx: &mut DiceComputations<'_>,
    artifact_group: &ArtifactGroup,
) -> buck2_error::Result<()> {
    let digest_config = ctx.global_data().get_digest_config();
    let artifact_fs = ctx.get_artifact_fs().await?;
    let mut dir = ActionDirectoryBuilder::empty();
    let values = ctx.ensure_artifact_group(&artifact_group).await?;
    for (artifact, value) in values.iter() {
        let path = artifact.resolve_path(
            &artifact_fs,
            if artifact.has_content_based_path() {
                Some(value.content_based_path_hash())
            } else {
                None
            }
            .as_ref(),
        )?;
        buck2_execute::directory::insert_artifact(&mut dir, path, &value)?;
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
        .map(|v| RemoteExecutorUseCase::new((*v).to_owned()))
        .unwrap_or_else(RemoteExecutorUseCase::buck2_default);
    ctx.per_transaction_data()
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
        .await?;

    Ok(())
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
pub struct MaterializationQueueTrackerHolder(Arc<DashSet<BuildArtifact>>);

pub trait HasMaterializationQueueTracker {
    fn init_materialization_queue_tracker(&mut self);

    fn get_materialization_queue_tracker(&self) -> Arc<DashSet<BuildArtifact>>;
}

impl HasMaterializationQueueTracker for UserComputationData {
    fn init_materialization_queue_tracker(&mut self) {
        self.data
            .set(MaterializationQueueTrackerHolder(Arc::new(DashSet::new())));
    }

    fn get_materialization_queue_tracker(&self) -> Arc<DashSet<BuildArtifact>> {
        self.data
            .get::<MaterializationQueueTrackerHolder>()
            .expect("MaterializationQueueTracker should be set")
            .0
            .dupe()
    }
}
