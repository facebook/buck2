/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::fmt::Debug;
use std::fmt::Formatter;
use std::sync::Arc;

use allocative::Allocative;
use anyhow::Context;
use buck2_common::executor_config::PathSeparatorKind;
use buck2_common::result::SharedResult;
use buck2_common::result::ToSharedResultExt;
use buck2_core::provider::label::ConfiguredProvidersLabel;
use buck2_events::dispatch::console_message;
use buck2_execute::artifact::fs::ExecutorFs;
use buck2_node::compatibility::MaybeCompatible;
use cli_proto::build_request::Materializations;
use dashmap::mapref::entry::Entry;
use dashmap::DashMap;
use dice::DiceComputations;
use dice::UserComputationData;
use futures::future;
use gazebo::dupe::Dupe;
use tokio::sync::Mutex;

use crate::actions::artifact::build_artifact::BuildArtifact;
use crate::actions::artifact::materializer::ArtifactMaterializer;
use crate::actions::artifact::BaseArtifactKind;
use crate::artifact_groups::ArtifactGroup;
use crate::artifact_groups::ArtifactGroupValues;
use crate::calculation::Calculation;
use crate::interpreter::rule_defs::cmd_args::AbsCommandLineContext;
use crate::interpreter::rule_defs::cmd_args::CommandLineArgLike;
use crate::interpreter::rule_defs::cmd_args::SimpleCommandLineArtifactVisitor;
use crate::interpreter::rule_defs::provider::builtin::run_info::RunInfo;
use crate::interpreter::rule_defs::provider::collection::FrozenProviderCollectionValue;
use crate::interpreter::rule_defs::provider::test_provider::TestProvider;

/// The types of provider to build on the configured providers label
#[derive(Debug, Clone, Dupe, Allocative)]
pub enum BuildProviderType {
    Default,
    DefaultOther,
    Run,
    Test,
}

#[derive(Clone, Debug, Allocative)]
pub struct BuildTargetResult {
    pub outputs: Vec<SharedResult<ProviderArtifacts>>,
    pub providers: FrozenProviderCollectionValue,
    pub run_args: Option<Vec<String>>,
}

pub async fn build_configured_label(
    ctx: &DiceComputations,
    materialization_context: &MaterializationContext,
    providers_label: &ConfiguredProvidersLabel,
    providers_to_build: &ProvidersToBuild,
    skippable: bool,
) -> anyhow::Result<Option<BuildTargetResult>> {
    let artifact_fs = ctx.get_artifact_fs().await?;

    let (providers, outputs, run_args) = {
        // A couple of these objects aren't Send and so scope them here so async transform doesn't get concerned.
        let providers = match ctx.get_providers(providers_label).await? {
            MaybeCompatible::Incompatible(reason) => {
                if skippable {
                    console_message(reason.skipping_message(providers_label.target()));
                    return Ok(None);
                } else {
                    return Err(reason.to_err());
                }
            }
            MaybeCompatible::Compatible(v) => v,
        };

        // Important we use an an ordered collections, so the order matches the order the rule
        // author wrote.
        let mut outputs = Vec::new();
        // Providers that produced each output, in the order of outputs above. We use a separate collection
        // otherwise we'd build the same output twice when it's both in DefaultInfo and RunInfo
        let collection = providers.provider_collection();

        let mut run_args: Option<Vec<String>> = None;

        if providers_to_build.default {
            collection
                .default_info()
                .for_each_default_output_artifact_only(&mut |o| {
                    outputs.push((ArtifactGroup::Artifact(o), BuildProviderType::Default));
                    Ok(())
                })?;
        }
        if providers_to_build.default_other {
            collection
                .default_info()
                .for_each_default_output_other_artifacts_only(&mut |o| {
                    outputs.push((o, BuildProviderType::DefaultOther));
                    Ok(())
                })?;
            // TODO(marwhal): We can remove this once we migrate all other outputs to be handled with Artifacts directly
            collection.default_info().for_each_other_output(&mut |o| {
                outputs.push((o, BuildProviderType::DefaultOther));
                Ok(())
            })?;
        }
        if providers_to_build.run {
            if let Some(runinfo) = RunInfo::from_providers(providers.provider_collection()) {
                let mut artifact_visitor = SimpleCommandLineArtifactVisitor::new();
                runinfo.visit_artifacts(&mut artifact_visitor)?;
                for input in artifact_visitor.inputs {
                    outputs.push((input, BuildProviderType::Run));
                }
                // Produce arguments to run on a local machine.
                let path_separator = if cfg!(windows) {
                    PathSeparatorKind::Windows
                } else {
                    PathSeparatorKind::Unix
                };
                let executor_fs = ExecutorFs::new(&artifact_fs, path_separator);
                let mut cli = Vec::<String>::new();
                let mut ctx = AbsCommandLineContext::new(&executor_fs);
                runinfo.add_to_command_line(&mut cli, &mut ctx)?;
                run_args = Some(cli);
            }
        }
        if providers_to_build.tests {
            if let Some(test_provider) = <dyn TestProvider>::from_collection(collection) {
                let mut artifact_visitor = SimpleCommandLineArtifactVisitor::new();
                test_provider.visit_artifacts(&mut artifact_visitor)?;
                for input in artifact_visitor.inputs {
                    outputs.push((input, BuildProviderType::Test));
                }
            }
        }

        (providers, outputs, run_args)
    };

    if !skippable && outputs.is_empty() {
        console_message(format!(
            "target {} does not have any outputs: building it does nothing",
            providers_label.target()
        ));
    }

    let outputs = future::join_all(outputs.into_iter().map(|(o, provider_type)| async move {
        let values = materialize_artifact_group(ctx, &o, materialization_context)
            .await
            .shared_error()?;
        Ok(ProviderArtifacts {
            values,
            provider_type,
        })
    }))
    .await;
    Ok(Some(BuildTargetResult {
        outputs,
        providers,
        run_args,
    }))
}

#[derive(Clone, Allocative)]
pub struct ProviderArtifacts {
    pub values: ArtifactGroupValues,
    pub provider_type: BuildProviderType,
}

// what type of artifacts to build based on the provider it came from
#[derive(Default, Clone)]
pub struct ProvidersToBuild {
    pub default: bool,
    pub default_other: bool,
    pub run: bool,
    pub tests: bool,
}

impl Debug for ProviderArtifacts {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("ProviderArtifacts")
            .field("values", &self.values.iter().collect::<Vec<_>>())
            .field("provider_type", &self.provider_type)
            .finish()
    }
}

pub async fn materialize_artifact_group(
    ctx: &DiceComputations,
    artifact_group: &ArtifactGroup,
    materialization_context: &MaterializationContext,
) -> anyhow::Result<ArtifactGroupValues> {
    let values = ctx.ensure_artifact_group(artifact_group).await?;

    if let MaterializationContext::Materialize { map, force } = materialization_context {
        future::try_join_all(values.iter().filter_map(|(artifact, _value)| {
            match artifact.as_parts().0 {
                BaseArtifactKind::Build(artifact) => {
                    match map.entry(artifact.dupe()) {
                        Entry::Vacant(v) => {
                            // Ensure we won't request this artifact elsewhere, and proceed to request
                            // it.
                            v.insert(());
                        }
                        Entry::Occupied(..) => {
                            // We've already requested this artifact, no use requesting it again.
                            return None;
                        }
                    }

                    Some(ctx.try_materialize_requested_artifact(artifact, *force))
                }
                BaseArtifactKind::Source(..) => None,
            }
        }))
        .await
        .context("Failed to materialize artifacts")?;
    }

    Ok(values)
}

#[derive(Clone, Dupe)]
pub enum MaterializationContext {
    Skip,
    Materialize {
        /// This map contains all the artifacts that we enqueued for materialization. This ensures
        /// we don't enqueue the same thing more than once.
        map: Arc<DashMap<BuildArtifact, ()>>,
        /// Whether we should force the materialization of requested artifacts, or defer to the
        /// config.
        force: bool,
    },
}

pub trait ConvertMaterializationContext {
    fn from(self) -> MaterializationContext;
}

impl ConvertMaterializationContext for Materializations {
    fn from(self) -> MaterializationContext {
        match self {
            Materializations::Skip => MaterializationContext::Skip,
            Materializations::Default => MaterializationContext::Materialize {
                map: Arc::new(DashMap::new()),
                force: false,
            },
            Materializations::Materialize => MaterializationContext::Materialize {
                map: Arc::new(DashMap::new()),
                force: true,
            },
        }
    }
}

pub trait HasCreateUnhashedSymlinkLock {
    fn set_create_unhashed_symlink_lock(&mut self, lock: Arc<Mutex<()>>);

    fn get_create_unhashed_symlink_lock(&self) -> Arc<Mutex<()>>;
}

impl HasCreateUnhashedSymlinkLock for UserComputationData {
    fn set_create_unhashed_symlink_lock(&mut self, lock: Arc<Mutex<()>>) {
        self.data.set(lock);
    }

    fn get_create_unhashed_symlink_lock(&self) -> Arc<Mutex<()>> {
        self.data
            .get::<Arc<Mutex<()>>>()
            .expect("Lock for creating unhashed symlinks should be set")
            .dupe()
    }
}
