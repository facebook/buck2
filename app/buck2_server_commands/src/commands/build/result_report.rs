/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

//! Processing and reporting the the results of the build

use buck2_build_api::build::BuildProviderType;
use buck2_build_api::build::BuildTargetResult;
use buck2_build_api::build::ConfiguredBuildTargetResult;
use buck2_build_api::build::ProviderArtifacts;
use buck2_certs::validate::check_cert_state;
use buck2_certs::validate::CertState;
use buck2_core::configuration::compatibility::MaybeCompatible;
use buck2_core::fs::artifact_path_resolver::ArtifactFs;
use buck2_core::provider::label::ConfiguredProvidersLabel;
use buck2_execute::artifact::artifact_dyn::ArtifactDyn;
use dupe::Dupe;
use starlark_map::small_map::SmallMap;

mod proto {
    pub use buck2_cli_proto::build_target::build_output::BuildOutputProviders;
    pub use buck2_cli_proto::build_target::BuildOutput;
    pub use buck2_cli_proto::BuildTarget;
}

/// Simple container for multiple [`buck2_error::Error`]s
pub(crate) struct BuildErrors {
    pub(crate) errors: Vec<buck2_error::Error>,
}

#[derive(Copy, Clone, Dupe)]
pub(crate) struct ResultReporterOptions {
    pub(crate) return_outputs: bool,
    pub(crate) return_default_other_outputs: bool,
}

/// Collects build results into a Result<Vec<proto::BuildTarget>, buck2_error::Errors>. If any targets
/// fail, then the error case will be returned, otherwise a vec of all the successful results.
pub(crate) struct ResultReporter<'a> {
    artifact_fs: &'a ArtifactFs,
    options: ResultReporterOptions,
    results: Vec<proto::BuildTarget>,
}

pub(crate) struct BuildTargetsAndErrors {
    pub(crate) build_targets: Vec<proto::BuildTarget>,
    pub(crate) build_errors: BuildErrors,
}

impl<'a> ResultReporter<'a> {
    pub(crate) async fn convert(
        artifact_fs: &'a ArtifactFs,
        cert_state: CertState,
        options: ResultReporterOptions,
        build_result: &BuildTargetResult,
    ) -> BuildTargetsAndErrors {
        let mut out = Self {
            artifact_fs,
            options,
            results: Vec::new(),
        };

        let mut non_action_errors = vec![];
        let mut action_errors = vec![];
        non_action_errors.extend(build_result.other_errors.values().flatten().cloned());

        for (k, v) in &build_result.configured {
            // We omit skipped targets here.
            let Some(v) = v else { continue };
            non_action_errors.extend(v.errors.iter().cloned());
            action_errors.extend(v.outputs.iter().filter_map(|x| x.as_ref().err()).cloned());

            out.collect_result(k, v);
        }

        let mut error_list = if let Some(e) = non_action_errors.pop() {
            // FIXME(JakobDegen): We'd like to return more than one error here, but we have
            // to get better at error deduplication first
            vec![e]
        } else {
            // FIXME: Only one non-action error or all action errors is returned currently
            action_errors
        };

        if !error_list.is_empty() {
            if let Some(e) = check_cert_state(cert_state).await {
                error_list.push(e.into());
            }
        }

        BuildTargetsAndErrors {
            build_targets: out.results,
            build_errors: BuildErrors { errors: error_list },
        }
    }

    fn collect_result(
        &mut self,
        label: &ConfiguredProvidersLabel,
        result: &ConfiguredBuildTargetResult,
    ) {
        let outputs = result
            .outputs
            .iter()
            .filter_map(|output| output.as_ref().ok());

        let artifacts = if self.options.return_outputs {
            // NOTE: We use an SmallMap here to preserve the order the rule author wrote, all
            // the while avoiding duplicates.
            let mut artifacts = SmallMap::new();

            for output in outputs {
                let ProviderArtifacts {
                    values,
                    provider_type,
                } = output;

                if !self.options.return_default_other_outputs
                    && matches!(provider_type, BuildProviderType::DefaultOther)
                {
                    continue;
                }

                for (artifact, _value) in values.iter() {
                    let entry =
                        artifacts
                            .entry(artifact)
                            .or_insert_with(|| proto::BuildOutputProviders {
                                default_info: false,
                                run_info: false,
                                other: false,
                                test_info: false,
                            });

                    match provider_type {
                        BuildProviderType::Default => {
                            entry.default_info = true;
                        }
                        BuildProviderType::DefaultOther => {
                            entry.other = true;
                        }
                        BuildProviderType::Run => {
                            entry.run_info = true;
                        }
                        BuildProviderType::Test => {
                            entry.test_info = true;
                        }
                    }
                }
            }

            let artifact_fs = &self.artifact_fs;

            // Write it this way because `.into_iter()` gets rust-analyzer confused
            IntoIterator::into_iter(artifacts)
                .map(|(a, providers)| proto::BuildOutput {
                    path: a.resolve_path(artifact_fs).unwrap().to_string(),
                    providers: Some(providers),
                })
                .collect()
        } else {
            Vec::new()
        };

        let target = label.unconfigured().to_string();
        let configuration = label.cfg().to_string();

        let configured_graph_size = match &result.configured_graph_size {
            Some(Ok(MaybeCompatible::Compatible(v))) => Some(*v),
            Some(Ok(MaybeCompatible::Incompatible(..))) => None,
            Some(Err(e)) => {
                // We don't expect an error on this unless something else on this target
                // failed.
                tracing::debug!(
                    "Graph size calculation error failed for {}: {:#}",
                    target,
                    e
                );
                None
            }
            None => None,
        };

        self.results.push(proto::BuildTarget {
            target,
            configuration,
            run_args: result.run_args.clone().unwrap_or_default(),
            target_rule_type_name: result.target_rule_type_name.clone(),
            outputs: artifacts,
            configured_graph_size,
        })
    }
}
