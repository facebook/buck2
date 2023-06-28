/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

//! Processing and reporting the the results of the build

use buck2_build_api::build::BuildTargetResult;
use buck2_core::provider::label::ConfiguredProvidersLabel;

pub(crate) enum BuildOwner<'a> {
    Target(&'a ConfiguredProvidersLabel),
}

/// Collects the results of the build and processes it
pub(crate) trait BuildResultCollector: Send {
    fn collect_result(&mut self, label: &BuildOwner, result: &BuildTargetResult);
}

impl BuildResultCollector for Vec<&mut dyn BuildResultCollector> {
    fn collect_result(&mut self, label: &BuildOwner, result: &BuildTargetResult) {
        for collector in self {
            collector.collect_result(label, result);
        }
    }
}

pub mod result_report {
    use buck2_build_api::build::BuildProviderType;
    use buck2_build_api::build::BuildTargetResult;
    use buck2_build_api::build::ProviderArtifacts;
    use buck2_cli_proto::build_target::build_output::BuildOutputProviders;
    use buck2_cli_proto::build_target::BuildOutput;
    use buck2_cli_proto::BuildTarget;
    use buck2_common::result::SharedError;
    use buck2_core::fs::artifact_path_resolver::ArtifactFs;
    use buck2_execute::artifact::artifact_dyn::ArtifactDyn;
    use dupe::Dupe;
    use starlark_map::small_map::SmallMap;

    use crate::commands::build::results::BuildOwner;
    use crate::commands::build::results::BuildResultCollector;

    /// Simple container for multiple [`SharedError`]s
    pub(crate) struct SharedErrors {
        pub errors: Vec<SharedError>,
    }

    #[derive(Copy, Clone, Dupe)]
    pub(crate) struct ResultReporterOptions {
        pub(crate) return_outputs: bool,
        pub(crate) return_default_other_outputs: bool,
    }

    pub(crate) struct ResultReporter<'a> {
        artifact_fs: &'a ArtifactFs,
        options: ResultReporterOptions,
        results: Result<Vec<BuildTarget>, SharedErrors>,
    }

    impl<'a> ResultReporter<'a> {
        pub(crate) fn new(artifact_fs: &'a ArtifactFs, options: ResultReporterOptions) -> Self {
            Self {
                artifact_fs,
                options,
                results: Ok(Vec::new()),
            }
        }

        pub(crate) fn results(self) -> Result<Vec<BuildTarget>, SharedErrors> {
            self.results
        }
    }

    impl<'a> BuildResultCollector for ResultReporter<'a> {
        fn collect_result(&mut self, label: &BuildOwner, result: &BuildTargetResult) {
            let outputs = result
                .outputs
                .iter()
                .filter_map(|output| match output {
                    Ok(output) => Some(output),
                    Err(e) => {
                        match self.results.as_mut() {
                            Ok(..) => {
                                self.results = Err(SharedErrors {
                                    errors: vec![e.dupe()],
                                });
                            }
                            Err(errs) => errs.errors.push(e.dupe()),
                        };
                        None
                    }
                })
                .collect::<Vec<_>>();

            if let Ok(r) = &mut self.results {
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
                                    .or_insert_with(|| BuildOutputProviders {
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

                    artifacts
                        .into_iter()
                        .map(|(a, providers)| BuildOutput {
                            path: a.resolve_path(artifact_fs).unwrap().to_string(),
                            providers: Some(providers),
                        })
                        .collect()
                } else {
                    Vec::new()
                };

                let (target, configuration) = match label {
                    BuildOwner::Target(t) => (t.unconfigured().to_string(), t.cfg().to_string()),
                };

                r.push(BuildTarget {
                    target,
                    configuration,
                    run_args: result.run_args.clone().unwrap_or_default(),
                    outputs: artifacts,
                })
            };
        }
    }
}

pub mod build_report {
    use std::collections::HashMap;

    use buck2_build_api::build::BuildProviderType;
    use buck2_core::configuration::data::ConfigurationData;
    use buck2_core::fs::artifact_path_resolver::ArtifactFs;
    use buck2_core::fs::paths::abs_norm_path::AbsNormPathBuf;
    use buck2_core::fs::project::ProjectRoot;
    use buck2_core::fs::project_rel_path::ProjectRelativePathBuf;
    use buck2_core::provider::label::NonDefaultProvidersName;
    use buck2_core::provider::label::ProvidersName;
    use buck2_core::target::label::TargetLabel;
    use buck2_execute::artifact::artifact_dyn::ArtifactDyn;
    use buck2_wrapper_common::invocation_id::TraceId;
    use derivative::Derivative;
    use dupe::Dupe;
    use itertools::Itertools;
    use serde::Serialize;
    use starlark_map::small_set::SmallSet;

    use crate::commands::build::results::BuildOwner;
    use crate::commands::build::results::BuildResultCollector;
    use crate::commands::build::BuildTargetResult;

    #[derive(Debug, Serialize)]
    #[allow(clippy::upper_case_acronyms)] // We care about how they serialise
    enum BuildOutcome {
        SUCCESS,
        FAIL,
        #[allow(dead_code)] // Part of the spec, but not yet used
        CANCELED,
    }

    impl Default for BuildOutcome {
        fn default() -> Self {
            Self::SUCCESS
        }
    }

    #[derive(Debug, Serialize)]
    pub(crate) struct BuildReport {
        trace_id: TraceId,
        success: bool,
        results: HashMap<EntryLabel, ConfiguredBuildReportEntry>,
        failures: HashMap<EntryLabel, ProjectRelativePathBuf>,
        project_root: AbsNormPathBuf,
        truncated: bool,
    }

    #[derive(Default, Debug, Serialize)]
    pub(crate) struct BuildReportEntry {
        /// whether this particular target was successful
        success: BuildOutcome,
        /// a map of each subtarget of the current target (outputted as a `|` delimited list) to
        /// the default exposed output of the subtarget
        outputs: HashMap<String, Vec<ProjectRelativePathBuf>>,
        /// a map of each subtarget of the current target (outputted as a `|` delimited list) to
        /// the hidden, implicitly built outputs of the subtarget. There are multiple outputs
        /// per subtarget
        other_outputs: HashMap<String, Vec<ProjectRelativePathBuf>>,
    }

    #[derive(Debug, Serialize)]
    pub(crate) struct ConfiguredBuildReportEntry {
        #[serde(flatten)]
        #[serde(skip_serializing_if = "Option::is_none")]
        compatible: Option<BuildReportEntry>,

        /// the configured entry
        configured: HashMap<ConfigurationData, BuildReportEntry>,
    }

    #[derive(Derivative, Serialize, Eq, PartialEq, Hash)]
    #[derivative(Debug)]
    #[serde(untagged)]
    enum EntryLabel {
        #[derivative(Debug = "transparent")]
        Target(TargetLabel),
    }

    pub(crate) struct BuildReportCollector<'a> {
        trace_id: &'a TraceId,
        artifact_fs: &'a ArtifactFs,
        build_report_results: HashMap<EntryLabel, ConfiguredBuildReportEntry>,
        overall_success: bool,
        project_root: &'a ProjectRoot,
        include_unconfigured_section: bool,
        include_other_outputs: bool,
    }

    impl<'a> BuildReportCollector<'a> {
        pub(crate) fn new(
            trace_id: &'a TraceId,
            artifact_fs: &'a ArtifactFs,
            project_root: &'a ProjectRoot,
            include_unconfigured_section: bool,
            include_other_outputs: bool,
        ) -> Self {
            Self {
                trace_id,
                artifact_fs,
                build_report_results: HashMap::new(),
                overall_success: true,
                project_root,
                include_unconfigured_section,
                include_other_outputs,
            }
        }

        pub(crate) fn into_report(self) -> BuildReport {
            BuildReport {
                trace_id: self.trace_id.dupe(),
                success: self.overall_success,
                results: self.build_report_results,
                failures: HashMap::new(),
                project_root: self.project_root.root().to_owned(),
                // In buck1 we may truncate build report for a large number of targets.
                // Setting this to false since we don't currently truncate buck2's build report.
                truncated: false,
            }
        }
    }

    impl<'a> BuildResultCollector for BuildReportCollector<'a> {
        fn collect_result(&mut self, label: &BuildOwner, result: &BuildTargetResult) {
            let (default_outs, other_outs, success) = {
                let mut default_outs = SmallSet::new();
                let mut other_outs = SmallSet::new();
                let mut success = true;

                result.outputs.iter().for_each(|res| {
                    match res {
                        Ok(artifacts) => {
                            let mut is_default = false;
                            let mut is_other = false;

                            match artifacts.provider_type {
                                BuildProviderType::Default => {
                                    // as long as we have requested it as a default info, it should  be
                                    // considered a default output whether or not it also appears as an other
                                    // non-main output
                                    is_default = true;
                                }
                                BuildProviderType::DefaultOther
                                | BuildProviderType::Run
                                | BuildProviderType::Test => {
                                    // as long as the output isn't the default, we add it to other outputs.
                                    // This means that the same artifact may appear twice if its part of the
                                    // default AND the other outputs, but this is intended as it accurately
                                    // describes the type of the artifact
                                    is_other = true;
                                }
                            }

                            for (artifact, _value) in artifacts.values.iter() {
                                if is_default {
                                    default_outs
                                        .insert(artifact.resolve_path(self.artifact_fs).unwrap());
                                }

                                if is_other && self.include_other_outputs {
                                    other_outs
                                        .insert(artifact.resolve_path(self.artifact_fs).unwrap());
                                }
                            }
                        }
                        Err(..) => success = false,
                    }
                });

                (default_outs, other_outs, success)
            };

            let report_results = self
                .build_report_results
                .entry(match label {
                    BuildOwner::Target(t) => EntryLabel::Target(t.unconfigured().target().dupe()),
                })
                .or_insert_with(|| ConfiguredBuildReportEntry {
                    compatible: if self.include_unconfigured_section {
                        Some(BuildReportEntry::default())
                    } else {
                        None
                    },
                    configured: HashMap::new(),
                });

            let unconfigured_report = &mut report_results.compatible;
            let configured_report = report_results
                .configured
                .entry(match label {
                    BuildOwner::Target(t) => t.cfg().dupe(),
                })
                .or_insert_with(BuildReportEntry::default);
            if !default_outs.is_empty() {
                if let Some(report) = unconfigured_report {
                    report.outputs.insert(
                        report_providers_name(label),
                        default_outs.iter().cloned().collect(),
                    );
                }

                configured_report.outputs.insert(
                    report_providers_name(label),
                    default_outs.into_iter().collect(),
                );
            }
            if !other_outs.is_empty() {
                if let Some(report) = unconfigured_report {
                    report.other_outputs.insert(
                        report_providers_name(label),
                        other_outs.iter().cloned().collect(),
                    );
                }

                configured_report.other_outputs.insert(
                    report_providers_name(label),
                    other_outs.into_iter().collect(),
                );
            }

            if !success {
                if let Some(report) = unconfigured_report {
                    report.success = BuildOutcome::FAIL;
                }
                configured_report.success = BuildOutcome::FAIL;
                self.overall_success = false;
            }
        }
    }

    fn report_providers_name(label: &BuildOwner) -> String {
        match label {
            BuildOwner::Target(t) => match t.name() {
                ProvidersName::Default => "DEFAULT".to_owned(),
                ProvidersName::NonDefault(box NonDefaultProvidersName::Named(names)) => {
                    names.iter().join("|")
                }
                ProvidersName::NonDefault(box NonDefaultProvidersName::UnrecognizedFlavor(f)) => {
                    format!("#{}", f)
                }
            },
        }
    }
}

pub mod providers {
    use crate::commands::build::results::BuildOwner;
    use crate::commands::build::results::BuildResultCollector;
    use crate::commands::build::BuildTargetResult;

    pub(crate) struct ProvidersPrinter;

    impl BuildResultCollector for ProvidersPrinter {
        fn collect_result(&mut self, _label: &BuildOwner, result: &BuildTargetResult) {
            // TODO: should we print the label here?
            let providers = result.providers.provider_collection();
            for x in providers.provider_ids() {
                let p = providers.get_provider_raw(x).unwrap();
                eprintln!("    {} = {}", x.name(), p);
            }
        }
    }
}
