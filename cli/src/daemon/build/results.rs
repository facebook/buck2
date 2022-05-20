/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

//! Processing and reporting the the results of the build

use buck2_build_api::bxl::BxlFunctionLabel;
use buck2_core::provider::ConfiguredProvidersLabel;

use crate::daemon::build::BuildTargetResult;

pub enum BuildOwner<'a> {
    Target(&'a ConfiguredProvidersLabel),
    Bxl(&'a BxlFunctionLabel),
}

/// Collects the results of the build and processes it
pub trait BuildResultCollector: Send {
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
    use buck2_build_api::{
        actions::artifact::ArtifactFs,
        build::{BuildProviderType, ProviderArtifacts},
    };
    use buck2_core::{configuration::Configuration, result::SharedError};
    use cli_proto::{
        build_target::{build_output::BuildOutputProviders, BuildOutput},
        BuildTarget,
    };
    use gazebo::prelude::*;
    use indexmap::IndexMap;

    use crate::daemon::build::{
        results::{BuildOwner, BuildResultCollector},
        BuildTargetResult,
    };

    /// Simple container for multiple [`SharedError`]s
    pub struct SharedErrors {
        pub errors: Vec<SharedError>,
    }

    pub struct ResultReporter<'a> {
        artifact_fs: &'a ArtifactFs,
        return_outputs: bool,
        results: Result<Vec<BuildTarget>, SharedErrors>,
    }

    impl<'a> ResultReporter<'a> {
        pub fn new(artifact_fs: &'a ArtifactFs, return_outputs: bool) -> Self {
            Self {
                artifact_fs,
                return_outputs,
                results: Ok(Vec::new()),
            }
        }

        pub fn results(self) -> Result<Vec<BuildTarget>, SharedErrors> {
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
                let artifacts = if self.return_outputs {
                    // NOTE: We use an IndexMap here to preserve the order the rule author wrote, all
                    // the while avoiding duplicates.
                    let mut artifacts = IndexMap::new();

                    for output in outputs {
                        let ProviderArtifacts {
                            values,
                            provider_type,
                        } = output;

                        for (artifact, _value) in values.iter() {
                            let mut entry =
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
                            path: artifact_fs.resolve(a).unwrap().to_string(),
                            providers: Some(providers),
                        })
                        .collect()
                } else {
                    Vec::new()
                };

                let (target, configuration) = match label {
                    BuildOwner::Target(t) => (t.unconfigured().to_string(), t.cfg().to_string()),
                    BuildOwner::Bxl(l) => {
                        // for bxl, there's no configurations so we use the unspecified configuration
                        (l.to_string(), Configuration::unspecified().to_string())
                    }
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

    use buck2_build_api::{
        actions::artifact::ArtifactFs, build::BuildProviderType, bxl::BxlFunctionLabel,
    };
    use buck2_core::{
        configuration::Configuration,
        fs::{
            paths::{AbsPath, AbsPathBuf},
            project::ProjectRelativePathBuf,
        },
        provider::ProvidersName,
        target::TargetLabel,
    };
    use derivative::Derivative;
    use events::TraceId;
    use gazebo::prelude::*;
    use indexmap::IndexSet;
    use itertools::Itertools;
    use serde::Serialize;

    use crate::daemon::build::{
        results::{BuildOwner, BuildResultCollector},
        BuildTargetResult,
    };

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
    pub struct BuildReport {
        trace_id: TraceId,
        success: bool,
        results: HashMap<EntryLabel, ConfiguredBuildReportEntry>,
        failures: HashMap<EntryLabel, ProjectRelativePathBuf>,
        project_root: AbsPathBuf,
        truncated: bool,
    }

    #[derive(Default, Debug, Serialize)]
    pub struct BuildReportEntry {
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
    pub struct ConfiguredBuildReportEntry {
        #[serde(flatten)]
        #[serde(skip_serializing_if = "Option::is_none")]
        compatible: Option<BuildReportEntry>,

        /// the configured entry
        configured: HashMap<Configuration, BuildReportEntry>,
    }

    #[derive(Derivative, Serialize, Eq, PartialEq, Hash)]
    #[derivative(Debug)]
    #[serde(untagged)]
    enum EntryLabel {
        #[derivative(Debug = "transparent")]
        Target(TargetLabel),
        #[derivative(Debug = "transparent")]
        Bxl(BxlFunctionLabel),
    }

    pub struct BuildReportCollector<'a> {
        trace_id: &'a TraceId,
        artifact_fs: &'a ArtifactFs,
        build_report_results: HashMap<EntryLabel, ConfiguredBuildReportEntry>,
        overall_success: bool,
        project_root: &'a AbsPath,
        include_unconfigured_section: bool,
        include_other_outputs: bool,
    }

    impl<'a> BuildReportCollector<'a> {
        pub fn new(
            trace_id: &'a TraceId,
            artifact_fs: &'a ArtifactFs,
            project_root: &'a AbsPath,
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

        pub fn into_report(self) -> BuildReport {
            BuildReport {
                trace_id: self.trace_id.dupe(),
                success: self.overall_success,
                results: self.build_report_results,
                failures: HashMap::new(),
                project_root: self.project_root.to_buf(),
                // In buck1 we may truncate build report for a large number of targets.
                // Setting this to false since we don't currently truncate buck2's build report.
                truncated: false,
            }
        }
    }

    impl<'a> BuildResultCollector for BuildReportCollector<'a> {
        fn collect_result(&mut self, label: &BuildOwner, result: &BuildTargetResult) {
            let (default_outs, other_outs, success) = {
                let mut default_outs = IndexSet::new();
                let mut other_outs = IndexSet::new();
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
                                        .insert(self.artifact_fs.resolve(artifact).unwrap());
                                }

                                if is_other && self.include_other_outputs {
                                    other_outs.insert(self.artifact_fs.resolve(artifact).unwrap());
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
                    BuildOwner::Bxl(l) => EntryLabel::Bxl((*l).clone()),
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
                    BuildOwner::Bxl(_) => Configuration::unspecified(),
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
                ProvidersName::Named(names) => names.iter().join("|"),
                ProvidersName::UnrecognizedFlavor(f) => {
                    format!("#{}", f)
                }
            },
            BuildOwner::Bxl(_) => "DEFAULT".to_owned(),
        }
    }
}

pub mod providers {
    use crate::daemon::build::{
        results::{BuildOwner, BuildResultCollector},
        BuildTargetResult,
    };

    pub(crate) struct ProvidersPrinter;

    impl BuildResultCollector for ProvidersPrinter {
        fn collect_result(&mut self, _label: &BuildOwner, result: &BuildTargetResult) {
            // TODO: should we print the label here?
            let providers = result.providers.as_ref().unwrap().provider_collection();
            for x in providers.provider_ids() {
                let p = providers.get_provider_raw(x).unwrap();
                eprintln!("    {} = {}", x.name(), p);
            }
        }
    }
}
