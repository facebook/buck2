/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

//! Processing and reporting the the results of the build

use std::collections::hash_map::DefaultHasher;
use std::collections::BTreeMap;
use std::collections::HashMap;
use std::collections::HashSet;
use std::hash::Hash;
use std::hash::Hasher;
use std::io::BufWriter;
use std::sync::Arc;

use buck2_artifact::artifact::artifact_dump::ArtifactInfo;
use buck2_artifact::artifact::artifact_dump::DirectoryInfo;
use buck2_artifact::artifact::artifact_dump::ExternalSymlinkInfo;
use buck2_artifact::artifact::artifact_dump::FileInfo;
use buck2_artifact::artifact::artifact_dump::SymlinkInfo;
use buck2_cli_proto::CommonBuildOptions;
use buck2_common::legacy_configs::dice::HasLegacyConfigs;
use buck2_common::legacy_configs::key::BuckconfigKeyRef;
use buck2_core::cells::CellResolver;
use buck2_core::configuration::compatibility::MaybeCompatible;
use buck2_core::configuration::data::ConfigurationData;
use buck2_core::fs::artifact_path_resolver::ArtifactFs;
use buck2_core::fs::fs_util;
use buck2_core::fs::paths::abs_norm_path::AbsNormPathBuf;
use buck2_core::fs::project::ProjectRoot;
use buck2_core::fs::project_rel_path::ProjectRelativePath;
use buck2_core::fs::project_rel_path::ProjectRelativePathBuf;
use buck2_core::provider::label::ConfiguredProvidersLabel;
use buck2_core::provider::label::NonDefaultProvidersName;
use buck2_core::provider::label::ProvidersLabel;
use buck2_core::provider::label::ProvidersName;
use buck2_core::target::label::label::TargetLabel;
use buck2_directory::directory::entry::DirectoryEntry;
use buck2_error::BuckErrorContext;
use buck2_error::UniqueRootId;
use buck2_events::errors::create_error_report;
use buck2_execute::artifact::artifact_dyn::ArtifactDyn;
use buck2_execute::directory::ActionDirectoryEntry;
use buck2_execute::directory::ActionDirectoryMember;
use buck2_execute::directory::ActionSharedDirectory;
use buck2_wrapper_common::invocation_id::TraceId;
use derivative::Derivative;
use dice::DiceComputations;
use dupe::Dupe;
use itertools::Either;
use itertools::EitherOrBoth;
use itertools::Itertools;
use serde::Serialize;
use starlark_map::small_set::SmallSet;

use crate::build::action_error::BuildReportActionError;
use crate::build::BuildProviderType;
use crate::build::ConfiguredBuildTargetResult;

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

/// DO NOT UPDATE WITHOUT UPDATING `docs/users/build_observability/build_report.md`!
#[derive(Debug, Serialize)]
pub struct BuildReport {
    trace_id: TraceId,
    success: bool,
    results: HashMap<EntryLabel, BuildReportEntry>,
    /// filled only when fill-out-failures is passed for Buck1 backcompat only
    failures: HashMap<EntryLabel, String>,
    project_root: AbsNormPathBuf,
    truncated: bool,
    strings: BTreeMap<String, String>,
}

/// The fields that stored in the unconfigured `BuildReportEntry` for buck1 backcompat.
///
/// Do not put new fields in here. Put them in `ConfiguredBuildReportEntry`
#[derive(Default, Debug, Serialize)]
struct MaybeConfiguredBuildReportEntry {
    /// whether this particular target was successful
    success: BuildOutcome,
    /// a map of each subtarget of the current target (outputted as a `|` delimited list) to
    /// the default exposed output of the subtarget
    outputs: HashMap<Arc<str>, SmallSet<ProjectRelativePathBuf>>,
    /// a map of each subtarget of the current target (outputted as a `|` delimited list) to
    /// the hidden, implicitly built outputs of the subtarget. There are multiple outputs
    /// per subtarget
    ///
    /// FIXME(JakobDegen): This should be in `ConfiguredBuildReportEntry`
    other_outputs: HashMap<Arc<str>, SmallSet<ProjectRelativePathBuf>>,
    /// The size of the graph for this target, if it was produced
    ///
    /// FIXME(JakobDegen): This should be in `ConfiguredBuildReportEntry`
    configured_graph_size: Option<u64>,
}

/// DO NOT UPDATE WITHOUT UPDATING `docs/users/build_observability/build_report.md`!
#[derive(Default, Debug, Serialize)]
pub(crate) struct ConfiguredBuildReportEntry {
    /// A list of errors that occurred while building this target
    errors: Vec<BuildReportError>,
    /// Remote artifact information, including hashes, etc.
    #[serde(skip_serializing_if = "HashMap::is_empty")]
    artifact_info: HashMap<Arc<str>, ArtifactInfo>,
    #[serde(flatten)]
    inner: MaybeConfiguredBuildReportEntry,
}

/// DO NOT UPDATE WITHOUT UPDATING `docs/users/build_observability/build_report.md`!
#[derive(Debug, Serialize)]
struct BuildReportEntry {
    /// The buck1 build report did not support multiple configurations of the same target. We
    /// do, which is why we have the `configured` field below, which users should ideally use.
    /// This field is kept around for buck1 compatibility only and should ideally be removed.
    ///
    /// We avoid the `WithErrors` variant here, to keep the errors field from conflicting with
    /// the one on this struct.
    #[serde(flatten)]
    #[serde(skip_serializing_if = "Option::is_none")]
    compatible: Option<MaybeConfiguredBuildReportEntry>,

    /// the configured entry
    configured: HashMap<ConfigurationData, ConfiguredBuildReportEntry>,

    /// Errors that could not be associated with a particular configured version of the target,
    /// typically because they happened before configuration.
    errors: Vec<BuildReportError>,

    /// The path to the package where this target is defined, relative to the project root.
    #[serde(skip_serializing_if = "Option::is_none")]
    package_project_relative_path: Option<ProjectRelativePathBuf>,
}

/// DO NOT UPDATE WITHOUT UPDATING `docs/users/build_observability/build_report.md`!
#[derive(Debug, Clone, Serialize, PartialOrd, Ord, PartialEq, Eq)]
struct BuildReportError {
    message_content: String,
    action_error: Option<BuildReportActionError>,
    error_tags: Vec<String>,
    /// An opaque index that can be use to de-duplicate errors. Two errors with the same
    /// cause index have the same cause
    ///
    /// For example, two targets in different packages may have the same cause (evaluation of
    /// common bzl file), but error stack will be different.
    cause_index: usize,
}

#[derive(Derivative, Serialize, Eq, PartialEq, Hash)]
#[derivative(Debug)]
#[serde(untagged)]
enum EntryLabel {
    #[derivative(Debug = "transparent")]
    Target(TargetLabel),
}

pub struct BuildReportOpts {
    pub print_unconfigured_section: bool,
    pub unstable_include_failures_build_report: bool,
    pub unstable_include_package_project_relative_paths: bool,
    pub unstable_include_artifact_hash_information: bool,
    pub unstable_build_report_filename: String,
}

pub struct BuildReportCollector<'a> {
    artifact_fs: &'a ArtifactFs,
    cell_resolver: &'a CellResolver,
    overall_success: bool,
    include_unconfigured_section: bool,
    error_cause_cache: HashMap<buck2_error::UniqueRootId, usize>,
    next_cause_index: usize,
    strings: BTreeMap<String, String>,
    failures: HashMap<EntryLabel, String>,
    include_failures: bool,
    include_package_project_relative_paths: bool,
    include_artifact_hash_information: bool,
}

impl<'a> BuildReportCollector<'a> {
    pub fn convert(
        trace_id: &TraceId,
        artifact_fs: &'a ArtifactFs,
        cell_resolver: &'a CellResolver,
        project_root: &ProjectRoot,
        include_unconfigured_section: bool,
        include_failures: bool,
        include_package_project_relative_paths: bool,
        include_artifact_hash_information: bool,
        configured: &BTreeMap<ConfiguredProvidersLabel, Option<ConfiguredBuildTargetResult>>,
        other_errors: &BTreeMap<Option<ProvidersLabel>, Vec<buck2_error::Error>>,
    ) -> BuildReport {
        let mut this: BuildReportCollector<'_> = Self {
            artifact_fs,
            cell_resolver,
            overall_success: true,
            include_unconfigured_section,
            error_cause_cache: HashMap::default(),
            next_cause_index: 0,
            strings: BTreeMap::default(),
            failures: HashMap::default(),
            include_failures,
            include_package_project_relative_paths,
            include_artifact_hash_information,
        };
        let mut entries = HashMap::new();

        if other_errors.values().flatten().next().is_some() {
            // Do this check ahead of time. We don't check for errors that aren't associated
            // with a target below, so we'd miss this otherwise.
            this.overall_success = false;
        }

        // The `BuildTargetResult` doesn't group errors by their unconfigured target, so we need
        // to do a little iterator munging to achieve that ourselves
        let results_by_unconfigured = configured
            .iter()
            .chunk_by(|x| x.0.target().unconfigured().dupe());
        let errors_by_unconfigured = other_errors
            .iter()
            .filter_map(|(l, e)| Some((l.as_ref()?.target().dupe(), e)));
        for i in Itertools::merge_join_by(
            IntoIterator::into_iter(&results_by_unconfigured),
            errors_by_unconfigured,
            |(l1, _), (l2, _)| Ord::cmp(l1, l2),
        ) {
            let (label, results, errors) = match i {
                EitherOrBoth::Both((label, results), (_, errors)) => {
                    (label, Either::Left(results), &**errors)
                }
                EitherOrBoth::Left((label, results)) => (label, Either::Left(results), &[][..]),
                EitherOrBoth::Right((label, errors)) => {
                    (label, Either::Right(std::iter::empty()), &**errors)
                }
            };
            let entry = this.collect_results_for_unconfigured(label.dupe(), results, errors);
            entries.insert(EntryLabel::Target(label), entry);
        }

        BuildReport {
            trace_id: trace_id.dupe(),
            success: this.overall_success,
            results: entries,
            failures: this.failures,
            project_root: project_root.root().to_owned(),
            // In buck1 we may truncate build report for a large number of targets.
            // Setting this to false since we don't currently truncate buck2's build report.
            truncated: false,
            strings: this.strings,
        }
    }

    pub(crate) fn update_string_cache(&mut self, string: String) -> String {
        let mut hasher = DefaultHasher::new();
        string.hash(&mut hasher);
        let hash = hasher.finish().to_string();
        self.strings.insert(hash.clone(), string);
        hash
    }

    /// Always called for one unconfigured target at a time
    fn collect_results_for_unconfigured<'b>(
        &mut self,
        target: TargetLabel,
        results: impl IntoIterator<
            Item = (
                &'b ConfiguredProvidersLabel,
                &'b Option<ConfiguredBuildTargetResult>,
            ),
        >,
        errors: &[buck2_error::Error],
    ) -> BuildReportEntry {
        // NOTE: if we're actually building a thing, then the package path must exist, but be
        // conservative and don't crash the overall processing if that happens.
        let package_project_relative_path = if self.include_package_project_relative_paths {
            self.cell_resolver
                .resolve_path(target.pkg().as_cell_path())
                .ok()
        } else {
            None
        };

        let mut unconfigured_report = if self.include_unconfigured_section {
            Some(MaybeConfiguredBuildReportEntry::default())
        } else {
            None
        };
        let mut configured_reports = HashMap::new();

        for (label, results) in &results
            .into_iter()
            // We omit skipped targets here.
            .filter_map(|(label, result)| Some((label, result.as_ref()?)))
            .chunk_by(|x| x.0.target().dupe())
        {
            let configured_report = self.collect_results_for_configured(target.dupe(), results);
            if let Some(report) = unconfigured_report.as_mut() {
                if !configured_report.errors.is_empty() {
                    report.success = BuildOutcome::FAIL;
                }

                // FIXME(JakobDegen): This potentially overwrites entries from other
                // configurations. Is that intended? Send a diff with a comment if you know
                report.outputs.extend(
                    configured_report
                        .inner
                        .outputs
                        .iter()
                        .map(|(k, v)| (k.clone(), v.clone())),
                );
                report.other_outputs.extend(
                    configured_report
                        .inner
                        .other_outputs
                        .iter()
                        .map(|(k, v)| (k.clone(), v.clone())),
                );
                if let Some(configured_graph_size) = configured_report.inner.configured_graph_size {
                    report.configured_graph_size = Some(configured_graph_size);
                }
            }

            configured_reports.insert(label.cfg().dupe(), configured_report);
        }

        let errors = self.convert_error_list(errors, target);
        if !errors.is_empty() {
            if let Some(report) = unconfigured_report.as_mut() {
                report.success = BuildOutcome::FAIL;
            }
        }

        BuildReportEntry {
            compatible: unconfigured_report,
            configured: configured_reports,
            errors,
            package_project_relative_path,
        }
    }

    fn collect_results_for_configured<'b>(
        &mut self,
        target: TargetLabel,
        results: impl IntoIterator<
            Item = (
                &'b ConfiguredProvidersLabel,
                &'b ConfiguredBuildTargetResult,
            ),
        >,
    ) -> ConfiguredBuildReportEntry {
        let mut configured_report = ConfiguredBuildReportEntry::default();
        let mut errors = Vec::new();
        for (label, result) in results {
            let provider_name: Arc<str> = report_providers_name(label).into();

            result.outputs.iter().for_each(|res| match res {
                Ok(artifacts) => {
                    if artifacts.provider_type == BuildProviderType::Default {
                        for (artifact, value) in artifacts.values.iter() {
                            if self.include_artifact_hash_information {
                                update_artifact_info(
                                    &mut configured_report.artifact_info,
                                    provider_name.dupe(),
                                    &value.entry(),
                                );
                            }
                            configured_report
                                .inner
                                .outputs
                                .entry(provider_name.dupe())
                                .or_default()
                                .insert(artifact.resolve_path(self.artifact_fs).unwrap());
                        }
                    }
                }
                Err(e) => errors.push(e.dupe()),
            });

            errors.extend(result.errors.iter().cloned());

            if let Some(Ok(MaybeCompatible::Compatible(configured_graph_size))) =
                result.configured_graph_size
            {
                configured_report.inner.configured_graph_size = Some(configured_graph_size);
            }
        }
        configured_report.errors = self.convert_error_list(&errors, target);
        if !configured_report.errors.is_empty() {
            configured_report.inner.success = BuildOutcome::FAIL;
        }
        configured_report
    }

    /// Note: In order for production of the build report to be deterministic, the order in
    /// which this function is called, and which errors it is called with, must be
    /// deterministic. The particular order of the errors need not be.
    fn convert_error_list(
        &mut self,
        errors: &[buck2_error::Error],
        target: TargetLabel,
    ) -> Vec<BuildReportError> {
        if errors.is_empty() {
            return Vec::new();
        }
        self.overall_success = false;

        struct ExpandedErrorInfo {
            root: UniqueRootId,
            cause_index: Option<usize>,
            message: String,
            error_tags: Vec<String>,
            action_error: Option<BuildReportActionError>,
        }

        let mut temp = Vec::with_capacity(errors.len());
        for e in errors {
            // we initially avoid assigning new cause indexes and instead use a sentinal value.
            // This is to make sure that we can be deterministic
            let root = e.root_id();
            let error_report = create_error_report(e);
            let message = if let Some(telemetry_message) = error_report.telemetry_message {
                telemetry_message
            } else {
                error_report.message
            };
            let error_tags = e
                .tags()
                .into_iter()
                .map(|tag| tag.as_str_name().to_owned())
                .collect_vec();

            temp.push(ExpandedErrorInfo {
                root,
                cause_index: self.error_cause_cache.get(&root).copied(),
                message,
                error_tags,
                action_error: e
                    .action_error()
                    .map(|e| BuildReportActionError::new(e, self)),
            });
        }
        // Sort the errors. This sort *almost* guarantees full determinism, but unfortunately
        // not quite; it is hypothetically non-deterministic if the same configured target has
        // two errors with different error roots but the same error message. Probably unlikely?
        temp.sort_unstable_by(|x, y| {
            Ord::cmp(&(x.cause_index, &x.message), &(y.cause_index, &y.message))
        });

        // Deduplicate errors with the same root. We have to do this after sorting to retain
        // determinism.
        //
        // FIXME(JakobDegen): Ideally we wouldn't need this. It originally wasn't here, but this
        // caused the size of the build report to grow very large in some cases. I suspect this
        // is the result of some rules producing large amounts of `other_outputs`. Because those
        // are all top level artifacts that get their own `BuildEvent`, if they all fail, they
        // all get their own error in the build report. Completing the migration to artifact
        // groups would likely let us get rid of this.
        let mut found_roots = HashSet::new();
        temp.retain(|info| found_roots.insert(info.root));

        let mut out = Vec::with_capacity(temp.len());
        // Now assign new cause indexes if we haven't yet
        for info in temp {
            let cause_index = match info.cause_index {
                Some(i) => i,
                None => {
                    // We need to recheck the cache first, as a previous iteration of this loop
                    // may have inserted our root
                    self.error_cause_cache
                        .get(&info.root)
                        .copied()
                        .unwrap_or_else(|| {
                            let index = self.next_cause_index;
                            self.next_cause_index += 1;
                            self.error_cause_cache.insert(info.root, index);
                            index
                        })
                }
            };

            let message_content = self.update_string_cache(info.message.clone());

            out.push(BuildReportError {
                message_content,
                action_error: info.action_error,
                error_tags: info.error_tags,
                cause_index,
            });
        }

        if self.include_failures {
            // Order is deterministic now, so picking the last one is fine. Also, we checked that
            // there was at least one error above.
            //
            // This both omits errors and overwrites previous ones. That's the price you pay for
            // using buck1
            self.failures.insert(
                EntryLabel::Target(target),
                self.strings
                    .get(&out.last().unwrap().message_content)
                    .unwrap()
                    .to_string(),
            );
        }

        out
    }
}

fn update_artifact_info(
    artifact_info: &mut HashMap<Arc<str>, ArtifactInfo>,
    provider_name: Arc<str>,
    entry: &ActionDirectoryEntry<ActionSharedDirectory>,
) {
    match entry {
        DirectoryEntry::Dir(dir) => {
            artifact_info.insert(
                provider_name,
                ArtifactInfo::Directory(DirectoryInfo {
                    digest: dir.fingerprint().clone(),
                }),
            );
        }
        DirectoryEntry::Leaf(ActionDirectoryMember::File(metadata)) => {
            let cas_digest = metadata.digest.data();
            artifact_info.insert(
                provider_name,
                ArtifactInfo::File(FileInfo {
                    digest: *cas_digest,
                    is_exec: metadata.is_executable,
                }),
            );
        }
        DirectoryEntry::Leaf(ActionDirectoryMember::Symlink(symlink_target)) => {
            artifact_info.insert(
                provider_name,
                ArtifactInfo::Symlink(SymlinkInfo {
                    symlink_rel_path: symlink_target.target().into(),
                }),
            );
        }
        DirectoryEntry::Leaf(ActionDirectoryMember::ExternalSymlink(external_symlink)) => {
            artifact_info.insert(
                provider_name,
                ArtifactInfo::ExternalSymlink(ExternalSymlinkInfo {
                    target: external_symlink.target().into(),
                    remaining_path: if external_symlink.remaining_path().is_empty() {
                        None
                    } else {
                        Some(external_symlink.remaining_path().to_owned())
                    },
                }),
            );
        }
    }
}

fn report_providers_name(label: &ConfiguredProvidersLabel) -> String {
    match label.name() {
        ProvidersName::Default => "DEFAULT".to_owned(),
        ProvidersName::NonDefault(flavor) => match flavor.as_ref() {
            NonDefaultProvidersName::Named(names) => names.iter().join("|"),
            NonDefaultProvidersName::UnrecognizedFlavor(s) => {
                format!("#{}", s)
            }
        },
    }
}

pub async fn build_report_opts<'a>(
    ctx: &mut DiceComputations<'a>,
    cell_resolver: &CellResolver,
    build_opts: &CommonBuildOptions,
) -> buck2_error::Result<BuildReportOpts> {
    let esto = &build_opts.unstable_build_report_filename;
    let build_report_opts = BuildReportOpts {
        print_unconfigured_section: ctx
            .parse_legacy_config_property(
                cell_resolver.root_cell(),
                BuckconfigKeyRef {
                    section: "build_report",
                    property: "print_unconfigured_section",
                },
            )
            .await?
            .unwrap_or(true),
        unstable_include_failures_build_report: build_opts.unstable_include_failures_build_report,
        unstable_include_package_project_relative_paths: build_opts
            .unstable_include_package_project_relative_paths,
        unstable_include_artifact_hash_information: build_opts
            .unstable_include_artifact_hash_information,
        unstable_build_report_filename: esto.clone(),
    };

    Ok(build_report_opts)
}

pub fn generate_build_report(
    opts: BuildReportOpts,
    artifact_fs: &ArtifactFs,
    cell_resolver: &CellResolver,
    project_root: &ProjectRoot,
    cwd: &ProjectRelativePath,
    trace_id: &TraceId,
    configured: &BTreeMap<ConfiguredProvidersLabel, Option<ConfiguredBuildTargetResult>>,
    other_errors: &BTreeMap<Option<ProvidersLabel>, Vec<buck2_error::Error>>,
) -> Result<Option<String>, buck2_error::Error> {
    let build_report = BuildReportCollector::convert(
        trace_id,
        artifact_fs,
        cell_resolver,
        project_root,
        opts.print_unconfigured_section,
        opts.unstable_include_failures_build_report,
        opts.unstable_include_package_project_relative_paths,
        opts.unstable_include_artifact_hash_information,
        configured,
        other_errors,
    );

    let mut serialized_build_report = None;

    if !opts.unstable_build_report_filename.is_empty() {
        let path = project_root
            .resolve(cwd)
            .as_abs_path()
            .join(opts.unstable_build_report_filename);
        if let Some(parent) = path.parent() {
            fs_util::create_dir_all(parent)?;
        }
        let file =
            fs_util::create_file(path.clone()).buck_error_context("Error writing build report")?;
        let mut file = BufWriter::new(file);
        serde_json::to_writer_pretty(&mut file, &build_report)?
    } else {
        serialized_build_report = Some(serde_json::to_string(&build_report)?);
    };

    Ok(serialized_build_report)
}
