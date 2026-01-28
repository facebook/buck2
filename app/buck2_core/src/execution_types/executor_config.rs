/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use std::fmt::Formatter;
use std::hash::Hash;
use std::hash::Hasher;
use std::str::FromStr;
use std::sync::Arc;
use std::time::Duration;

use allocative::Allocative;
use buck2_util::hash::BuckHasher;
use derive_more::Display;
use dupe::Dupe;
use itertools::Itertools;
use once_cell::sync::Lazy;
use pagable::Pagable;
use starlark_map::small_map::SmallMap;
use starlark_map::sorted_map::SortedMap;
use static_interner::Intern;
use static_interner::interner;

#[derive(Debug, Eq, Hash, PartialEq, Clone, Dupe, Allocative, Pagable)]
pub struct LocalExecutorOptions {
    pub use_persistent_workers: bool,
}

impl Default for LocalExecutorOptions {
    fn default() -> Self {
        Self {
            use_persistent_workers: true,
        }
    }
}

#[derive(Debug, Eq, Hash, PartialEq, Clone, Allocative, Pagable)]
pub struct RemoteEnabledExecutorOptions {
    pub executor: RemoteEnabledExecutor,
    pub re_properties: RePlatformFields,
    pub re_use_case: RemoteExecutorUseCase,
    pub re_action_key: Option<String>,
    pub cache_upload_behavior: CacheUploadBehavior,
    pub remote_cache_enabled: bool,
    pub remote_dep_file_cache_enabled: bool,
    pub dependencies: Vec<RemoteExecutorDependency>,
    pub gang_workers: Vec<ReGangWorker>,
    pub custom_image: Option<Box<RemoteExecutorCustomImage>>,
    pub meta_internal_extra_params: MetaInternalExtraParams,
}

#[derive(Debug, buck2_error::Error)]
#[buck2(input)]
enum RemoteExecutorDependencyErrors {
    #[error("RE dependency requires `{0}` to be set")]
    MissingField(&'static str),
    #[error("too many fields set for RE dependency: `{0}`")]
    UnsupportedFields(String),
}

#[derive(Debug, Eq, Hash, Pagable, PartialEq, Clone, Allocative)]
pub struct ImagePackageIdentifier {
    pub name: String,
    pub uuid: String,
}

#[derive(Debug, Eq, PartialEq, Clone, Hash, Pagable, Allocative)]
pub struct RemoteExecutorCafFbpkg {
    pub name: String,
    pub uuid: String,
    pub tag: Option<String>,
    pub permissions: Option<String>,
}

#[derive(Debug, Eq, Hash, Pagable, PartialEq, Clone, Allocative)]
pub struct RemoteExecutorCustomImage {
    pub identifier: ImagePackageIdentifier,
    pub drop_host_mount_globs: Vec<String>,
}

/// A Remote Action can specify a list of dependencies that are required before starting the execution `https://fburl.com/wiki/offzl3ox`
#[derive(Debug, Eq, PartialEq, Clone, Hash, Pagable, Allocative)]
pub struct RemoteExecutorDependency {
    /// The SMC tier that the Remote Executor will query to try to acquire the dependency
    pub smc_tier: String,
    /// The id of the dependency to acquire
    pub id: String,
}

/// Describes a worker in a gang for Remote Execution.
/// A gang is a collection of workers that are scheduled together for distributed execution.
/// Each worker specifies its capabilities (platform requirements).
#[derive(Debug, Eq, PartialEq, Clone, Hash, Allocative, Pagable)]
pub struct ReGangWorker {
    /// The platform capabilities required for this gang worker
    pub capabilities: SortedMap<String, String>,
}

impl ReGangWorker {
    pub fn parse(worker_map: SmallMap<&str, &str>) -> buck2_error::Result<ReGangWorker> {
        let capabilities: SortedMap<String, String> = worker_map
            .iter()
            .map(|(k, v)| (k.to_string(), v.to_string()))
            .collect();

        Ok(ReGangWorker { capabilities })
    }
}

impl RemoteExecutorDependency {
    pub fn parse(dep_map: SmallMap<&str, &str>) -> buck2_error::Result<RemoteExecutorDependency> {
        fn username() -> Option<String> {
            #[cfg(fbcode_build)]
            {
                user::current_username()
                    .ok()
                    .filter(|u| user::is_human_unixname(u))
            }
            #[cfg(not(fbcode_build))]
            {
                None
            }
        }

        let smc_tier = dep_map
            .get("smc_tier")
            .ok_or(RemoteExecutorDependencyErrors::MissingField("smc_tier"))?;
        let id = dep_map
            .get("id")
            .ok_or(RemoteExecutorDependencyErrors::MissingField("id"))?;
        let interpolate = dep_map.get("enable_interpolation").unwrap_or(&"false");

        let id = if *interpolate == "true" {
            let username: Option<String> = username();
            if let Some(username) = username {
                id.replace("$(username)", &username)
            } else {
                id.replace("$(username)", "")
            }
        } else {
            id.to_string()
        };

        if dep_map.len() > 3 {
            return Err(RemoteExecutorDependencyErrors::UnsupportedFields(
                dep_map.keys().join(", "),
            )
            .into());
        }
        Ok(RemoteExecutorDependency {
            smc_tier: smc_tier.to_string(),
            id,
        })
    }
}

#[derive(Clone, Debug, Display, Eq, PartialEq, Hash, Allocative, Pagable)]
struct RemoteExecutorUseCaseData(String);

interner!(
    USE_CASE_INTERNER,
    BuckHasher,
    RemoteExecutorUseCaseData,
    String,
    str
);

#[derive(Debug, Eq, PartialEq, Copy, Clone, Dupe, Display, Allocative, Pagable)]
pub struct RemoteExecutorUseCase(Intern<RemoteExecutorUseCaseData>);

impl RemoteExecutorUseCase {
    pub fn new(use_case: String) -> Self {
        Self(USE_CASE_INTERNER.intern(RemoteExecutorUseCaseData(use_case)))
    }

    pub fn as_str(&self) -> &'static str {
        self.0.deref_static()
    }

    /// The "buck2-default" use case. This is meant to be used when no use case is configured. It's
    /// not meant to be used for convenience when a use case is not available where it's needed!
    pub fn buck2_default() -> Self {
        static USE_CASE: Lazy<RemoteExecutorUseCase> =
            Lazy::new(|| RemoteExecutorUseCase::new("buck2-default".to_owned()));
        *USE_CASE
    }
}

// The derived PartialEq (which uses pointer equality on the interned data) is still correct.
#[allow(clippy::derived_hash_with_manual_eq)]
impl Hash for RemoteExecutorUseCase {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.0.hash(state);
    }
}

impl FromStr for RemoteExecutorUseCase {
    type Err = buck2_error::Error;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Ok(RemoteExecutorUseCase::new(s.to_owned()))
    }
}

#[derive(Debug, Default, Eq, PartialEq, Clone, Hash, Allocative, Pagable)]
pub struct RemoteExecutorOptions {
    pub re_max_input_files_bytes: Option<u64>,
    pub re_max_queue_time: Option<Duration>,
    pub re_resource_units: Option<i64>,
}

/// The actual executor portion of a RemoteEnabled executor. It's possible for a RemoteEnabled
/// executor to wrap a local executor, which is a glorified way of saying "this is a local executor
/// with a RE backend for caching".
#[derive(Display, Debug, Eq, PartialEq, Clone, Hash, Allocative, Pagable)]
pub enum RemoteEnabledExecutor {
    #[display("local")]
    Local(LocalExecutorOptions),
    #[display("remote")]
    Remote(RemoteExecutorOptions),
    #[display("hybrid")]
    Hybrid {
        local: LocalExecutorOptions,
        remote: RemoteExecutorOptions,
        level: HybridExecutionLevel,
    },
}

/// Normalized `remote_execution::Platform`. Also implements `Eq`, `Hash`.
#[derive(Default, Debug, Clone, PartialEq, Eq, Hash, Pagable, Allocative)]
pub struct RePlatformFields {
    pub properties: Arc<SortedMap<String, String>>,
}

#[derive(Debug, Eq, PartialEq, Clone, Hash, Pagable, Allocative)]
#[allow(clippy::large_enum_variant)]
pub enum Executor {
    /// This executor only runs local commands.
    Local(LocalExecutorOptions),

    /// This executor interacts with a RE backend. It may use that to read or write to caches, or
    /// to execute commands.
    RemoteEnabled(RemoteEnabledExecutorOptions),
    /// Can't run any actions
    None,
}

impl Display for Executor {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Local(options) => {
                write!(
                    f,
                    "Local + use persistent workers {}",
                    options.use_persistent_workers
                )
            }
            Self::RemoteEnabled(options) => {
                let cache = match options.remote_cache_enabled {
                    true => "enabled",
                    false => "disabled",
                };
                let dep_file_cache = match options.remote_dep_file_cache_enabled {
                    true => "enabled",
                    false => "disabled",
                };
                write!(
                    f,
                    "RemoteEnabled + executor {} + remote cache {} + cache upload {} + remote dep file cache {}",
                    options.executor, cache, options.cache_upload_behavior, dep_file_cache
                )
            }
            Self::None => write!(f, "None"),
        }
    }
}

#[derive(Debug, Eq, PartialEq, Clone, Copy, Dupe, Hash, Pagable, Allocative)]
pub enum PathSeparatorKind {
    Unix,
    Windows,
}

impl PathSeparatorKind {
    pub fn system_default() -> Self {
        if cfg!(windows) {
            Self::Windows
        } else {
            Self::Unix
        }
    }
}

/// Controls how we implement output_dirs, output_files, output_paths in RE actions.
#[derive(Debug, Eq, PartialEq, Clone, Copy, Dupe, Hash, Pagable, Allocative)]
pub enum OutputPathsBehavior {
    /// Ask for things as either files or directories.
    Strict,
    /// Ask for things as either directories when certain or files AND directories.
    Compatibility,
    /// Ask for things using output_paths.
    OutputPaths,
}

impl FromStr for OutputPathsBehavior {
    type Err = buck2_error::Error;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "strict" => Ok(OutputPathsBehavior::Strict),
            "compatibility" => Ok(OutputPathsBehavior::Compatibility),
            #[cfg(not(fbcode_build))]
            "output_paths" => Ok(OutputPathsBehavior::OutputPaths),
            _ => Err(buck2_error::buck2_error!(
                buck2_error::ErrorTag::Input,
                "Invalid OutputPathsBehavior: `{}`",
                s
            )),
        }
    }
}

impl Default for OutputPathsBehavior {
    fn default() -> Self {
        if crate::is_open_source() {
            Self::OutputPaths
        } else {
            Self::Compatibility
        }
    }
}

#[derive(
    Display, Debug, Eq, PartialEq, Clone, Copy, Dupe, Hash, Pagable, Allocative
)]
#[derive(Default)]
pub enum CacheUploadBehavior {
    #[display("enabled")]
    Enabled { max_bytes: Option<u64> },
    #[display("disabled")]
    #[default]
    Disabled,
}

#[derive(Debug, Eq, PartialEq, Clone, Copy, Dupe, Hash, Pagable, Allocative)]
pub struct CommandGenerationOptions {
    pub path_separator: PathSeparatorKind,
    pub output_paths_behavior: OutputPathsBehavior,
    pub use_bazel_protocol_remote_persistent_workers: bool,
}

#[derive(Debug, Eq, PartialEq, Hash, Allocative, Clone, Pagable)]
pub struct CommandExecutorConfig {
    pub executor: Executor,
    pub options: CommandGenerationOptions,
}

#[derive(Debug, Eq, PartialEq, Clone, Copy, Dupe, Hash, Pagable, Allocative)]
pub enum HybridExecutionLevel {
    /// Expose both executors but only run it in one preferred executor.
    Limited,
    /// Expose both executors, fallback to the non-preferred executor if execution on the preferred
    /// executor doesn't provide a successful response. By default, we fallback only on errors (i.e.
    /// the infra failed), but not on failures (i.e. the job exited with 1). If
    /// `fallback_on_failure` is set, then we also fallback on failures.
    Fallback { fallback_on_failure: bool },
    /// Race both executors.
    Full {
        fallback_on_failure: bool,
        low_pass_filter: bool,
    },
}

impl CommandExecutorConfig {
    pub fn testing_local() -> Arc<CommandExecutorConfig> {
        Arc::new(CommandExecutorConfig {
            executor: Executor::Local(LocalExecutorOptions::default()),
            options: CommandGenerationOptions {
                path_separator: PathSeparatorKind::system_default(),
                output_paths_behavior: Default::default(),
                use_bazel_protocol_remote_persistent_workers: false,
            },
        })
    }

    pub fn re_cache_enabled(&self) -> bool {
        match &self.executor {
            Executor::Local(_) => false,
            Executor::RemoteEnabled(options) => options.remote_cache_enabled,
            Executor::None => false,
        }
    }
}

/// This struct is used to pass policy info about the action to RE, its data should
/// match the TExecutionPolicy in the RE thrift API.
/// affinity_keys is not defined here because it's already defined in ReActionIdentity
/// duration_ms is not supported because we can't unpack i64 from starlark easily
#[derive(Default, Debug, Clone, Eq, Hash, Pagable, PartialEq, Allocative)]
pub struct RemoteExecutionPolicy {
    pub priority: Option<i32>,
    pub region_preference: Option<String>,
    pub setup_preference_key: Option<String>,
}

/// This struct is used to pass meta internal params to RE
#[derive(Default, Debug, Clone, Eq, Hash, Pagable, PartialEq, Allocative)]
pub struct MetaInternalExtraParams {
    pub remote_execution_policy: RemoteExecutionPolicy,
    pub remote_execution_caf_fbpkgs: Vec<RemoteExecutorCafFbpkg>,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_re_gang_worker_parse_success() {
        let mut worker_map = SmallMap::new();
        worker_map.insert("subplatform", "H100");
        worker_map.insert("rack", "rack_01");

        let result = ReGangWorker::parse(worker_map);
        assert!(result.is_ok());

        let worker = result.unwrap();
        assert_eq!(worker.capabilities.len(), 2);
        assert_eq!(
            worker.capabilities.get("subplatform"),
            Some(&"H100".to_owned())
        );
        assert_eq!(worker.capabilities.get("rack"), Some(&"rack_01".to_owned()));
    }
}
