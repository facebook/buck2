/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::fmt::Display;
use std::fmt::Formatter;
use std::hash::Hash;
use std::hash::Hasher;
use std::str::FromStr;
use std::sync::Arc;

use allocative::Allocative;
use buck2_core::collections::sorted_map::SortedMap;
use derive_more::Display;
use dupe::Dupe;
use internment_tweaks::Intern;
use internment_tweaks::StaticInterner;
use once_cell::sync::Lazy;

#[derive(Debug, Eq, PartialEq, Copy, Clone, Dupe, Display, Allocative)]
pub struct RemoteExecutorUseCase(Intern<String>);

impl RemoteExecutorUseCase {
    pub fn new(use_case: String) -> Self {
        static USE_CASE_INTERNER: StaticInterner<String> = StaticInterner::new();
        Self(USE_CASE_INTERNER.intern(&use_case))
    }

    pub fn as_str(&self) -> &'static str {
        self.0.deref_static().as_str()
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

#[derive(Debug, Default, Eq, PartialEq, Clone, Hash, Allocative)]
pub struct RemoteExecutorOptions {
    pub re_action_key: Option<String>,
    pub re_max_input_files_bytes: Option<u64>,
    pub re_max_queue_time_ms: Option<u64>,
}

/// The actual executor portion of a RemoteEnabled executor. It's possible for a RemoteEnabled
/// executor to wrap a local executor, which is a glorified way of saying "this is a local executor
/// with a RE backend for caching".
#[derive(Display, Debug, Eq, PartialEq, Clone, Hash, Allocative)]
pub enum RemoteEnabledExecutor {
    #[display(fmt = "local")]
    Local,
    #[display(fmt = "remote")]
    Remote(RemoteExecutorOptions),
    #[display(fmt = "hybrid")]
    Hybrid {
        remote: RemoteExecutorOptions,
        level: HybridExecutionLevel,
    },
}

#[derive(Debug, Eq, PartialEq, Clone, Hash, Allocative)]
pub enum Executor {
    /// This executor only runs local commands.
    Local,

    /// This executor interacts with a RE backend. It may use that to read or write to caches, or
    /// to execute commands.
    RemoteEnabled {
        executor: RemoteEnabledExecutor,
        re_properties: SortedMap<String, String>,
        re_use_case: RemoteExecutorUseCase,
        cache_upload_behavior: CacheUploadBehavior,
        remote_cache_enabled: bool,
    },
}

impl Display for Executor {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Local => {
                write!(f, "Local")
            }
            Self::RemoteEnabled {
                executor,
                re_properties: _,
                re_use_case: _,
                cache_upload_behavior,
                remote_cache_enabled,
            } => {
                let cache = match remote_cache_enabled {
                    true => "enabled",
                    false => "disabled",
                };
                write!(
                    f,
                    "RemoteEnabled + executor {} + remote cache {} + cache upload {}",
                    executor, cache, cache_upload_behavior
                )
            }
        }
    }
}

#[derive(Debug, Eq, PartialEq, Clone, Copy, Dupe, Hash, Allocative)]
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
#[derive(Debug, Eq, PartialEq, Clone, Copy, Dupe, Hash, Allocative)]
pub enum OutputPathsBehavior {
    /// Ask for things as either files or directories.
    Strict,
    /// Ask for things as either directories when certain or files AND directories.
    Compatibility,
    /// Ask for things using output_paths.
    OutputPaths,
}

impl FromStr for OutputPathsBehavior {
    type Err = anyhow::Error;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "strict" => Ok(OutputPathsBehavior::Strict),
            "compatibility" => Ok(OutputPathsBehavior::Compatibility),
            #[cfg(not(fbcode_build))]
            "output_paths" => Ok(OutputPathsBehavior::OutputPaths),
            _ => Err(anyhow::anyhow!("Invalid OutputPathsBehavior: `{}`", s)),
        }
    }
}

impl Default for OutputPathsBehavior {
    fn default() -> Self {
        if buck2_core::is_open_source() {
            Self::OutputPaths
        } else {
            Self::Compatibility
        }
    }
}

#[derive(Display, Debug, Eq, PartialEq, Clone, Copy, Dupe, Hash, Allocative)]
pub enum CacheUploadBehavior {
    #[display(fmt = "enabled")]
    Enabled { max_bytes: Option<u64> },
    #[display(fmt = "disabled")]
    Disabled,
}

impl Default for CacheUploadBehavior {
    fn default() -> Self {
        Self::Disabled
    }
}

#[derive(Debug, Eq, PartialEq, Clone, Copy, Dupe, Hash, Allocative)]
pub struct CommandGenerationOptions {
    pub path_separator: PathSeparatorKind,
    pub output_paths_behavior: OutputPathsBehavior,
}

#[derive(Debug, Eq, PartialEq, Hash, Allocative)]
pub struct CommandExecutorConfig {
    pub executor: Executor,
    pub options: CommandGenerationOptions,
}

#[derive(Debug, Eq, PartialEq, Clone, Copy, Dupe, Hash, Allocative)]
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
            executor: Executor::Local,
            options: CommandGenerationOptions {
                path_separator: PathSeparatorKind::system_default(),
                output_paths_behavior: Default::default(),
            },
        })
    }
}
