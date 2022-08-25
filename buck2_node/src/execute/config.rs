/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::hash::Hash;
use std::hash::Hasher;

use gazebo::prelude::Dupe;
use internment_tweaks::Intern;
use internment_tweaks::StaticInterner;
use once_cell::sync::Lazy;
use starlark_map::small_map::SmallMap;

#[derive(Debug, Eq, Hash, PartialEq, Clone, Dupe)]
pub struct LocalExecutorOptions {}

#[derive(Debug, Eq, PartialEq, Copy, Clone, Dupe)]
pub struct RemoteExecutorUseCase(Intern<String>);

impl RemoteExecutorUseCase {
    pub fn new(use_case: String) -> Self {
        static USE_CASE_INTERNER: StaticInterner<String> = StaticInterner::new();
        Self(USE_CASE_INTERNER.intern(use_case))
    }

    pub fn as_str(&self) -> &'static str {
        self.0.deref_static().as_str()
    }

    /// The "buck2-default" use case. This is meant to be used when no use case is configured. It's
    /// not meant to be used for convenience when a use case is not available where it's needed!
    pub fn buck2_default() -> &'static Self {
        static USE_CASE: Lazy<RemoteExecutorUseCase> =
            Lazy::new(|| RemoteExecutorUseCase::new("buck2-default".to_owned()));
        &USE_CASE
    }
}

// The derived PartialEq (which uses pointer equality on the interned data) is still correct.
#[allow(clippy::derive_hash_xor_eq)]
impl Hash for RemoteExecutorUseCase {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.0.hash(state);
    }
}

impl Default for RemoteExecutorUseCase {
    fn default() -> Self {
        Self::new("buck2-default".to_owned())
    }
}

#[derive(Debug, Eq, PartialEq, Clone, Hash)]
pub struct RemoteExecutorOptions {
    pub re_properties: SmallMap<String, String>,
    pub re_action_key: Option<String>,
    pub re_max_input_files_bytes: Option<u64>,
    pub re_use_case: RemoteExecutorUseCase,
}

impl Default for RemoteExecutorOptions {
    fn default() -> Self {
        Self {
            re_properties: Default::default(),
            re_action_key: Default::default(),
            re_max_input_files_bytes: Default::default(),
            re_use_case: RemoteExecutorUseCase::new("buck2-default".to_owned()),
        }
    }
}

#[derive(Debug, thiserror::Error)]
enum ExecutorConfigError {
    #[error("Action executor config must have at least one of local or remote options")]
    MissingLocalAndRemote,
}

#[derive(Debug, Eq, PartialEq, Clone, Hash)]
pub enum CommandExecutorKind {
    Local(LocalExecutorOptions),
    Remote(RemoteExecutorOptions),
    Hybrid {
        local: LocalExecutorOptions,
        remote: RemoteExecutorOptions,
        level: HybridExecutionLevel,
    },
}

#[derive(Debug, Eq, PartialEq, Clone, Copy, Dupe, Hash)]
pub enum PathSeparatorKind {
    Unix,
    Windows,
}

#[derive(Debug, Eq, PartialEq, Clone, Hash)]
pub struct CommandExecutorConfig {
    pub executor_kind: CommandExecutorKind,
    pub path_separator: PathSeparatorKind,
}

#[derive(Debug, Eq, PartialEq, Clone, Copy, Dupe, Hash)]
pub enum HybridExecutionLevel {
    /// Expose both executors but only run it in one preferred executor.
    Limited,
    /// Expose both executors, fallback to the non-preferred executor if execution on the preferred
    /// executor doesn't provide a successful response. By default, we fallback only on errors (i.e.
    /// the infra failed), but not on failures (i.e. the job exited with 1). If
    /// `fallback_on_failure` is set, then we also fallback on failures.
    Fallback { fallback_on_failure: bool },
    /// Race both executors.
    Full { fallback_on_failure: bool },
}

impl CommandExecutorKind {
    pub fn new(
        local: Option<LocalExecutorOptions>,
        remote: Option<RemoteExecutorOptions>,
        hybrid_level: HybridExecutionLevel,
    ) -> anyhow::Result<Self> {
        match (local, remote) {
            (None, None) => Err(ExecutorConfigError::MissingLocalAndRemote.into()),
            (None, Some(remote)) => Ok(Self::Remote(remote)),
            (Some(local), None) => Ok(Self::Local(local)),
            (Some(local), Some(remote)) => Ok(Self::Hybrid {
                local,
                remote,
                level: hybrid_level,
            }),
        }
    }
}

impl CommandExecutorConfig {
    pub fn new(executor_kind: CommandExecutorKind, path_separator: PathSeparatorKind) -> Self {
        Self {
            executor_kind,
            path_separator,
        }
    }

    pub fn new_with_default_path_separator(executor_kind: CommandExecutorKind) -> Self {
        Self {
            executor_kind,
            path_separator: if cfg!(windows) {
                PathSeparatorKind::Windows
            } else {
                PathSeparatorKind::Unix
            },
        }
    }

    pub fn testing_local() -> Self {
        Self::new_with_default_path_separator(CommandExecutorKind::Local(LocalExecutorOptions {}))
    }
}
