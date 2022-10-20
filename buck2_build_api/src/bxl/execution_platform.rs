/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

//! Common utilities for bxl
use buck2_common::executor_config::CacheUploadBehavior;
use buck2_common::executor_config::CommandExecutorConfig;
use buck2_common::executor_config::CommandExecutorKind;
use buck2_common::executor_config::LocalExecutorOptions;
use buck2_common::executor_config::PathSeparatorKind;
use buck2_core::configuration::Configuration;
use buck2_node::configuration::execution::ExecutionPlatform;
use buck2_node::configuration::execution::ExecutionPlatformResolution;
use once_cell::sync::Lazy;

// TODO(bobyf) this should be configured by the bxl function similar to
// execution groups, which we don't have yet. Currently we hard code this to
// local since IDE will mostly be invoking local tools anyways
pub static EXECUTION_PLATFORM: Lazy<ExecutionPlatformResolution> = Lazy::new(|| {
    ExecutionPlatformResolution::new(
        Some(ExecutionPlatform::legacy_execution_platform(
            CommandExecutorConfig {
                executor_kind: CommandExecutorKind::Local(LocalExecutorOptions {}),
                path_separator: PathSeparatorKind::system_default(),
                cache_upload_behavior: CacheUploadBehavior::Disabled,
            },
            Configuration::unspecified(),
        )),
        vec![],
    )
});
