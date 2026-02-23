/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use core::fmt;
use std::sync::atomic::AtomicU64;
use std::sync::atomic::Ordering;

use allocative::Allocative;
use buck2_core::provider::label::ConfiguredProvidersLabel;
use buck2_error::internal_error;
use buck2_test_api::data::ConfiguredTargetHandle;
use dashmap::DashMap;
use dupe::Dupe;

#[derive(Debug, Clone, Copy, Dupe, Default, Allocative, PartialEq, Hash, Eq)]
pub struct TestSessionOptions {
    /// Whether this session should allow things to run on RE.
    pub allow_re: bool,
    pub force_use_project_relative_paths: bool,
    pub force_run_from_project_root: bool,
}

impl fmt::Display for TestSessionOptions {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "allow_re = {}, force_use_project_relative_paths = {}, force_run_from_project_root = {}",
            self.allow_re, self.force_use_project_relative_paths, self.force_run_from_project_root
        )
    }
}

/// The state of a buck2 test command.
pub struct TestSession {
    /// The next ConfiguredTargetHandle that will be assigned.
    next_id: AtomicU64,
    /// A mapping of ConfiguredTargetHandle (which Tpx can use with) to the underlying provider in
    /// Buck2.
    labels: DashMap<ConfiguredTargetHandle, ConfiguredProvidersLabel>,
    /// Options overriding the behavior of tests executed in this session. This is primarily
    /// intended for unstable or debugging features.
    options: TestSessionOptions,
}

impl TestSession {
    pub fn new(options: TestSessionOptions) -> Self {
        Self {
            next_id: AtomicU64::new(0),
            labels: DashMap::new(),
            options,
        }
    }

    pub fn options(&self) -> TestSessionOptions {
        self.options
    }

    /// Insert a new provider and retrieve the matching handle.
    pub fn register(&self, label: ConfiguredProvidersLabel) -> ConfiguredTargetHandle {
        let id = self.next_id.fetch_add(1, Ordering::Relaxed).into();
        let inserted = self.labels.insert(id, label).is_none();
        assert!(inserted);
        id
    }

    /// Retrieve the provider for a given handle.
    pub fn get(&self, id: ConfiguredTargetHandle) -> buck2_error::Result<ConfiguredProvidersLabel> {
        let res = self
            .labels
            .get(&id)
            .ok_or_else(|| internal_error!("Invalid id provided to TestSession: {id:?}"))?;

        Ok(res.clone())
    }
}
