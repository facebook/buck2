/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::sync::atomic::{AtomicU64, Ordering};

use anyhow::Context as _;
use buck2_core::{
    fs::paths::{ForwardRelativePath, ForwardRelativePathBuf},
    provider::ConfiguredProvidersLabel,
};
use chrono::Local;
use dashmap::DashMap;
use test_api::data::ConfiguredTargetHandle;

/// The state of a buck2 test command.
pub struct TestSession {
    /// The next ConfiguredTargetHandle that will be assigned.
    next_id: AtomicU64,
    /// A mapping of ConfiguredTargetHandle (which Tpx can use with) to the underlying provider in
    /// Buck2.
    labels: DashMap<ConfiguredTargetHandle, ConfiguredProvidersLabel>,
    /// The prefix to assign to all paths for this test session. This isn't used to provide any
    /// uniqueness (at least not at this time), but it's helpful to group outputs in a way that
    /// more-or-less matches a given test session.
    prefix: ForwardRelativePathBuf,
    /// Whether this session should allow things to run on RE.
    allow_re: bool,
}

impl TestSession {
    pub fn new() -> Self {
        // NOTE: This is the format that Tpx has historically used. We don't really *have* to use
        // this considering we don't even put it in the same place (we do it in ./buck-out/v2/tmp,
        // but Tpx put it in /tmp), but it's a reasonable one.
        let now = Local::now();
        let now = now.format("%Y%m%d-%H%M%S").to_string();
        let prefix = ForwardRelativePathBuf::unchecked_new(now);

        Self {
            next_id: AtomicU64::new(0),
            labels: DashMap::new(),
            prefix,
            allow_re: false,
        }
    }

    pub fn enable_re(&mut self) {
        self.allow_re = true;
    }

    pub fn allows_re(&self) -> bool {
        self.allow_re
    }

    pub fn prefix(&self) -> &ForwardRelativePath {
        self.prefix.as_ref()
    }

    /// Insert a new provider and retrieve the matching handle.
    pub fn register(&self, label: ConfiguredProvidersLabel) -> ConfiguredTargetHandle {
        let id = self.next_id.fetch_add(1, Ordering::Relaxed).into();
        let inserted = self.labels.insert(id, label).is_none();
        assert!(inserted);
        id
    }

    /// Retrieve the provider for a given handle.
    pub fn get(&self, id: ConfiguredTargetHandle) -> anyhow::Result<ConfiguredProvidersLabel> {
        let res = self
            .labels
            .get(&id)
            .with_context(|| format!("Invalid id provided to TestSession: {:?}", id))?;

        Ok(res.clone())
    }
}
