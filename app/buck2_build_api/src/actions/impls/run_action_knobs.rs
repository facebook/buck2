/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use buck2_common::file_ops::metadata::TrackedFileDigest;
use buck2_directory::directory::dashmap_directory_interner::DashMapDirectoryInterner;
use buck2_execute::directory::ActionDirectoryMember;
use dice::UserComputationData;
use dupe::Dupe;

/// Knobs controlling how RunAction works.
#[derive(Clone, Dupe, Default)]
pub struct RunActionKnobs {
    /// Process dep files as they are generated.
    pub eager_dep_files: bool,

    /// Whether to try reading from the action output cache (in buck-out/*/offline-cache)
    /// for network actions (download_file, cas_artifact). Used to support offline
    /// builds.
    pub use_network_action_output_cache: bool,

    /// Default for per-action `allow_cache_upload`, to make it opt-out instead of opt-in.
    pub default_allow_cache_upload: bool,

    pub action_paths_interner:
        Option<DashMapDirectoryInterner<ActionDirectoryMember, TrackedFileDigest>>,

    pub deduplicate_get_digests_ttl_calls: bool,
}

pub trait HasRunActionKnobs {
    fn set_run_action_knobs(&mut self, knobs: RunActionKnobs);

    fn get_run_action_knobs(&self) -> &RunActionKnobs;
}

impl HasRunActionKnobs for UserComputationData {
    fn set_run_action_knobs(&mut self, knobs: RunActionKnobs) {
        self.data.set(knobs);
    }

    fn get_run_action_knobs(&self) -> &RunActionKnobs {
        self.data
            .get::<RunActionKnobs>()
            .expect("RunActionKnobs should be set")
    }
}
