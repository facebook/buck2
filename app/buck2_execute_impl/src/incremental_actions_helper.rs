/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use std::sync::Arc;

use allocative::Allocative;
use buck2_core::fs::artifact_path_resolver::ArtifactFs;
use buck2_core::fs::project_rel_path::ProjectRelativePathBuf;
use buck2_execute::execute::request::CommandExecutionOutput;
use buck2_execute::execute::result::CommandExecutionResult;
use buck2_fs::paths::forward_rel_path::ForwardRelativePathBuf;
use starlark_map::small_map::SmallMap;

use crate::sqlite::incremental_state_db::IncrementalDbState;

pub(crate) fn get_incremental_path_map(
    incremental_db_state: &Arc<IncrementalDbState>,
    key: &Option<String>,
) -> Option<Arc<IncrementalPathMap>> {
    if let Some(key) = key {
        incremental_db_state.get(key)
    } else {
        None
    }
}
// Contains the declared short path name to the full content-based hash path
#[derive(Allocative, Clone)]
pub struct IncrementalPathMap(SmallMap<ForwardRelativePathBuf, ProjectRelativePathBuf>);

impl IncrementalPathMap {
    pub fn new(state: SmallMap<ForwardRelativePathBuf, ProjectRelativePathBuf>) -> Self {
        IncrementalPathMap(state)
    }

    pub fn get(&self, key: &ForwardRelativePathBuf) -> Option<&ProjectRelativePathBuf> {
        self.0.get(key)
    }

    pub fn insert(
        &mut self,
        key: ForwardRelativePathBuf,
        value: ProjectRelativePathBuf,
    ) -> Option<ProjectRelativePathBuf> {
        self.0.insert(key, value)
    }

    pub fn iter(&self) -> impl Iterator<Item = (&ForwardRelativePathBuf, &ProjectRelativePathBuf)> {
        self.0.iter()
    }
}

pub(crate) fn save_content_based_incremental_state(
    run_action_key: String,
    incremental_db_state: &Arc<IncrementalDbState>,
    artifact_fs: &ArtifactFs,
    result: &CommandExecutionResult,
) {
    let mut incremental_path_map = SmallMap::new();

    for (k, v) in &result.outputs {
        if let CommandExecutionOutput::BuildArtifact { path, .. } = k
            && path.is_content_based_path()
        {
            let p = path.path().to_buf();
            let content_based_path =
                artifact_fs.resolve_build(path, Some(&v.content_based_path_hash())).
                expect("Non-content-based path provided even though is_content_based_path() returned true");

            incremental_path_map.insert(p, content_based_path);
        }
    }

    // Need to clear the state if there weren't any outputs to prevent stale outputs from being used
    if incremental_path_map.is_empty() {
        incremental_db_state.delete(&run_action_key);
    } else {
        incremental_db_state.insert(
            &run_action_key,
            IncrementalPathMap::new(incremental_path_map),
        );
    }
}
