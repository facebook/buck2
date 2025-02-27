/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use allocative::Allocative;
use buck2_core::fs::buck_out_path::BuildArtifactPath;
use indexmap::IndexSet;

use crate::analysis::registry::RecordedAnalysisValues;
use crate::artifact_groups::ArtifactGroup;

/// The result of evaluating a bxl function
#[derive(Allocative)]
pub struct BxlResult {
    output_loc: BuildArtifactPath,
    error_loc: BuildArtifactPath,
    artifacts: Vec<ArtifactGroup>,
    analysis_values: RecordedAnalysisValues,
}

impl BxlResult {
    pub fn new(
        output_loc: BuildArtifactPath,
        error_loc: BuildArtifactPath,
        ensured_artifacts: IndexSet<ArtifactGroup>,
        analysis_values: RecordedAnalysisValues,
    ) -> Self {
        Self {
            output_loc,
            error_loc,
            artifacts: ensured_artifacts.into_iter().collect(),
            analysis_values,
        }
    }

    pub(crate) fn analysis_values(&self) -> &RecordedAnalysisValues {
        &self.analysis_values
    }

    pub fn output_loc(&self) -> &BuildArtifactPath {
        &self.output_loc
    }

    pub fn error_loc(&self) -> &BuildArtifactPath {
        &self.error_loc
    }

    pub fn artifacts(&self) -> &Vec<ArtifactGroup> {
        &self.artifacts
    }
}
