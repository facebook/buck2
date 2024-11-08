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
use crate::bxl::build_result::BxlBuildResult;

/// The result of evaluating a bxl function
#[derive(Allocative)]
pub enum BxlResult {
    /// represents that the bxl function has no built results
    None {
        output_loc: BuildArtifactPath,
        error_loc: BuildArtifactPath,
        analysis_values: RecordedAnalysisValues,
    },
    /// a bxl that deals with builds
    BuildsArtifacts {
        output_loc: BuildArtifactPath,
        error_loc: BuildArtifactPath,
        built: Vec<BxlBuildResult>,
        artifacts: Vec<ArtifactGroup>,
        analysis_values: RecordedAnalysisValues,
    },
}

impl BxlResult {
    pub fn new(
        output_loc: BuildArtifactPath,
        error_loc: BuildArtifactPath,
        ensured_artifacts: IndexSet<ArtifactGroup>,
        analysis_values: RecordedAnalysisValues,
    ) -> Self {
        if ensured_artifacts.is_empty() {
            Self::None {
                output_loc,
                error_loc,
                analysis_values,
            }
        } else {
            Self::BuildsArtifacts {
                output_loc,
                error_loc,
                built: vec![],
                artifacts: ensured_artifacts.into_iter().collect(),
                analysis_values,
            }
        }
    }

    pub(crate) fn analysis_values(&self) -> &RecordedAnalysisValues {
        match self {
            BxlResult::None {
                analysis_values, ..
            } => analysis_values,
            BxlResult::BuildsArtifacts {
                analysis_values, ..
            } => analysis_values,
        }
    }

    pub fn get_output_loc(&self) -> &BuildArtifactPath {
        match self {
            BxlResult::None { output_loc, .. } => output_loc,
            BxlResult::BuildsArtifacts { output_loc, .. } => output_loc,
        }
    }

    pub fn get_error_loc(&self) -> &BuildArtifactPath {
        match self {
            BxlResult::None { error_loc, .. } => error_loc,
            BxlResult::BuildsArtifacts { error_loc, .. } => error_loc,
        }
    }

    pub fn get_artifacts_opt(&self) -> Option<&Vec<ArtifactGroup>> {
        match self {
            BxlResult::None { .. } => None,
            BxlResult::BuildsArtifacts { artifacts, .. } => Some(artifacts),
        }
    }

    pub fn get_build_result_opt(&self) -> Option<&Vec<BxlBuildResult>> {
        match self {
            BxlResult::None { .. } => None,
            BxlResult::BuildsArtifacts { built, .. } => Some(built),
        }
    }
}
