/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::collections::BTreeMap;
use std::collections::BTreeSet;

use buck2_core::fs::paths::abs_path::AbsPathBuf;
use serde::Deserialize;
use serde::Serialize;

use crate::TargetCfg;

#[derive(Serialize, Deserialize)]
pub enum NewGenericRequest {
    Materialize(MaterializeRequest),
    DebugEval(DebugEvalRequest),
    Explain(ExplainRequest),
    ExpandExternalCells(ExpandExternalCellsRequest),
    Complete(CompleteRequest),
    Docs(DocsRequest),
}

#[derive(Serialize, Deserialize)]
pub enum NewGenericResponse {
    Materialize(MaterializeResponse),
    DebugEval(DebugEvalResponse),
    Explain(ExplainResponse),
    ExpandExternalCells(ExpandExternalCellsResponse),
    Complete(CompleteResponse),
    Docs(DocsResponse),
}

#[derive(Serialize, Deserialize)]
pub struct MaterializeRequest {
    /// The paths we want to materialize.
    pub paths: Vec<String>,
}

#[derive(Serialize, Deserialize)]
pub struct MaterializeResponse {}

#[derive(Serialize, Deserialize)]
pub struct DebugEvalRequest {
    pub paths: Vec<String>,
}

#[derive(Serialize, Deserialize)]
pub struct DebugEvalResponse {}

#[derive(Serialize, Deserialize)]
pub struct ExplainRequest {
    pub output: Option<AbsPathBuf>,
    pub target: String,
    pub fbs_dump: Option<AbsPathBuf>,
    pub manifold_path: Option<String>,
    pub log_path: AbsPathBuf,
    // build options
    pub target_universe: Vec<String>,
    pub target_cfg: TargetCfg,
}

#[derive(Serialize, Deserialize)]
pub struct ExplainResponse {}

#[derive(Serialize, Deserialize)]
pub enum ExpandExternalCellsRequest {
    All,
    Specific(BTreeSet<String>),
}

#[derive(Serialize, Deserialize)]
pub struct ExpandExternalCellsResponse {
    pub paths: BTreeMap<String, String>,
}

#[derive(Serialize, Deserialize)]
pub struct CompleteRequest {
    pub target_cfg: TargetCfg,
    /// The label with partial target we want to complete \[\[cell\]//\]package:\[partial_target\]
    pub partial_target: String,
}

#[derive(Serialize, Deserialize)]
pub struct CompleteResponse {
    /// Completions matching the partial input.
    pub completions: Vec<String>,
}

#[derive(Serialize, Deserialize)]
pub enum DocsOutputFormat {
    Json,
    /// Contains the markdown output path
    Markdown(AbsPathBuf),
}

#[derive(Serialize, Deserialize)]
pub struct DocsStarlarkRequest {
    pub symbol_patterns: Vec<String>,
    pub format: DocsOutputFormat,
}

#[derive(Serialize, Deserialize)]
pub struct DocsStarlarkBuiltinsRequest {
    pub path: String,
}

#[derive(Serialize, Deserialize)]
pub enum DocsRequest {
    Starlark(DocsStarlarkRequest),
    StarlarkBuiltins(DocsStarlarkBuiltinsRequest),
}

#[derive(Serialize, Deserialize)]
pub struct DocsResponse {
    // Set when requested format is JSON.
    pub json_output: Option<String>,
}
