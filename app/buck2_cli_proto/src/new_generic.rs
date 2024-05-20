/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use buck2_core::fs::paths::abs_path::AbsPathBuf;
use serde::Deserialize;
use serde::Serialize;

#[derive(Serialize, Deserialize)]
pub enum NewGenericRequest {
    Materialize(MaterializeRequest),
    DebugEval(DebugEvalRequest),
    Explain(ExplainRequest),
    ExpandExternalCell(ExpandExternalCellRequest),
}

#[derive(Serialize, Deserialize)]
pub enum NewGenericResponse {
    Materialize(MaterializeResponse),
    DebugEval(DebugEvalResponse),
    Explain(ExplainResponse),
    ExpandExternalCell(ExpandExternalCellResponse),
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
    pub allow_vpnless: bool,
    pub manifold_path: Option<String>,
}

#[derive(Serialize, Deserialize)]
pub struct ExplainResponse {}

#[derive(Serialize, Deserialize)]
pub struct ExpandExternalCellRequest {
    pub cell_name: String,
}

#[derive(Serialize, Deserialize)]
pub struct ExpandExternalCellResponse {
    pub path: String,
}
