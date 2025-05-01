/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use allocative::Allocative;
use buck2_core::deferred::dynamic::DynamicLambdaResultsKey;
use dupe::Dupe;

#[derive(
    Clone,
    Dupe,
    Eq,
    PartialEq,
    Hash,
    Debug,
    Allocative,
    derive_more::Display
)]
#[display("{}", self.dynamic_lambda_results_key)]
pub struct DynamicValue {
    pub dynamic_lambda_results_key: DynamicLambdaResultsKey,
}
