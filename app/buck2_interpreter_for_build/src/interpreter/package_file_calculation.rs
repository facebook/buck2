/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use async_trait::async_trait;
use buck2_interpreter::paths::package::PackageFilePath;
use buck2_node::super_package::SuperPackage;
use dice::DiceComputations;

use crate::interpreter::dice_calculation_delegate::HasCalculationDelegate;

#[async_trait]
pub trait EvalPackageFile {
    async fn eval_package_file(&self, path: &PackageFilePath) -> anyhow::Result<SuperPackage>;
}

#[async_trait]
impl EvalPackageFile for DiceComputations {
    async fn eval_package_file(&self, path: &PackageFilePath) -> anyhow::Result<SuperPackage> {
        self.get_interpreter_calculator(path.cell(), path.build_file_cell())
            .await?
            .eval_package_file(path)
            .await
    }
}
