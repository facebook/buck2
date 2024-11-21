/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use async_trait::async_trait;
use buck2_core::cells::build_file_cell::BuildFileCell;
use buck2_core::package::PackageLabel;
use buck2_node::super_package::SuperPackage;
use dice::DiceComputations;

use crate::interpreter::dice_calculation_delegate::HasCalculationDelegate;

#[async_trait]
pub trait EvalPackageFile {
    async fn eval_package_file(&mut self, path: PackageLabel) -> buck2_error::Result<SuperPackage>;
}

#[async_trait]
impl EvalPackageFile for DiceComputations<'_> {
    async fn eval_package_file(&mut self, path: PackageLabel) -> buck2_error::Result<SuperPackage> {
        let cell_name = path.as_cell_path().cell();
        self.get_interpreter_calculator(cell_name, BuildFileCell::new(cell_name))
            .await?
            .eval_package_file(path)
            .await
    }
}
