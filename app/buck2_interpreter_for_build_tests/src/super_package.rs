/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

//! Tests for `PACKAGE` files.

use assert_matches::assert_matches;
use buck2_core::cells::build_file_cell::BuildFileCell;
use buck2_core::fs::project::ProjectRootTemp;
use buck2_core::package::testing::PackageExt;
use buck2_core::package::PackageLabel;
use buck2_interpreter::starlark_profiler::StarlarkProfilerOrInstrumentation;
use buck2_interpreter_for_build::interpreter::dice_calculation_delegate::HasCalculationDelegate;

use crate::tests::calculation;
use crate::tests::root_cell;

// `PACKAGE` files do not do anything useful yet, so test them only by failing.

#[tokio::test]
async fn test_same_dir_package_file_evaluated() {
    let fs = ProjectRootTemp::new().unwrap();

    fs.write_file("headphones/PACKAGE", "fail('keyboard')");
    fs.write_file("headphones/BUCK", "");

    let ctx = calculation(&fs);
    let interpreter = ctx
        .get_interpreter_calculator(root_cell(), BuildFileCell::new(root_cell()))
        .await
        .unwrap();

    let result = interpreter
        .eval_build_file(
            PackageLabel::testing_new("root", "headphones"),
            &mut StarlarkProfilerOrInstrumentation::disabled(),
        )
        .await;
    assert_matches!(result, Err(e) if format!("{:?}", e).contains("keyboard"));
}

#[tokio::test]
async fn test_parent_file_evaluated() {
    let fs = ProjectRootTemp::new().unwrap();

    fs.write_file("PACKAGE", "fail('mouse')");
    fs.write_file("trackpad/BUCK", "");

    let ctx = calculation(&fs);
    let interpreter = ctx
        .get_interpreter_calculator(root_cell(), BuildFileCell::new(root_cell()))
        .await
        .unwrap();

    let result = interpreter
        .eval_build_file(
            PackageLabel::testing_new("root", "trackpad"),
            &mut StarlarkProfilerOrInstrumentation::disabled(),
        )
        .await;
    assert_matches!(result, Err(e) if format!("{:?}", e).contains("mouse"));
}
