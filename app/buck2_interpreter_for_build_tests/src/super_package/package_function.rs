/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use buck2_core::cells::build_file_cell::BuildFileCell;
use buck2_core::fs::project::ProjectRootTemp;
use buck2_core::package::PackageLabel;
use buck2_interpreter::starlark_profiler::StarlarkProfilerOrInstrumentation;
use buck2_interpreter_for_build::interpreter::dice_calculation_delegate::HasCalculationDelegate;

use crate::tests::calculation;
use crate::tests::root_cell;

#[tokio::test]
async fn test_package() {
    let fs = ProjectRootTemp::new().unwrap();

    fs.write_file(
        "juxtaposition/PACKAGE",
        r#"
package(
    visibility = ["//aaa/..."],
    within_view = ["//bbb/..."],
    inherit = True,
)
"#,
    );
    fs.write_file("juxtaposition/BUCK", "");

    let ctx = calculation(&fs).await;

    let interpreter = ctx
        .get_interpreter_calculator(root_cell(), BuildFileCell::new(root_cell()))
        .await
        .unwrap();

    let _result = interpreter
        .eval_build_file(
            PackageLabel::testing_parse("root//juxtaposition"),
            &mut StarlarkProfilerOrInstrumentation::disabled(),
        )
        .await
        .unwrap();
}
