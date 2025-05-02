# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

load("@prelude//android:android_toolchain.bzl", "AndroidToolchainInfo")

def check_for_duplicate_classes(
        ctx: AnalysisContext,
        target_name_to_class_name_mapping: dict[str, Artifact]) -> ValidationInfo:
    validation_output = ctx.actions.declare_output("validation_output.txt")
    target_name_to_class_names_map_file = ctx.actions.declare_output("target_name_to_class_names_map_file.txt")
    ctx.actions.write_json(
        target_name_to_class_names_map_file,
        target_name_to_class_name_mapping,
    )

    ctx.actions.run(
        cmd_args(
            ctx.attrs._android_toolchain[AndroidToolchainInfo].duplicate_class_checker[RunInfo],
            "--validation-output",
            validation_output.as_output(),
            "--target-name-to-class-names-map-file",
            target_name_to_class_names_map_file,
            hidden = target_name_to_class_name_mapping.values(),
        ),
        category = "check_duplicate_classes",
    )
    return ValidationInfo(
        validations =
            [
                ValidationSpec(name = "duplicate_class_check", validation_result = validation_output),
            ],
    )
