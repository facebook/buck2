# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

load("@prelude//android:android_toolchain.bzl", "AndroidToolchainInfo")

def check_for_duplicate_classes_for_pre_dexed_libs(
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
            "--mode",
            "pre-dexed-libs",
            "--validation-output",
            validation_output.as_output(),
            "--target-name-to-class-names-map-file",
            target_name_to_class_names_map_file,
            hidden = target_name_to_class_name_mapping.values(),
        ),
        category = "check_duplicate_classes_pre_dexed_libs",
    )
    return ValidationInfo(
        validations =
            [
                ValidationSpec(name = "duplicate_class_check", validation_result = validation_output),
            ],
    )

def check_for_duplicate_classes_for_non_pre_dexed_jars(
        ctx: AnalysisContext,
        jar_to_owning_target_mapping: dict[Artifact, TargetLabel]) -> ValidationInfo:
    validation_output = ctx.actions.declare_output("validation_output.txt")
    jar_to_owning_target_map_file = ctx.actions.declare_output("jar_to_owning_target_map_file.txt")
    ctx.actions.write_json(
        jar_to_owning_target_map_file,
        jar_to_owning_target_mapping,
    )

    ctx.actions.run(
        cmd_args(
            ctx.attrs._android_toolchain[AndroidToolchainInfo].duplicate_class_checker[RunInfo],
            "--mode",
            "non-pre-dexed-jars",
            "--jar-to-owning-target-map-file",
            jar_to_owning_target_map_file,
            "--validation-output",
            validation_output.as_output(),
            hidden = jar_to_owning_target_mapping.keys(),
        ),
        category = "check_duplicate_classes_non_pre_dexed_jars",
    )
    return ValidationInfo(
        validations =
            [
                ValidationSpec(name = "duplicate_class_check", validation_result = validation_output),
            ],
    )
