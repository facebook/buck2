# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

load("@prelude//android:android_toolchain.bzl", "AndroidToolchainInfo")
load("@prelude//utils:argfile.bzl", "argfile")

def check_for_duplicate_classes(
        ctx: AnalysisContext,
        target_name_to_class_name_mapping: dict[str, Artifact]) -> ValidationInfo:
    validation_output = ctx.actions.declare_output("validation_output.txt")
    target_name_file = ctx.actions.declare_output("target_names.txt")
    target_name_files_argfile = argfile(
        actions = ctx.actions,
        name = target_name_file,
        args = list(target_name_to_class_name_mapping.keys()),
    )
    class_name_paths_file = ctx.actions.declare_output("class_name_files.txt")
    class_name_paths_files_arg_file = argfile(
        actions = ctx.actions,
        name = class_name_paths_file,
        args = [target_name_to_class_name_mapping[target_name] for target_name in target_name_to_class_name_mapping],
    )

    ctx.actions.run(
        cmd_args(
            [
                ctx.attrs._android_toolchain[AndroidToolchainInfo].duplicate_class_checker[RunInfo],
                "--validation-output",
                validation_output.as_output(),
                "--target-names-file",
                target_name_files_argfile,
                "--class-name-paths-file",
                class_name_paths_files_arg_file,
            ],
        ),
        category = "check_duplicate_classes",
    )
    return ValidationInfo(
        validations =
            [
                ValidationSpec(name = "duplicate_class_check", validation_result = validation_output),
            ],
    )
