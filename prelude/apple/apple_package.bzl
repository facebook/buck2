# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

load("@prelude//apple:apple_toolchain_types.bzl", "AppleToolsInfo")
load(":apple_package_config.bzl", "IpaCompressionLevel")
load(":apple_package_types.bzl", "ApplePackageInfo")

def apple_package_impl(ctx: AnalysisContext) -> list[Provider]:
    package_name = ctx.attrs.package_name if ctx.attrs.package_name else ctx.attrs.bundle.label.name
    package = ctx.actions.declare_output("{}.{}".format(package_name, ctx.attrs.ext))

    contents = (
        ctx.attrs.bundle[DefaultInfo].default_outputs[0] if ctx.attrs.packager else _get_ipa_contents(ctx)
    )
    if ctx.attrs.packager:
        process_ipa_cmd = cmd_args([
            ctx.attrs.packager[RunInfo],
            "--app-bundle-path",
            contents,
            "--output-path",
            package.as_output(),
            ctx.attrs.packager_args,
        ])
        category = "apple_package_make_custom"
    else:
        process_ipa_cmd = _get_default_package_cmd(
            ctx,
            contents,
            package.as_output(),
        )
        category = "apple_package_make"

    sub_targets = {}

    prepackaged_validators_artifacts = _get_prepackaged_validators_outputs(ctx, contents)
    if prepackaged_validators_artifacts:
        # Add the artifacts to packaging cmd so that they are run.
        process_ipa_cmd.add(cmd_args(hidden = prepackaged_validators_artifacts))
        sub_targets["prepackaged_validators"] = [
            DefaultInfo(default_outputs = prepackaged_validators_artifacts),
        ]

    ctx.actions.run(process_ipa_cmd, category = category)

    return [DefaultInfo(
        default_output = package,
        sub_targets = sub_targets,
    ), ApplePackageInfo(name = package_name, extension = ctx.attrs.ext, package = package)]

def _get_ipa_contents(ctx: AnalysisContext) -> Artifact:
    ipa_package_dep = ctx.attrs._ipa_package
    default_outputs = ipa_package_dep[DefaultInfo].default_outputs
    if len(default_outputs) != 1:
        fail("Expect exactly one output for .ipa package")
    return default_outputs[0]

def _get_default_package_cmd(ctx: AnalysisContext, unprocessed_ipa_contents: Artifact, output: OutputArtifact) -> cmd_args:
    apple_tools = ctx.attrs._apple_tools[AppleToolsInfo]
    process_ipa_cmd = cmd_args([
        apple_tools.ipa_package_maker,
        "--ipa-contents-dir",
        unprocessed_ipa_contents,
        "--ipa-output-path",
        output,
        "--compression-level",
        _compression_level_arg(IpaCompressionLevel(ctx.attrs._ipa_compression_level)),
    ])

    return process_ipa_cmd

def _compression_level_arg(compression_level: IpaCompressionLevel) -> str:
    if compression_level.value == "none":
        return "0"
    elif compression_level.value == "default":
        return "6"
    elif compression_level.value == "min":
        return "1"
    elif compression_level.value == "max":
        return "9"
    else:
        fail("Unknown .ipa compression level: " + str(compression_level))

def _get_prepackaged_validators_outputs(ctx: AnalysisContext, prepackaged_contents: Artifact) -> list[Artifact]:
    if not ctx.attrs.prepackaged_validators:
        return []

    outputs = []
    for idx, validator in enumerate(ctx.attrs.prepackaged_validators):
        if type(validator) == "tuple":
            validator, validator_args = validator
        else:
            validator = validator
            validator_args = []

        output = ctx.actions.declare_output(validator.label.name + "_{}".format(idx))
        outputs.append(output)

        ctx.actions.run(
            cmd_args([
                validator[RunInfo],
                "--contents-dir",
                prepackaged_contents,
                "--output-path",
                output.as_output(),
                validator_args,
            ]),
            category = "prepackaged_validator",
            identifier = str(idx),
        )

    return outputs
