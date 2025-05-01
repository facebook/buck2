# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

load("@prelude//apple:apple_toolchain_types.bzl", "AppleToolsInfo")
load("@prelude//apple/user:apple_xcframework.bzl", "XCFrameworkInfo")
load("@prelude//user:rule_spec.bzl", "RuleRegistrationSpec")
load("@prelude//utils:utils.bzl", "flatten")

def _apple_spm_package_impl(ctx: AnalysisContext) -> list[Provider]:
    apple_tools = ctx.attrs._apple_tools[AppleToolsInfo]

    spm_packager = apple_tools.spm_packager

    output_dir = ctx.actions.declare_output(ctx.attrs.name, dir = True)

    xcframework_binaries = [dep for dep in ctx.attrs.deps if dep.get(XCFrameworkInfo)]
    binary_args = flatten([["--xcframework", dep[XCFrameworkInfo].name, dep[DefaultInfo].default_outputs[0]] for dep in xcframework_binaries])

    spm_command = cmd_args([
        spm_packager,
        "--output-path",
        output_dir.as_output(),
        "--package-name",
        ctx.attrs.package_name,
    ] + binary_args)

    ctx.actions.run(spm_command, category = "assemble_spm_package")

    return [
        DefaultInfo(default_output = output_dir),
    ]

registration_spec = RuleRegistrationSpec(
    name = "apple_spm_package",
    impl = _apple_spm_package_impl,
    attrs = {
        "deps": attrs.list(attrs.dep(), default = []),
        "package_name": attrs.string(),
        "_apple_tools": attrs.exec_dep(default = "prelude//apple/tools:apple-tools", providers = [AppleToolsInfo]),
    },
)

def apple_spm_package_extra_attrs():
    attribs = {
        "_apple_tools": attrs.exec_dep(default = "prelude//apple/tools:apple-tools", providers = [AppleToolsInfo]),
    }
    return attribs
