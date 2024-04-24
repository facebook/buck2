# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

load("@prelude//apple:apple_toolchain_types.bzl", "AppleToolsInfo")
load("@prelude//user:rule_spec.bzl", "RuleRegistrationSpec")

def _impl(ctx: AnalysisContext) -> list[Provider]:
    apple_tools = ctx.attrs._apple_tools[AppleToolsInfo]

    framework_outputs = ctx.attrs.framework.get(DefaultInfo).default_outputs
    if len(framework_outputs) > 1:
        fail("xcframework's framework target must only produce one output")
    framework = framework_outputs[0]

    xcframework_dir = ctx.actions.declare_output(ctx.attrs.name + ".xcframework", dir = True)
    xcframework_command = cmd_args([
        apple_tools.xcframework_maker,
        "--output-path",
        xcframework_dir.as_output(),
        "--framework-path",
        framework,
    ])
    ctx.actions.run(xcframework_command, category = "apple_xcframework")
    return [
        DefaultInfo(default_output = xcframework_dir),
    ]

registration_spec = RuleRegistrationSpec(
    name = "apple_xcframework",
    impl = _impl,
    attrs = {
        "framework": attrs.dep(),
        "_apple_tools": attrs.exec_dep(default = "prelude//apple/tools:apple-tools", providers = [AppleToolsInfo]),
    },
)

def apple_xcframework_extra_attrs():
    attribs = {
        "_apple_tools": attrs.exec_dep(default = "prelude//apple/tools:apple-tools", providers = [AppleToolsInfo]),
    }
    return attribs
