# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

load(
    "@prelude//linking:shared_libraries.bzl",
    "SharedLibraryInfo",
)

def _impl(ctx: AnalysisContext) -> list[Provider]:
    label = ctx.label.raw_target()
    setting = ConstraintSettingInfo(label = label)
    value = ConstraintValueInfo(setting = setting, label = label)

    return [
        DefaultInfo(),
        SharedLibraryInfo(),
        # A never-satisfied configuration. This is useful as a way to express
        # negation in a select for a target_compatible_with attribute.
        #     target_compatible_with = select({
        #         "DEFAULT": [],
        #         "config//build_mode/constraints:tsan": ["prelude//:none"],
        #     })
        ConfigurationInfo(
            constraints = {label: value},
            values = {},
        ),
    ]

none_rule = rule(
    attrs = {},
    impl = _impl,
    doc = "A rule that produces nothing. Used for no-op dep in a select.",
    is_configuration_rule = True,
)
