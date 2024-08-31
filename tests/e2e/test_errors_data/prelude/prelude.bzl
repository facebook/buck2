# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

def _impl(_ctx):
    return [DefaultInfo()]

simple_rule = rule(
    impl = _impl,
    attrs = {
        "deps": attrs.list(attrs.one_of(attrs.dep(), attrs.configured_dep()), default = []),
    },
)

def _platform_impl(ctx):
    return [DefaultInfo(), PlatformInfo(label = str(ctx.label.raw_target()), configuration = ConfigurationInfo(constraints = {}, values = {}))]

platform = rule(
    impl = _platform_impl,
    attrs = {},
)
