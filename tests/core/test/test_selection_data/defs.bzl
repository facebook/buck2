# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

def _impl_ok(_ctx):
    return [
        DefaultInfo(),
        ExternalRunnerTestInfo(
            command = ["python3", "-c", "import sys; sys.exit(0)"],
            type = "custom",
        ),
    ]

ok_test = rule(attrs = {}, impl = _impl_ok)

def _impl_fail(_ctx):
    return [
        DefaultInfo(),
        ExternalRunnerTestInfo(
            command = ["python3", "-c", "import sys; sys.exit(1)"],
            type = "custom",
        ),
    ]

fail_test = rule(attrs = {}, impl = _impl_fail)

def _impl_noop(_ctx):
    return [
        DefaultInfo(),
    ]

noop = rule(attrs = {}, impl = _impl_noop)

def _impl_dummy_transition(platform, refs):
    _ignore = (platform, refs)  # buildifier: disable=unused-variable
    return PlatformInfo(
        label = "<dummy>",
        configuration = ConfigurationInfo(constraints = {}, values = {}),
    )

dummy_transition = transition(
    impl = _impl_dummy_transition,
    refs = {},
)

noop_self_transition = rule(
    impl = _impl_noop,
    attrs = {},
    cfg = dummy_transition,
)

def _impl_platform(ctx):
    return [
        DefaultInfo(),
        PlatformInfo(
            label = str(ctx.label.raw_target()),
            configuration = ConfigurationInfo(constraints = {}, values = {}),
        ),
    ]

platform = rule(impl = _impl_platform, attrs = {})
