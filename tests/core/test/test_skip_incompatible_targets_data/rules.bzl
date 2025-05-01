# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

def _impl(ctx):
    test = ctx.actions.write("test.sh", "echo hello", is_executable = True)
    return [DefaultInfo(), ExternalRunnerTestInfo(type = "custom", command = [test])]

test_rule = rule(
    impl = _impl,
    attrs = {
    },
)
