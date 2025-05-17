# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

def _run_test_with_content_based_path_impl(ctx):
    unused_but_path_needs_resolving = ctx.actions.declare_output("unused.txt", uses_experimental_content_based_path_hashing = True)
    ctx.actions.write(unused_but_path_needs_resolving, "unused")

    return [
        DefaultInfo(),
        ExternalRunnerTestInfo(
            command = ["python3", "-c", "import sys; sys.exit(0)", cmd_args(unused_but_path_needs_resolving)],
            type = "custom",
        ),
    ]

run_test_with_content_based_path = rule(
    impl = _run_test_with_content_based_path_impl,
    attrs = {
    },
)
