# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

def _impl(ctx):
    empty_exec = ctx.actions.write("empty.txt", "", is_executable = True)
    copied = ctx.actions.copy_file("copied.txt", empty_exec)
    perms = ctx.actions.declare_output("perms.txt")
    script = cmd_args(
        "ls -l",
        copied,
        "| cut -d ' ' -f 1 >",
        perms.as_output(),
        delimiter = " ",
    )
    ctx.actions.run(cmd_args("bash", "-c", script), category = "test")
    return [
        DefaultInfo(default_output = perms),
    ]

perms_of_copied_file = rule(
    impl = _impl,
    attrs = {},
)
