# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

# Create an argument file.
# Return `cmd_args` which is single string containing `@path/to/argfile`.
# Returned `cmd_args` contains given files as hidden artifacts.
def at_argfile(
        *,
        # ctx.actions
        actions,
        # name of the argument file
        name: str,
        # the arguments to write to the argument file
        args) -> cmd_args:
    args_file = actions.write(name, args)
    return cmd_args(args_file, format = "@{}", hidden = args)
