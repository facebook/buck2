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
        name: str | Artifact,
        # the arguments to write to the argument file
        args,
        # pass to `ctx.actions.write`
        allow_args: bool = False) -> cmd_args:
    if allow_args:
        args_file, _ = actions.write(name, args, allow_args = True, with_inputs = True)
    else:
        args_file = actions.write(name, args, with_inputs = True)
    return cmd_args(args_file, format = "@{}", hidden = args)

# Write arguments to a file, and return the file path as `cmd_args`
# with args attached as hidden artifacts.
def argfile(
        *,
        # ctx.actions
        actions,
        # name of the argument file
        name: str | Artifact,
        # the arguments to write to the argument file
        args,
        # pass to `ctx.actions.write`
        allow_args: bool = False) -> cmd_args:
    if allow_args:
        args_file, _ = actions.write(name, args, allow_args = True, with_inputs = True)
    else:
        args_file = actions.write(name, args, with_inputs = True)
    return cmd_args(args_file, hidden = args)
