# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

# Information on argsfiles created for compilation.
CompileArgsfile = record(
    # The generated argsfile (does not contain dependent inputs).
    file = field("artifact"),
    # This argsfile as a command form that would use the argsfile (includes dependent inputs).
    cmd_form = field("cmd_args"),
    # Input args necessary for the argsfile to reference.
    input_args = field([["artifacts", "cmd_args"]]),
    # Args as written to the argsfile (with shell quoting applied).
    args = field("cmd_args"),
    # Args aggregated for the argsfile excluding file prefix args (excludes shell quoting).
    args_without_file_prefix_args = field("cmd_args"),
)

CompileArgsfiles = record(
    # Relative path argsfiles used for build actions, mapped by extension.
    relative = field({str.type: CompileArgsfile.type}, default = {}),
    # Absolute path argsfiles used for extra outputs, mapped by extension.
    absolute = field({str.type: CompileArgsfile.type}, default = {}),
)
