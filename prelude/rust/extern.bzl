# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

load(":context.bzl", "CrateName")

# Create `--extern` flag. For crates with a name computed during analysis:
#
#     --extern=NAME=path/to/libNAME.rlib
#
# For crates with a name computed during build:
#
#     --extern=$(cat path/to/REALNAME)=path/to/libPROVISIONAL.rlib
#
def extern_arg(flags: list[str], crate: CrateName, lib: Artifact) -> cmd_args:
    if flags == []:
        flags = ""
    else:
        flags = ",".join(flags) + ":"

    if crate.dynamic:
        # TODO: consider using `cmd_args(crate.dynamic, quote = "json")` so it
        # doesn't fall apart on paths containing ')'
        crate_name = cmd_args(crate.dynamic, format = "$(cat {})")
    else:
        crate_name = crate.simple

    return cmd_args("--extern=", flags, crate_name, "=", lib, delimiter = "")

# Create `--crate-map` flag. For crates with a name computed during analysis:
#
#     --crate-map=NAME=//path/to:target
#
# For crates with a name computed during build:
#
#     --crate-map=$(cat path/to/REALNAME)=//path/to:target
#
def crate_map_arg(crate: CrateName, label: Label) -> cmd_args:
    if crate.dynamic:
        crate_name = cmd_args(crate.dynamic, format = "$(cat {})")
    else:
        crate_name = crate.simple

    return cmd_args("--crate-map=", crate_name, "=", str(label.raw_target()), delimiter = "")
