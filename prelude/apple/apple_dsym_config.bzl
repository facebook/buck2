# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

load("@prelude//utils:buckconfig.bzl", "read_choice", "read_list")

def apple_dsym_config() -> dict[str, typing.Any]:
    return {
        "_dsymutil_extra_flags": read_list("apple", "dsymutil_extra_flags", delimiter = " ", default = [], root_cell = True),
        # The default value of `--verify-dwarf` depends on the toolchain build mode. Default to `none` to unify behavior.
        # https://github.com/llvm/llvm-project/blob/e3eb12cce97fa75d1d2443bcc2c2b26aa660fe34/llvm/tools/dsymutil/dsymutil.cpp#L94-L98
        "_dsymutil_verify_dwarf": read_choice("apple", "dsymutil_verify_dwarf", choices = ["none", "input", "output", "all", "auto"], default = "none", root_cell = True),
    }
