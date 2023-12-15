# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

load("@prelude//utils:buckconfig.bzl", "read_list")

def apple_dsym_config() -> dict[str, typing.Any]:
    return {
        "_dsymutil_extra_flags": read_list("apple", "dsymutil_extra_flags", delimiter = " ", default = [], root_cell = True),
    }
