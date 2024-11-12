# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

load("@prelude//decls:toolchains_common.bzl", "toolchains_common")

def _attributes() -> dict[str, Attr]:
    return {
        "_test_toolchain": toolchains_common.test_toolchain(),
    }

test_common = struct(
    attributes = _attributes,
)
