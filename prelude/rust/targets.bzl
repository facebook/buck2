# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

load("@prelude//os_lookup:defs.bzl", "OsLookup")

_OS_TRIPLES = {
    ("linux", "arm64"): "aarch64-unknown-linux-gnu",
    ("linux", "x86_64"): "x86_64-unknown-linux-gnu",
    ("macos", "aarch64"): "aarch64-apple-darwin",
    ("macos", "x86_64"): "x86_64-apple-darwin",
    ("windows", "aarch64"): "aarch64-pc-windows-gnu",
    ("windows", "x86_64"): "x86_64-pc-windows-gnu",
}

def _exec_triple(ctx: "context") -> [str.type, None]:
    exec_os = ctx.attrs._exec_os_type[OsLookup]
    return _OS_TRIPLES.get((exec_os.platform, exec_os.cpu))

targets = struct(
    exec_triple = _exec_triple,
)
