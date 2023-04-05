# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

# We only want to document the rules in the prelude, so have a module that
# only reexports those.

load("@prelude//:native.bzl", _native = "native")

def _native_rules():
    res = {}
    for key in dir(_native):
        val = getattr(_native, key)
        if type(val) == "rule":
            res[key] = val
    return struct(**res)

native = _native_rules()
