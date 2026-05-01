# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

load("@shim//build_defs/lib:oss.bzl", "default_base_module", "translate_target")

def python_library(srcs = [], visibility = ["PUBLIC"], deps = None, base_module = None, **kwargs):
    if deps != None:
        kwargs["deps"] = [translate_target(d) for d in deps]

    if base_module == None:
        base_module = default_base_module()

    # @lint-ignore BUCKLINT: avoid "Direct usage of native rules is not allowed."
    native.python_library(srcs = srcs, visibility = visibility, base_module = base_module, **kwargs)
