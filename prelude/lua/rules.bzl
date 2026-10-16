# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

load(":cxx_lua_extension.bzl", "cxx_lua_extension")
load(":lua_binary.bzl", "lua_binary")
load(":lua_library.bzl", "lua_library")

rules = {
    "cxx_lua_extension": cxx_lua_extension,
    "lua_binary": lua_binary,
    "lua_library": lua_library,
}
