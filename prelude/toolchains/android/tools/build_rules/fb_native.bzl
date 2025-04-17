# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

# @oss-disable[end= ]: load("@fbsource//tools/build_defs:fb_native_wrapper.bzl", _fb_native = "fb_native")
load("@prelude//:native.bzl", _fb_native = "native") # @oss-enable

fb_native = _fb_native
