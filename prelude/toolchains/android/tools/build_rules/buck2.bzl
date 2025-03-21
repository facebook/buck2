# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

load("@prelude//:native.bzl", "native")

def enable_buck2_bootstrap_prebuilts():
    return native.read_config("java", "enable_bootstrap_prebuilts", "") in ["true", "True", "1", "yes"]
