# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

load("@fbsource//tools/target_determinator/macros:ci.bzl", "ci")

# We want to test most things on all platforms we support, but for various reasons,
# this PACKAGE should be treated as Linux-only.
def ci_linux_only():
    ci.package(
        [ci.linux()],
        overwrite = True,
    )
