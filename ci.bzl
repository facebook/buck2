# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

load("@fbcode//target_determinator/macros:ci_package.bzl", "ci_package")
load("@fbcode//target_determinator/macros:labels.bzl", "ci")

# We want to test most things on all platforms we support, but for various reasons,
# this PACKAGE should be treated as Linux-only.
def ci_linux_only():
    ci_package.set(
        [ci.linux()],
        overwrite = True,
    )
