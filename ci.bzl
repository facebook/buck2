# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

load("@fbsource//tools/target_determinator/macros:ci.bzl", "ci")

# We want to test most things on all platforms we support, but for various reasons,
# this PACKAGE should be treated as Linux-only.
def ci_linux_only():
    ci.package(
        [ci.linux()],
        overwrite = True,
    )
