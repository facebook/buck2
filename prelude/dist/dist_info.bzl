# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

load("@prelude//linking:shared_libraries.bzl", "SharedLibrariesTSet")

# DistInfo is a provider that indicates what other targets/artifacts might be
# necessary to ship alongside the current target as part of a package or other
# distribution mechanism, such as shared libraries, default configuration,
# graphical/media assets, etc.
DistInfo = provider(fields = {
    "shared_libs": provider_field(SharedLibrariesTSet | None, default = None),
})
