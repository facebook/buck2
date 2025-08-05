# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

load("@prelude//toolchains/go:system_go_bootstrap_toolchain.bzl", _system_go_bootstrap_toolchain = "system_go_bootstrap_toolchain")
load("@prelude//toolchains/go:system_go_toolchain.bzl", _system_go_toolchain = "system_go_toolchain")

# deprecated: use `@prelude///toolchains/go:system_go_bootstrap_toolchain.bzl` instead
system_go_bootstrap_toolchain = _system_go_bootstrap_toolchain

# deprecated: use `@prelude///toolchains/go:system_go_toolchain.bzl` instead
system_go_toolchain = _system_go_toolchain
