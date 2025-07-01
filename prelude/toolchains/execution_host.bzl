# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

load("@prelude//utils:host.bzl", "HostOSTypes")

_ExecutionHostOSTypes = HostOSTypes + ["fat"]  # Fat toolchains are compatible on multiple OSes.

ExecutionHostOSType = enum(*_ExecutionHostOSTypes)
