# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

load("@prelude//utils:host.bzl", "HostOSTypes")

_ExecutionHostOSTypes = HostOSTypes + ["fat"]  # Fat toolchains are compatible on multiple OSes.

ExecutionHostOSType = enum(*_ExecutionHostOSTypes)
