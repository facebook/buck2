# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

load(":matlab_program.bzl", "matlab_program_impl")
load(":matlab_toolchain.bzl", "matlab_toolchain")

implemented_rules = {
    "matlab_program": matlab_program_impl,
}

extra_attributes = {
    "matlab_program": {
        "main": attrs.source(),
        "_matlab_toolchain": matlab_toolchain(),
    },
}
