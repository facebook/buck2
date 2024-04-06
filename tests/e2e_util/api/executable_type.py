# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

# pyre-unsafe

from enum import Enum


class ExecutableType(Enum):
    """
    Enum for type of buck project we want to test.
    Projects that are supported so far are buck v1 and buck v2.
    """

    buck1 = "buck1"
    buck2 = "buck2"
    # For binaries in buck2/build_api, such as buck-build and bql binaries.
    # Although buck-build and bql binaries have separate parameters, we treat them the same here
    # because we are just differentiating between buck2 binary, which takes a subcommand,
    # and buck2/build_api binaries, which do not.
    buck2_build_api_binary = "buck2_build_api_binary"
    daemon = "daemon"
