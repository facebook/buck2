# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

RemoteTestExecutionToolchainInfo = provider(
    fields = [
        # The profile to use by default.
        "default_profile",
        # A dictionary of string names to pre-registered profiles.  Rules can
        # use the profile name to references these.
        "profiles",
        # A bool indicating whether the test suites executed by this toolchain
        # should be run in a bundle. This makes all tests in a suite run in
        # a single RE action as opposed to one action per test.
        "default_run_as_bundle",
    ],
)
