# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

# We don't want e.g. Apple simulator and Android emulator targets to be configured differently and handled as a different resource broker by buck2 core.
# By clearing the platform we make sure there is only a single configured target for each resource broker which manages resources of certain type.
def _transition_impl(platform: PlatformInfo, refs: struct) -> PlatformInfo:
    # buildifier: disable=unused-variable
    _ = (platform, refs)
    return PlatformInfo(
        label = "empty_platform",
        configuration = ConfigurationInfo(
            constraints = {},
            values = {},
        ),
    )

clear_platform_transition = transition(
    impl = _transition_impl,
    refs = {},
)
