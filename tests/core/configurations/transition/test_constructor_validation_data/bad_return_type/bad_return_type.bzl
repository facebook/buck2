# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

# @nolint

# This is split transition, return type should be `dict[str, PlatformInfo]`
def _impl(platform: PlatformInfo, refs) -> PlatformInfo:
    _ignore = (platform, refs)
    fail()

bad_return_type = transition(
    impl = _impl,
    refs = {},
    split = True,
)
