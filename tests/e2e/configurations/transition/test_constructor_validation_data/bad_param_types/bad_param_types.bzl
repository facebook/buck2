# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

# @nolint

# Type of `platform` must be `PlatformInfo`.
def _impl(platform: str, refs):
    _ignore = (platform, refs)
    pass

bad_param_types = transition(
    impl = _impl,
    refs = {},
)
