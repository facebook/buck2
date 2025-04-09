# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

def _impl(platform, refs):
    _ = refs  # buildifier: disable=unused-variable
    return PlatformInfo(
        # Increase the length of the label, so it overflows (incorrectly).
        label = platform.label + "!hello!",
        configuration = platform.configuration,
    )

transition_increase_label_len = transition(impl = _impl, refs = {})
