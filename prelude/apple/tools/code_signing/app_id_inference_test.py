# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

import unittest

from .app_id import AppId


class TestAppIdInference(unittest.TestCase):
    def test_unprefixed_first_keychain_group_raises(self) -> None:
        # Only the first keychain access group is read, whatever it looks like.
        with self.assertRaisesRegex(
            RuntimeError, "Malformed app ID string: 'com.apple.token'"
        ):
            AppId.infer_from_entitlements(
                {
                    "keychain-access-groups": [
                        "com.apple.token",
                        "ABCDE12345.com.example.app",
                    ]
                }
            )
