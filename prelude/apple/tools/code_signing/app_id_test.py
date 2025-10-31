# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

import importlib.resources
import plistlib
import unittest

from .app_id import AppId


class TestAppId(unittest.TestCase):
    def test_string_parsing(self):
        result = AppId.from_string("ABCDE12345.com.example.TestApp")
        expected = AppId("ABCDE12345", "com.example.TestApp")
        self.assertEqual(expected, result)

        result = AppId.from_string("ABCDE12345.*")
        expected = AppId("ABCDE12345", "*")
        self.assertEqual(expected, result)

        with self.assertRaisesRegex(
            RuntimeError,
            "Malformed app ID string: 'invalid.'. We expected a prefix of a ten-character alphanumeric sequence and a Bundle ID which may be a fully-qualified name or a wildcard ending in '*'.",
        ):
            _ = AppId.from_string("invalid.")

    def test_entitlements_parsing(self):
        expected = AppId("ABCDE12345", "com.example.TestApp")
        test_plist_files = [
            "test_resources/test1.plist",
            "test_resources/test2.plist",
            "test_resources/test3.plist",
        ]

        package = importlib.resources.files(__package__)
        for file in test_plist_files:
            with (package / file).open("rb") as f:
                entitlements = plistlib.load(f)
                result = AppId.infer_from_entitlements(entitlements)
                self.assertEqual(expected, result)

        invalid_file = b"""<?xml version="1.0" encoding="UTF-8"?>
                            <!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN" "http://www.apple.com/DTDs/PropertyList-1.0.dtd">
                            <plist version="1.0">
                            <dict>
                                <key>keychain-access-groups</key>
                                <array>
                                    <string>com.facebook.CommonTestHost</string>
                                </array>p
                            </dict>
                            </plist>"""

        invalid_entitlement = plistlib.loads(invalid_file)
        with self.assertRaisesRegex(
            RuntimeError,
            "Error when parsing the entitlements for the app ID: Malformed app ID string: 'com.facebook.CommonTestHost'. "
            "We expected a prefix of a ten-character alphanumeric sequence and a Bundle ID which may be a fully-qualified name or a wildcard ending in '*'.",
        ):
            AppId.infer_from_entitlements(invalid_entitlement)
