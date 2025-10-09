# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

import datetime
import importlib.resources
import unittest
from pathlib import Path

from .app_id import AppId
from .provisioning_profile_metadata import ProvisioningProfileMetadata


class TestParse(unittest.TestCase):
    def test_canary(self):
        path = Path("test_resources/sample.mobileprovision")
        file_content = (importlib.resources.files(__package__) / path).read_bytes()
        metadata = ProvisioningProfileMetadata.from_provisioning_profile_file_content(
            path, file_content
        )
        self.assertEqual(metadata.uuid, "00000000-0000-0000-0000-000000000000")
        self.assertEqual(
            metadata.get_app_id(), AppId("ABCDE12345", "com.example.TestApp")
        )
        self.assertEqual(
            metadata.expiration_date,
            datetime.datetime.fromisoformat("9999-03-05T01:33:40"),
        )
        self.assertEqual(
            metadata.developer_certificate_fingerprints,
            {"BE16FC419BFB6B59A86BC08755BA0F332EC574FB"},
        )
        self.assertEqual(
            metadata.platforms,
            {"iOS"},
        )

    def test_qualified_entitlements_parsed(self):
        path = Path("test_resources/sample.mobileprovision")
        file_content = (importlib.resources.files(__package__) / path).read_bytes()
        metadata = ProvisioningProfileMetadata.from_provisioning_profile_file_content(
            path, file_content
        )
        self.assertEqual(
            metadata.get_app_id(), AppId("ABCDE12345", "com.example.TestApp")
        )

    def test_filtered_entitlements_stripped_out(self):
        path = Path("test_resources/sample.mobileprovision")
        file_content = (importlib.resources.files(__package__) / path).read_bytes()
        metadata = ProvisioningProfileMetadata.from_provisioning_profile_file_content(
            path, file_content
        )
        self.assertIn(
            "com.apple.developer.icloud-container-development-container-identifiers",
            metadata.entitlements,
        )
        self.assertEqual(
            metadata.get_mergeable_entitlements(),
            {
                "application-identifier": "ABCDE12345.com.example.TestApp",
                "get-task-allow": False,
                "com.apple.developer.team-identifier": "12345ABCDE",
            },
        )
