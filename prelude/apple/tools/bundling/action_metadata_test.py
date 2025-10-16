# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

import importlib.resources
import unittest
from json import JSONDecodeError
from pathlib import Path

from .action_metadata import parse_action_metadata


class TestActionMetadata(unittest.TestCase):
    def _resource_content(self, filename):
        resources = importlib.resources.files(__package__) / "test_resources"
        return resources / filename

    def test_valid_metadata_is_parsed_successfully(self):
        path = self._resource_content("valid_action_metadata.json")
        with path.open() as file_content:
            result = parse_action_metadata(file_content)
        self.assertEqual(
            result,
            {
                Path("repo/foo.txt"): "foo_digest",
                Path("buck-out/bar.txt"): "bar_digest",
            },
        )

    def test_error_when_invalid_metadata(self):
        path = self._resource_content("the.broken_json")
        with self.assertRaises(JSONDecodeError):
            with path.open() as file_content:
                _ = parse_action_metadata(file_content)

    def test_user_friendly_error_when_metadata_with_newer_version(self):
        path = self._resource_content("newer_version_action_metadata.json")
        with self.assertRaises(Exception) as context:
            with path.open() as file_content:
                _ = parse_action_metadata(file_content)
            self.assertEqual(
                context.exception,
                RuntimeError("Expected metadata version to be `1` got `2`."),
            )
