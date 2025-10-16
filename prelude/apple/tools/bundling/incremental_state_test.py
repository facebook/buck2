# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

import importlib.resources
import io
import json
import unittest
from pathlib import Path

from .incremental_state import (
    CodesignedOnCopy,
    IncrementalState,
    IncrementalStateItem,
    IncrementalStateJSONEncoder,
    parse_incremental_state,
)


class TestIncrementalState(unittest.TestCase):
    def test_state_serialization_and_deserialization(self):
        expected = IncrementalState(
            items=[
                IncrementalStateItem(
                    source=Path("repo/foo.txt"),
                    destination_relative_to_bundle=Path("foo.txt"),
                    digest="foo_digest",
                    resolved_symlink=None,
                ),
                IncrementalStateItem(
                    source=Path("buck-out/bar.txt"),
                    destination_relative_to_bundle=Path("Resources/bar.txt"),
                    digest="bar_digest",
                    resolved_symlink=None,
                ),
            ],
            codesigned=True,
            codesign_configuration=None,
            codesigned_on_copy=[
                CodesignedOnCopy(
                    path=Path("Resources/bar.txt"),
                    entitlements_digest=None,
                    codesign_flags_override=None,
                    extra_codesign_paths=None,
                ),
                CodesignedOnCopy(
                    path=Path("Resources/baz.txt"),
                    entitlements_digest="abc",
                    codesign_flags_override=None,
                    extra_codesign_paths=None,
                ),
                CodesignedOnCopy(
                    path=Path("Resources/qux.txt"),
                    entitlements_digest=None,
                    codesign_flags_override=["--deep", "--force"],
                    extra_codesign_paths=None,
                ),
                CodesignedOnCopy(
                    path=Path("Resources/Parent.framework"),
                    entitlements_digest=None,
                    codesign_flags_override=None,
                    extra_codesign_paths=["Frameworks/Base.framework"],
                ),
            ],
            codesign_identity="Johnny Appleseed",
            codesign_arguments=[
                "--force",
            ],
            swift_stdlib_paths=[Path("Frameworks/libswiftCore.dylib")],
            versioned_if_macos=False,
        )
        json_result = json.dumps(expected, cls=IncrementalStateJSONEncoder)
        result = parse_incremental_state(io.StringIO(json_result))
        self.assertEqual(
            result,
            expected,
        )

    def _resource_content(self, filename):
        resources = importlib.resources.files(__package__) / "test_resources"
        return resources / filename

    def test_valid_state_is_parsed_successfully(self):
        path = self._resource_content("valid_incremental_state.json")
        with path.open() as file_content:
            result = parse_incremental_state(file_content)
        expected = IncrementalState(
            items=[
                IncrementalStateItem(
                    source=Path("repo/foo.txt"),
                    destination_relative_to_bundle=Path("foo.txt"),
                    digest="foo_digest",
                    resolved_symlink=None,
                ),
                IncrementalStateItem(
                    source=Path("buck-out/bar.txt"),
                    destination_relative_to_bundle=Path("Resources/bar.txt"),
                    digest="bar_digest",
                    resolved_symlink=None,
                ),
                IncrementalStateItem(
                    source=Path("buck-out/bar"),
                    destination_relative_to_bundle=Path("Resources/bar"),
                    digest=None,
                    resolved_symlink=Path("bar.txt"),
                ),
            ],
            codesigned=True,
            codesign_configuration=None,
            codesigned_on_copy=[
                CodesignedOnCopy(
                    path=Path("Resources/bar.txt"),
                    entitlements_digest=None,
                    codesign_flags_override=None,
                    extra_codesign_paths=None,
                ),
                CodesignedOnCopy(
                    path=Path("Resources/baz.txt"),
                    entitlements_digest="abc",
                    codesign_flags_override=None,
                    extra_codesign_paths=None,
                ),
                CodesignedOnCopy(
                    path=Path("Resources/qux.txt"),
                    entitlements_digest=None,
                    codesign_flags_override=["--deep", "--force"],
                    extra_codesign_paths=None,
                ),
                CodesignedOnCopy(
                    path=Path("Resources/Parent.framework"),
                    entitlements_digest=None,
                    codesign_flags_override=None,
                    extra_codesign_paths=["Frameworks/Base.framework"],
                ),
            ],
            codesign_identity="Johny Appleseed",
            codesign_arguments=[
                "--force",
                "--deep",
            ],
            swift_stdlib_paths=[Path("Frameworks/libswiftCore.dylib")],
            versioned_if_macos=True,
        )
        self.assertEqual(
            result,
            expected,
        )

    def test_error_when_invalid_metadata(self):
        path = self._resource_content("the.broken_json")
        with self.assertRaises(json.JSONDecodeError):
            with path.open() as file_content:
                _ = parse_incremental_state(file_content)

    def test_user_friendly_error_when_metadata_with_newer_version(self):
        path = self._resource_content("newer_version_incremental_state.json")
        with self.assertRaises(Exception) as context:
            with path.open() as file_content:
                _ = parse_incremental_state(file_content)
            self.assertEqual(
                context.exception,
                RuntimeError("Expected incremental state version to be `2` got `3`."),
            )
