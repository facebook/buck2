# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

import io
import plistlib
import unittest

from .process import _apply_insert, _apply_merge, _apply_set, process


class TestProcess(unittest.TestCase):
    def test_canary(self):
        input_file = io.BytesIO(
            b"""<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN" "http://www.apple.com/DTDs/PropertyList-1.0.dtd">
<plist version="1.0">
<dict>
    <key>foo</key>
    <string>bar</string>
</dict>
</plist>
"""
        )
        output_file = io.BytesIO()
        process(input_file, output_file)
        self.assertTrue(len(output_file.getvalue()) > 0)

    def test_additional_input_given_no_keys_conflict(self):
        input_file = io.BytesIO(
            b"""<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN" "http://www.apple.com/DTDs/PropertyList-1.0.dtd">
<plist version="1.0">
<dict>
    <key>foo</key>
    <string>bar</string>
</dict>
</plist>
"""
        )
        override_input_file = io.BytesIO(
            b"""<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN" "http://www.apple.com/DTDs/PropertyList-1.0.dtd">
<plist version="1.0">
<dict>
    <key>baz</key>
    <string>qux</string>
</dict>
</plist>
"""
        )
        output_file = io.BytesIO()
        process(input_file, output_file, override_input_file)
        output_file.seek(0)
        root = plistlib.load(output_file)
        self.assertEqual(root, {"foo": "bar", "baz": "qux"})

    def test_additional_input_given_keys_conflict(self):
        input_file = io.BytesIO(
            b"""<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN" "http://www.apple.com/DTDs/PropertyList-1.0.dtd">
<plist version="1.0">
<dict>
    <key>foo</key>
    <string>bar</string>
    <key>qux</key>
    <dict>
        <key>a</key>
        <string>b</string>
        <key>b</key>
        <string>c</string>
    </dict>
    <key>foobar</key>
    <dict>
        <key>hello</key>
        <string>world</string>
    </dict>
</dict>
</plist>
"""
        )
        override_input_file = io.BytesIO(
            b"""<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN" "http://www.apple.com/DTDs/PropertyList-1.0.dtd">
<plist version="1.0">
<dict>
    <key>foo</key>
    <string>baz</string>
    <key>qux</key>
    <dict>
        <key>a</key>
        <string>z</string>
        <key>c</key>
        <string>x</string>
    </dict>
    <key>foobar</key>
    <string>zanzibar</string>
</dict>
</plist>
"""
        )
        output_file = io.BytesIO()
        process(input_file, output_file, override_input_file)
        output_file.seek(0)
        root = plistlib.load(output_file)
        self.assertEqual(
            root,
            {"foo": "baz", "qux": {"a": "z", "b": "c", "c": "x"}, "foobar": "zanzibar"},
        )

    def test_additional_keys(self):
        input_file = io.BytesIO(
            b"""<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN" "http://www.apple.com/DTDs/PropertyList-1.0.dtd">
<plist version="1.0">
<dict>
    <key>foo</key>
    <string>bar</string>
</dict>
</plist>
"""
        )
        additional_keys = {"baz": "qux"}
        output_file = io.BytesIO()
        process(input_file, output_file, additional_keys=additional_keys)
        output_file.seek(0)
        root = plistlib.load(output_file)
        self.assertEqual(root, {"foo": "bar", "baz": "qux"})

    def test_additional_keys_do_not_override(self):
        input_file = io.BytesIO(
            b"""<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN" "http://www.apple.com/DTDs/PropertyList-1.0.dtd">
<plist version="1.0">
<dict>
    <key>foo</key>
    <string>bar</string>
</dict>
</plist>
"""
        )
        additional_keys = {"foo": "baz"}
        output_file = io.BytesIO()
        process(input_file, output_file, additional_keys=additional_keys)
        output_file.seek(0)
        root = plistlib.load(output_file)
        self.assertEqual(root, {"foo": "bar"})

    def test_additional_keys_from_file(self):
        input_file = io.BytesIO(
            b"""<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN" "http://www.apple.com/DTDs/PropertyList-1.0.dtd">
<plist version="1.0">
<dict>
    <key>foo</key>
    <string>bar</string>
</dict>
</plist>
"""
        )
        additional_keys_file = io.BytesIO(b"""{"baz": "qux"}""")
        output_file = io.BytesIO()
        process(input_file, output_file, additional_keys_file=additional_keys_file)
        output_file.seek(0)
        root = plistlib.load(output_file)
        self.assertEqual(root, {"foo": "bar", "baz": "qux"})

    def test_override_keys_from_file(self):
        input_file = io.BytesIO(
            b"""<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN" "http://www.apple.com/DTDs/PropertyList-1.0.dtd">
<plist version="1.0">
<dict>
    <key>foo</key>
    <string>bar</string>
</dict>
</plist>
"""
        )
        override_keys_file = io.BytesIO(b"""{"foo": "baz"}""")
        output_file = io.BytesIO()
        process(input_file, output_file, override_keys_file=override_keys_file)
        output_file.seek(0)
        root = plistlib.load(output_file)
        self.assertEqual(root, {"foo": "baz"})

    def test_apply_set_string(self):
        """Test _apply_set with a string value."""
        plist_data = {"ExistingKey": "ExistingValue"}
        set_params = {"keypath": "TestKey", "value": "TestValue"}
        _apply_set(plist_data, set_params)
        self.assertEqual(plist_data["TestKey"], "TestValue")
        self.assertEqual(plist_data["ExistingKey"], "ExistingValue")

    def test_apply_set_integer(self):
        """Test _apply_set with an integer value."""
        plist_data = {"ExistingKey": "ExistingValue"}
        set_params = {"keypath": "TestKey", "value": 42}
        _apply_set(plist_data, set_params)
        self.assertEqual(plist_data["TestKey"], 42)
        self.assertIsInstance(plist_data["TestKey"], int)

    def test_apply_set_boolean(self):
        """Test _apply_set with a boolean value."""
        plist_data = {"ExistingKey": "ExistingValue"}
        set_params = {"keypath": "TestKey", "value": True}
        _apply_set(plist_data, set_params)
        self.assertEqual(plist_data["TestKey"], True)
        self.assertIsInstance(plist_data["TestKey"], bool)

    def test_apply_set_override(self):
        """Test _apply_set overriding an existing key."""
        plist_data = {"TestKey": "OldValue"}
        set_params = {"keypath": "TestKey", "value": "NewValue"}
        _apply_set(plist_data, set_params)
        self.assertEqual(plist_data["TestKey"], "NewValue")

    def test_apply_insert_simple(self):
        """Test _apply_insert with a simple list."""
        plist_data = {"TestList": ["existing_value"]}
        insert_params = {"keypath": "TestList", "value": "new_value"}
        _apply_insert(plist_data, insert_params)
        self.assertEqual(plist_data["TestList"], ["new_value", "existing_value"])

    def test_apply_insert_create_list(self):
        """Test _apply_insert creating a new list."""
        plist_data = {}
        insert_params = {"keypath": "NewList", "value": "new_value"}
        _apply_insert(plist_data, insert_params)
        self.assertEqual(plist_data["NewList"], ["new_value"])

    def test_apply_insert_nested(self):
        """Test _apply_insert with a nested list structure."""
        plist_data = {"CFBundleURLTypes": [{"CFBundleURLSchemes": ["existing_scheme"]}]}
        insert_params = {
            "keypath": "CFBundleURLTypes.CFBundleURLSchemes",
            "value": "new_scheme",
        }
        _apply_insert(plist_data, insert_params)
        expected = {
            "CFBundleURLTypes": [
                {"CFBundleURLSchemes": ["new_scheme", "existing_scheme"]}
            ]
        }
        self.assertEqual(plist_data, expected)

    def test_apply_insert_nested_create(self):
        """Test _apply_insert creating a nested structure."""
        plist_data = {"CFBundleURLTypes": []}
        insert_params = {
            "keypath": "CFBundleURLTypes.CFBundleURLSchemes",
            "value": "new_scheme",
        }
        _apply_insert(plist_data, insert_params)
        expected = {"CFBundleURLTypes": [{"CFBundleURLSchemes": ["new_scheme"]}]}
        self.assertEqual(plist_data, expected)

    def test_apply_insert_with_type(self):
        """Test _apply_insert with type conversion."""
        plist_data = {"NumberList": []}
        insert_params = {"keypath": "NumberList", "value": 42}
        _apply_insert(plist_data, insert_params)
        self.assertEqual(plist_data["NumberList"], [42])
        self.assertIsInstance(plist_data["NumberList"][0], int)

    def test_apply_merge(self):
        """Test _apply_merge with a temporary file."""
        import tempfile

        # Create a temporary plist file to merge
        with tempfile.NamedTemporaryFile(mode="wb", delete=False) as temp_file:
            merge_data = {
                "MergedKey": "MergedValue",
                "NestedDict": {"SubKey2": "SubValue2"},
                "MergedList": ["item1", "item2"],
            }
            plistlib.dump(merge_data, temp_file)
            temp_path = temp_file.name

        try:
            # Test merging the file
            plist_data = {
                "ExistingKey": "ExistingValue",
                "NestedDict": {
                    "SubKey1": "OriginalValue",
                    "ExistingSubKey": "KeepThis",
                },
                "MergedList": ["existing_item"],
            }

            _apply_merge(plist_data, temp_path)

            # Verify the results
            self.assertEqual(plist_data["MergedKey"], "MergedValue")
            self.assertEqual(plist_data["ExistingKey"], "ExistingValue")

            # Check deep merge of nested dictionary
            self.assertEqual(plist_data["NestedDict"]["SubKey1"], "OriginalValue")
            self.assertEqual(plist_data["NestedDict"]["SubKey2"], "SubValue2")
            self.assertEqual(plist_data["NestedDict"]["ExistingSubKey"], "KeepThis")

            # Check list merging
            self.assertEqual(len(plist_data["MergedList"]), 3)
            self.assertIn("item1", plist_data["MergedList"])
            self.assertIn("item2", plist_data["MergedList"])
            self.assertIn("existing_item", plist_data["MergedList"])
        finally:
            # Clean up the temporary file
            import os

            os.unlink(temp_path)

    def test_apply_merge_nonexistent_file(self):
        """Test _apply_merge with a nonexistent file (should not raise an error)."""
        plist_data = {"ExistingKey": "ExistingValue"}
        # This should not raise an exception
        _apply_merge(plist_data, "/nonexistent/file/path.plist")
        # Data should remain unchanged
        self.assertEqual(plist_data, {"ExistingKey": "ExistingValue"})

    def test_apply_merge_nested_list_of_dicts(self):
        """Test _apply_merge with a nested list of dictionaries."""
        import tempfile

        # Create a temporary plist file to merge with nested list of dicts
        with tempfile.NamedTemporaryFile(mode="wb", delete=False) as temp_file:
            merge_data = {
                "NestedListOfDicts": [
                    {"SubKey1": "SubValue1", "SubKey2": "SubValue2"},
                    {"SubKey3": "SubValue3"},
                ]
            }
            plistlib.dump(merge_data, temp_file)
            temp_path = temp_file.name

        try:
            # Test merging the file
            plist_data = {
                "NestedListOfDicts": [
                    {"SubKey1": "OriginalValue", "UniqueKey": "KeepThis"},
                    {"SubKey4": "SubValue4"},
                ]
            }

            _apply_merge(plist_data, temp_path)

            # Verify the results
            self.assertEqual(len(plist_data["NestedListOfDicts"]), 4)

            # Check that the original items are preserved
            self.assertIn(
                {"SubKey1": "OriginalValue", "UniqueKey": "KeepThis"},
                plist_data["NestedListOfDicts"],
            )
            self.assertIn({"SubKey4": "SubValue4"}, plist_data["NestedListOfDicts"])

            # Check that the new items are added
            self.assertIn(
                {"SubKey1": "SubValue1", "SubKey2": "SubValue2"},
                plist_data["NestedListOfDicts"],
            )
            self.assertIn({"SubKey3": "SubValue3"}, plist_data["NestedListOfDicts"])
        finally:
            # Clean up the temporary file
            import os

            os.unlink(temp_path)

    def test_apply_insert_complex_keypath(self):
        """Test _apply_insert with a complex keypath including array indexing."""
        # Create a non-trivial plist_data structure with nested dictionaries and arrays
        plist_data = {
            "Root": {
                "Level1": [
                    {"Name": "Item0", "Values": ["a", "b", "c"]},
                    {"Name": "Item1", "Values": ["d", "e", "f"]},
                    {"Name": "Item2", "Values": ["g", "h", "i"]},
                    {
                        "Name": "Item3",
                        "Values": ["j", "k", "l"],
                        "Nested": {
                            "Config": [
                                {"Type": "Type1", "Options": ["opt1", "opt2"]},
                                {"Type": "Type2", "Options": ["opt3", "opt4"]},
                            ]
                        },
                    },
                ]
            }
        }

        # Insert a new option at a complex keypath with array indexing
        insert_params = {
            "keypath": "Root.Level1[3].Nested.Config[1].Options",
            "value": "new_option",
        }
        _apply_insert(plist_data, insert_params)

        # Verify that the value was inserted at the correct location
        expected_options = ["new_option", "opt3", "opt4"]
        self.assertEqual(
            plist_data["Root"]["Level1"][3]["Nested"]["Config"][1]["Options"],
            expected_options,
        )

        # Verify that other parts of the structure remain unchanged
        self.assertEqual(plist_data["Root"]["Level1"][0]["Name"], "Item0")
        self.assertEqual(plist_data["Root"]["Level1"][3]["Values"], ["j", "k", "l"])
        self.assertEqual(
            plist_data["Root"]["Level1"][3]["Nested"]["Config"][0]["Options"],
            ["opt1", "opt2"],
        )

    def test_apply_set_complex_keypath(self):
        """Test _apply_set with a complex keypath including array indexing."""
        # Create a non-trivial plist_data structure with nested dictionaries and arrays
        plist_data = {
            "Root": {
                "Level1": [
                    {"Name": "Item0", "Values": ["a", "b", "c"]},
                    {"Name": "Item1", "Values": ["d", "e", "f"]},
                    {"Name": "Item2", "Values": ["g", "h", "i"]},
                    {
                        "Name": "Item3",
                        "Values": ["j", "k", "l"],
                        "Nested": {
                            "Config": [
                                {"Type": "Type1", "Settings": {"Enabled": False}},
                                {"Type": "Type2", "Settings": {"Enabled": False}},
                            ]
                        },
                    },
                ]
            }
        }

        # Set a value at a complex keypath with array indexing
        set_params = {
            "keypath": "Root.Level1[3].Nested.Config[0].Settings.Enabled",
            "value": True,
        }
        _apply_set(plist_data, set_params)

        # Verify that the value was set at the correct location
        self.assertEqual(
            plist_data["Root"]["Level1"][3]["Nested"]["Config"][0]["Settings"][
                "Enabled"
            ],
            True,
        )

        # Verify that other parts of the structure remain unchanged
        self.assertEqual(plist_data["Root"]["Level1"][0]["Name"], "Item0")
        self.assertEqual(plist_data["Root"]["Level1"][3]["Values"], ["j", "k", "l"])
        self.assertEqual(
            plist_data["Root"]["Level1"][3]["Nested"]["Config"][1]["Settings"][
                "Enabled"
            ],
            False,
        )
