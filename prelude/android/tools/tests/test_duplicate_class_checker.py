# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

"""Tests for duplicate_class_checker.py."""

import json
import os
import tempfile
import unittest
import zipfile

from android.tools.duplicate_class_checker import (
    extract_class_names_from_jar,
    get_class_to_target_mapping_from_jars,
)


class ExtractClassNamesTest(unittest.TestCase):
    def _jar_with(self, tmp: str, name: str, *entries: str) -> str:
        path = os.path.join(tmp, name)
        with zipfile.ZipFile(path, "w") as jar:
            for entry in entries:
                jar.writestr(entry, b"")
        return path

    def test_nested_class_and_non_class_entries(self) -> None:
        with tempfile.TemporaryDirectory() as tmp:
            jar = self._jar_with(
                tmp,
                "a.jar",
                "com/example/Foo.class",
                "com/example/Foo$1.class",
                "META-INF/MANIFEST.MF",
            )
            self.assertEqual(
                extract_class_names_from_jar(jar),
                {"com.example.Foo", "com.example.Foo$1"},
            )

    def test_dot_class_inside_a_package_name_is_removed(self) -> None:
        # Every ".class" in the path is dropped, so a package called `classloader`
        # loses its tail and two unrelated classes end up with one name.
        with tempfile.TemporaryDirectory() as tmp:
            a = self._jar_with(tmp, "a.jar", "com/example/classloader/Foo.class")
            b = self._jar_with(tmp, "b.jar", "com/exampleloader/Foo.class")
            self.assertEqual(extract_class_names_from_jar(a), {"com.exampleloader.Foo"})
            mapping = os.path.join(tmp, "map.json")
            with open(mapping, "w") as f:
                json.dump({a: "//lib:a", b: "//lib:b"}, f)
            self.assertEqual(
                get_class_to_target_mapping_from_jars(mapping),
                {"com.exampleloader.Foo": ["//lib:a", "//lib:b"]},
            )
