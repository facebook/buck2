#!/usr/bin/env fbpython
# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

import io
import json
import pathlib
import tempfile
import unittest
import zipfile

from gen_class_to_source_map import generate_class_to_source_map


class GenClassToSourceMapTest(unittest.TestCase):
    def _generate(
        self,
        directory: pathlib.Path,
        *,
        jar_classes: list[str],
        sources: list[pathlib.Path],
        include_prefixes: tuple[str, ...] = (),
    ) -> list[dict[str, str]]:
        jar_path = directory / "library.jar"
        with zipfile.ZipFile(jar_path, "w") as jar:
            for class_name in jar_classes:
                jar.writestr(class_name.replace(".", "/") + ".class", b"")

        output = io.StringIO()
        generate_class_to_source_map(
            str(jar_path),
            [str(source) for source in sources],
            output,
            include_classes_prefixes=include_prefixes,
        )
        return json.loads(output.getvalue())["classes"]

    def test_maps_source_by_legacy_package_path(self) -> None:
        with tempfile.TemporaryDirectory() as temp_dir:
            directory = pathlib.Path(temp_dir)
            source = directory / "src" / "com" / "example" / "Legacy.java"
            classes = self._generate(
                directory,
                jar_classes=["com.example.Legacy"],
                sources=[source],
            )

        self.assertEqual(
            classes,
            [{"className": "com.example.Legacy", "srcPath": str(source)}],
        )

    def test_normalizes_kotlin_file_facade(self) -> None:
        with tempfile.TemporaryDirectory() as temp_dir:
            directory = pathlib.Path(temp_dir)
            source = directory / "com" / "example" / "Utilities.kt"
            classes = self._generate(
                directory,
                jar_classes=["com.example.UtilitiesKt"],
                sources=[source],
            )

        self.assertEqual(
            classes,
            [{"className": "com.example.Utilities", "srcPath": str(source)}],
        )

    def test_preserves_real_class_with_kt_suffix(self) -> None:
        with tempfile.TemporaryDirectory() as temp_dir:
            directory = pathlib.Path(temp_dir)
            source = directory / "com" / "example" / "UtilitiesKt.kt"
            classes = self._generate(
                directory,
                jar_classes=["com.example.UtilitiesKt"],
                sources=[source],
            )

        self.assertEqual(
            classes,
            [{"className": "com.example.UtilitiesKt", "srcPath": str(source)}],
        )

    def test_preserves_sourceless_generated_classes(self) -> None:
        with tempfile.TemporaryDirectory() as temp_dir:
            directory = pathlib.Path(temp_dir)
            classes = self._generate(
                directory,
                jar_classes=[
                    "androidx.databinding.WidgetBinding",
                    "com.example.Unknown",
                ],
                sources=[],
                include_prefixes=("androidx.databinding",),
            )

        self.assertEqual(classes, [{"className": "androidx.databinding.WidgetBinding"}])
