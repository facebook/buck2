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
from contextlib import redirect_stdout

from gen_class_to_source_map import generate_class_to_source_map, main


class GenClassToSourceMapTest(unittest.TestCase):
    def _generate(
        self,
        directory: pathlib.Path,
        *,
        jar_classes: list[str],
        sources: list[pathlib.Path],
        debuginfo: list[dict] | None,
        include_prefixes: tuple[str, ...] = (),
    ) -> list[dict[str, str]]:
        jar_path = directory / "library.jar"
        with zipfile.ZipFile(jar_path, "w") as jar:
            for class_name in jar_classes:
                jar.writestr(class_name.replace(".", "/") + ".class", b"")

        debuginfo_path = None
        if debuginfo is not None:
            debuginfo_file = directory / "debuginfo.json"
            debuginfo_file.write_text(json.dumps(debuginfo))
            debuginfo_path = str(debuginfo_file)

        output = io.StringIO()
        generate_class_to_source_map(
            str(jar_path),
            [str(source) for source in sources],
            output,
            debuginfo=debuginfo_path,
            include_classes_prefixes=include_prefixes,
        )
        return json.loads(output.getvalue())["classes"]

    def test_maps_case_mismatch_for_class_and_source_consumers(self) -> None:
        with tempfile.TemporaryDirectory() as temp_dir:
            directory = pathlib.Path(temp_dir)
            source = directory / "MWATestInitializer.kt"
            classes = self._generate(
                directory,
                jar_classes=["com.example.MwaTestInitializer"],
                sources=[source],
                debuginfo=[
                    {
                        "file_path": str(source),
                        "classes": [{"name": "com.example.MwaTestInitializer"}],
                    }
                ],
            )

        self.assertEqual(
            classes,
            [
                {
                    "className": "com.example.MwaTestInitializer",
                    "srcPath": str(source),
                },
                {
                    "className": "com.example.MWATestInitializer",
                    "srcPath": str(source),
                },
            ],
        )

    def test_maps_source_root_that_does_not_match_package(self) -> None:
        with tempfile.TemporaryDirectory() as temp_dir:
            directory = pathlib.Path(temp_dir)
            source = directory / "unrelated" / "Feature.java"
            classes = self._generate(
                directory,
                jar_classes=["com.example.Feature"],
                sources=[source],
                debuginfo=[
                    {
                        "file_path": str(source),
                        "classes": [{"name": "com.example.Feature"}],
                    }
                ],
            )

        self.assertEqual(
            classes,
            [{"className": "com.example.Feature", "srcPath": str(source)}],
        )

    def test_maps_multiple_top_level_kotlin_classes(self) -> None:
        with tempfile.TemporaryDirectory() as temp_dir:
            directory = pathlib.Path(temp_dir)
            source = directory / "Shared.kt"
            classes = self._generate(
                directory,
                jar_classes=["com.example.First", "com.example.Second"],
                sources=[source],
                debuginfo=[
                    {
                        "file_path": str(source),
                        "classes": [
                            {"name": "com.example.First"},
                            {"name": "com.example.Second"},
                        ],
                    }
                ],
            )

        self.assertEqual(
            classes,
            [
                {"className": "com.example.First", "srcPath": str(source)},
                {"className": "com.example.Second", "srcPath": str(source)},
                {"className": "com.example.Shared", "srcPath": str(source)},
            ],
        )

    def test_normalizes_kotlin_file_facade(self) -> None:
        with tempfile.TemporaryDirectory() as temp_dir:
            directory = pathlib.Path(temp_dir)
            source = directory / "Utilities.kt"
            classes = self._generate(
                directory,
                jar_classes=["com.example.UtilitiesKt"],
                sources=[source],
                debuginfo=[
                    {
                        "file_path": str(source),
                        "classes": [{"name": "com.example.UtilitiesKt"}],
                    }
                ],
            )

        self.assertEqual(
            classes,
            [{"className": "com.example.Utilities", "srcPath": str(source)}],
        )

    def test_preserves_real_class_with_kt_suffix(self) -> None:
        with tempfile.TemporaryDirectory() as temp_dir:
            directory = pathlib.Path(temp_dir)
            source = directory / "UtilitiesKt.kt"
            classes = self._generate(
                directory,
                jar_classes=["com.example.UtilitiesKt"],
                sources=[source],
                debuginfo=[
                    {
                        "file_path": str(source),
                        "classes": [{"name": "com.example.UtilitiesKt"}],
                    }
                ],
            )

        self.assertEqual(
            classes,
            [{"className": "com.example.UtilitiesKt", "srcPath": str(source)}],
        )

    def test_nested_and_synthetic_classes_map_to_outer_class(self) -> None:
        with tempfile.TemporaryDirectory() as temp_dir:
            directory = pathlib.Path(temp_dir)
            source = directory / "Outer.kt"
            classes = self._generate(
                directory,
                jar_classes=["com.example.Outer$Inner", "com.example.Outer$1"],
                sources=[source],
                debuginfo=[
                    {
                        "file_path": str(source),
                        "classes": [
                            {"name": "com.example.Outer"},
                            {"name": "com.example.Outer$Inner"},
                        ],
                    }
                ],
            )

        self.assertEqual(
            classes,
            [{"className": "com.example.Outer", "srcPath": str(source)}],
        )

    def test_sourceful_mapping_wins_regardless_of_jar_order(self) -> None:
        with tempfile.TemporaryDirectory() as temp_dir:
            directory = pathlib.Path(temp_dir)
            source = directory / "Outer.kt"
            classes = self._generate(
                directory,
                jar_classes=[
                    "androidx.databinding.Outer",
                    "androidx.databinding.Outer$Inner",
                ],
                sources=[source],
                debuginfo=[
                    {
                        "file_path": str(source),
                        "classes": [{"name": "androidx.databinding.Outer$Inner"}],
                    }
                ],
                include_prefixes=("androidx.databinding",),
            )

        self.assertEqual(
            classes,
            [
                {
                    "className": "androidx.databinding.Outer",
                    "srcPath": str(source),
                }
            ],
        )

    def test_preserves_sourceless_generated_classes_and_ignores_parser_only_classes(
        self,
    ) -> None:
        with tempfile.TemporaryDirectory() as temp_dir:
            directory = pathlib.Path(temp_dir)
            source = directory / "Phantom.java"
            classes = self._generate(
                directory,
                jar_classes=[
                    "androidx.databinding.WidgetBinding",
                    "com.example.Unknown",
                ],
                sources=[source],
                debuginfo=[
                    {
                        "file_path": str(source),
                        "classes": [{"name": "com.example.Phantom"}],
                    }
                ],
                include_prefixes=("androidx.databinding",),
            )

        self.assertEqual(classes, [{"className": "androidx.databinding.WidgetBinding"}])

    def test_ignores_ambiguous_debuginfo_and_falls_back_to_paths(self) -> None:
        with tempfile.TemporaryDirectory() as temp_dir:
            directory = pathlib.Path(temp_dir)
            source = directory / "com" / "example" / "Ambiguous.java"
            other = directory / "elsewhere" / "Ambiguous.java"
            classes = self._generate(
                directory,
                jar_classes=["com.example.Ambiguous"],
                sources=[source, other],
                debuginfo=[
                    {
                        "file_path": str(source),
                        "classes": [{"name": "com.example.Ambiguous"}],
                    },
                    {
                        "file_path": str(other),
                        "classes": [{"name": "com.example.Ambiguous"}],
                    },
                ],
            )

        # Two sources claim the same class, so the debug mapping is discarded and the
        # legacy package-path match (com/example/Ambiguous) is used instead.
        self.assertEqual(
            classes,
            [{"className": "com.example.Ambiguous", "srcPath": str(source)}],
        )

    def test_falls_back_to_legacy_path_matching_without_debuginfo(self) -> None:
        with tempfile.TemporaryDirectory() as temp_dir:
            directory = pathlib.Path(temp_dir)
            source = directory / "src" / "com" / "example" / "Legacy.java"
            classes = self._generate(
                directory,
                jar_classes=["com.example.Legacy"],
                sources=[source],
                debuginfo=None,
            )

        self.assertEqual(
            classes,
            [{"className": "com.example.Legacy", "srcPath": str(source)}],
        )

    def test_defaults_output_to_stdout(self) -> None:
        with tempfile.TemporaryDirectory() as temp_dir:
            jar_path = pathlib.Path(temp_dir) / "library.jar"
            with zipfile.ZipFile(jar_path, "w"):
                pass

            output = io.StringIO()
            with redirect_stdout(output):
                exit_code = main(["gen_class_to_source_map", str(jar_path)])

        self.assertEqual(exit_code, 0)
        self.assertEqual(
            json.loads(output.getvalue()),
            {"jarPath": str(jar_path), "classes": []},
        )

    def test_serializes_owner_target(self) -> None:
        with tempfile.TemporaryDirectory() as temp_dir:
            jar_path = pathlib.Path(temp_dir) / "library.jar"
            with zipfile.ZipFile(jar_path, "w"):
                pass

            output = io.StringIO()
            generate_class_to_source_map(
                str(jar_path),
                [],
                output,
                owner_target="cell//package:library",
            )

        self.assertEqual(
            json.loads(output.getvalue()),
            {
                "jarPath": str(jar_path),
                "classes": [],
                "ownerTarget": "cell//package:library",
            },
        )
