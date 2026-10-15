# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

from __future__ import annotations

import os
import sys
import tempfile
import unittest
from pathlib import Path

from package_tree import flat_package, merge


def _write(path: Path, text: str = "") -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text(text)


class TestFlatPackage(unittest.TestCase):
    def setUp(self) -> None:
        tmp = tempfile.TemporaryDirectory()
        self.addCleanup(tmp.cleanup)
        self.root = Path(tmp.name)

    def test_emit_and_package_json_at_root(self) -> None:
        _write(self.root / "emit" / "index.js", "js")
        _write(self.root / "emit" / "lib" / "util.d.ts", "dts")
        _write(self.root / "package.json", "{}")
        flat_package(self.root / "emit", self.root / "package.json", self.root / "out")
        self.assertEqual((self.root / "out" / "index.js").read_text(), "js")
        self.assertEqual((self.root / "out" / "lib" / "util.d.ts").read_text(), "dts")
        self.assertEqual((self.root / "out" / "package.json").read_text(), "{}")

    def test_without_package_json(self) -> None:
        _write(self.root / "emit" / "index.js")
        flat_package(self.root / "emit", None, self.root / "out")
        self.assertEqual(sorted(os.listdir(self.root / "out")), ["index.js"])


class TestMerge(unittest.TestCase):
    def setUp(self) -> None:
        tmp = tempfile.TemporaryDirectory()
        self.addCleanup(tmp.cleanup)
        self.root = Path(tmp.name)
        self.deps = self.root / "deps"
        self.prebuilt = self.root / "prebuilt"
        self.out = self.root / "out"

    def test_disjoint_trees_merge(self) -> None:
        _write(self.deps / "a" / "index.js", "a")
        _write(self.deps / "@scope" / "x" / "index.js", "x")
        _write(self.prebuilt / "b" / "index.js", "b")
        _write(self.prebuilt / "@scope" / "y" / "index.js", "y")
        self.assertEqual(merge(self.deps, self.prebuilt, self.out), [])
        self.assertEqual(sorted(os.listdir(self.out)), ["@scope", "a", "b"])
        self.assertEqual(sorted(os.listdir(self.out / "@scope")), ["x", "y"])
        self.assertEqual((self.out / "@scope" / "y" / "index.js").read_text(), "y")

    def test_package_in_both_trees_collides(self) -> None:
        _write(self.deps / "a" / "index.js")
        _write(self.prebuilt / "a" / "index.js")
        _write(self.prebuilt / "b" / "index.js")
        self.assertEqual(merge(self.deps, self.prebuilt, self.out), ["a"])
        self.assertFalse(self.out.exists())

    def test_scoped_package_in_both_trees_collides(self) -> None:
        _write(self.deps / "@scope" / "x" / "index.js")
        _write(self.prebuilt / "@scope" / "x" / "index.js")
        _write(self.prebuilt / "@scope" / "y" / "index.js")
        self.assertEqual(merge(self.deps, self.prebuilt, self.out), ["@scope/x"])

    def test_scope_against_file_collides(self) -> None:
        _write(self.deps / "@scope" / "x" / "index.js")
        _write(self.prebuilt / "@scope")
        self.assertEqual(merge(self.deps, self.prebuilt, self.out), ["@scope"])

    @unittest.skipIf(sys.platform == "win32", "creating symlinks needs privileges")
    def test_symlinks_stay_symlinks(self) -> None:
        _write(self.prebuilt / "pkg" / "cli.js", "cli")
        (self.prebuilt / ".bin").mkdir()
        os.symlink("../pkg/cli.js", self.prebuilt / ".bin" / "cli")
        self.deps.mkdir()
        self.assertEqual(merge(self.deps, self.prebuilt, self.out), [])
        link = self.out / ".bin" / "cli"
        self.assertTrue(link.is_symlink())
        self.assertEqual(os.readlink(link), "../pkg/cli.js")
        self.assertEqual(link.read_text(), "cli")

    @unittest.skipIf(sys.platform == "win32", "creating symlinks needs privileges")
    def test_scope_symlink_against_scope_dir_merges(self) -> None:
        for linked in ("deps", "prebuilt"):
            with self.subTest(linked=linked):
                tmp = tempfile.TemporaryDirectory()
                self.addCleanup(tmp.cleanup)
                root = Path(tmp.name)
                deps, prebuilt, out = root / "deps", root / "prebuilt", root / "out"
                real, other = (prebuilt, deps) if linked == "deps" else (deps, prebuilt)
                _write(real / "@scope" / "x" / "index.js", "x")
                _write(root / "store" / "y" / "index.js", "y")
                other.mkdir()
                os.symlink(root / "store", other / "@scope", target_is_directory=True)
                self.assertEqual(merge(deps, prebuilt, out), [])
                self.assertFalse((out / "@scope").is_symlink())
                self.assertEqual(sorted(os.listdir(out / "@scope")), ["x", "y"])
                self.assertEqual(sorted(os.listdir(root / "store")), ["y"])

    @unittest.skipIf(sys.platform == "win32", "creating symlinks needs privileges")
    def test_linked_scope_with_linked_packages_merges(self) -> None:
        _write(self.root / "store" / "pkg" / "index.js", "pkg")
        _write(self.root / "store" / "scope" / "x" / "index.js", "x")
        os.symlink(
            "../../store/pkg",
            self.root / "store" / "scope" / "pkg",
            target_is_directory=True,
        )
        self.deps.mkdir()
        os.symlink("../store/scope", self.deps / "@scope", target_is_directory=True)
        _write(self.prebuilt / "@scope" / "y" / "index.js", "y")
        for keep_symlinks in (True, False):
            with self.subTest(keep_symlinks=keep_symlinks):
                out = self.root / f"out-{keep_symlinks}"
                self.assertEqual(
                    merge(self.deps, self.prebuilt, out, keep_symlinks=keep_symlinks),
                    [],
                )
                self.assertFalse((out / "@scope").is_symlink())
                self.assertEqual(sorted(os.listdir(out / "@scope")), ["pkg", "x", "y"])
                self.assertEqual((out / "@scope" / "pkg").is_symlink(), keep_symlinks)
                self.assertEqual(
                    (out / "@scope" / "pkg" / "index.js").read_text(), "pkg"
                )
        self.assertEqual(
            sorted(os.listdir(self.root / "store" / "scope")), ["pkg", "x"]
        )

    @unittest.skipIf(sys.platform == "win32", "creating symlinks needs privileges")
    def test_symlinks_copied_as_targets(self) -> None:
        _write(self.prebuilt / "pkg" / "cli.js", "cli")
        (self.prebuilt / ".bin").mkdir()
        os.symlink("../pkg/cli.js", self.prebuilt / ".bin" / "cli")
        self.deps.mkdir()
        os.symlink("../prebuilt/pkg", self.deps / "lib", target_is_directory=True)
        self.assertEqual(
            merge(self.deps, self.prebuilt, self.out, keep_symlinks=False), []
        )
        for path in (self.out / ".bin" / "cli", self.out / "lib" / "cli.js"):
            self.assertFalse(path.is_symlink())
            self.assertEqual(path.read_text(), "cli")
        self.assertFalse((self.out / "lib").is_symlink())
