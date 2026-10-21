# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

from __future__ import annotations

import contextlib
import io
import os
import stat
import sys
import tempfile
import unittest
from pathlib import Path
from unittest import mock

from package_tree import flat_package, main, merge, strip_types


def _write(path: Path, text: str = "") -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text(text)


def _files(root: Path) -> list[str]:
    return sorted(
        p.relative_to(root).as_posix() for p in root.rglob("*") if p.is_file()
    )


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

    def test_cli_reports_missing_package_json(self) -> None:
        _write(self.root / "emit" / "index.js")
        argv = ["package_tree.py", "flat-package", "--emit", str(self.root / "emit")]
        argv += ["--package-json", str(self.root / "missing.json")]
        stderr = io.StringIO()
        with (
            mock.patch.object(sys, "argv", argv + ["--out", str(self.root / "out")]),
            contextlib.redirect_stderr(stderr),
        ):
            self.assertEqual(main(), 1)
        self.assertRegex(stderr.getvalue(), r"^flat-package: .*missing\.json'\n$")


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

    @unittest.skipIf(sys.platform == "win32", "creating symlinks needs privileges")
    def test_symlink_cycle_copied_as_targets_fails(self) -> None:
        _write(self.deps / "pkg" / "index.js")
        os.symlink("..", self.deps / "pkg" / "loop", target_is_directory=True)
        self.prebuilt.mkdir()
        with self.assertRaisesRegex(ValueError, "symlink cycle: .*loop"):
            merge(self.deps, self.prebuilt, self.out, keep_symlinks=False)


class TestStripTypes(unittest.TestCase):
    def setUp(self) -> None:
        tmp = tempfile.TemporaryDirectory()
        self.addCleanup(tmp.cleanup)
        self.root = Path(tmp.name)
        self.src = self.root / "src"
        self.out = self.root / "out"

    def test_declarations_and_their_maps_removed(self) -> None:
        for name in (
            "index.js",
            "index.js.map",
            "index.d.ts",
            "index.d.ts.map",
            "esm.mjs",
            "esm.mjs.map",
            "esm.d.mts",
            "esm.d.mts.map",
            "cjs.cjs",
            "cjs.cjs.map",
            "cjs.d.cts",
            "cjs.d.cts.map",
            "main.ts",
        ):
            _write(self.src / "lib" / name, name)
        _write(self.src / "types" / "index.d.ts")
        _write(self.src / "package.json", "{}")
        strip_types(self.src, self.out)
        self.assertEqual(
            _files(self.out),
            [
                "lib/cjs.cjs",
                "lib/cjs.cjs.map",
                "lib/esm.mjs",
                "lib/esm.mjs.map",
                "lib/index.js",
                "lib/index.js.map",
                "lib/main.ts",
                "package.json",
            ],
        )
        self.assertEqual(
            (self.out / "lib" / "index.js.map").read_text(), "index.js.map"
        )
        self.assertEqual(os.listdir(self.out / "types"), [])

    @unittest.skipIf(sys.platform == "win32", "Windows has no executable bit")
    def test_executable_bit_kept(self) -> None:
        _write(self.src / "bin" / "cli.js", "#!/usr/bin/env node")
        os.chmod(self.src / "bin" / "cli.js", 0o755)
        strip_types(self.src, self.out)
        mode = stat.S_IMODE(os.stat(self.out / "bin" / "cli.js").st_mode)
        self.assertEqual(mode & 0o111, 0o111)

    @unittest.skipIf(sys.platform == "win32", "creating symlinks needs privileges")
    def test_symlinks_copied_as_targets(self) -> None:
        _write(self.root / "store" / "pkg" / "index.js", "pkg")
        _write(self.root / "store" / "pkg" / "index.d.ts")
        _write(self.root / "store" / "util.js", "util")
        _write(self.root / "store" / "util.d.ts")
        self.src.mkdir()
        os.symlink("../store/pkg", self.src / "pkg", target_is_directory=True)
        os.symlink("../store/util.js", self.src / "util.js")
        os.symlink("../store/util.d.ts", self.src / "util.d.ts")
        strip_types(self.src, self.out)
        self.assertEqual(_files(self.out), ["pkg/index.js", "util.js"])
        for path in (
            self.out / "pkg",
            self.out / "pkg" / "index.js",
            self.out / "util.js",
        ):
            self.assertFalse(path.is_symlink())
        self.assertEqual((self.out / "pkg" / "index.js").read_text(), "pkg")
        self.assertEqual((self.out / "util.js").read_text(), "util")

    @unittest.skipIf(sys.platform == "win32", "creating symlinks needs privileges")
    def test_symlink_to_declaration_under_other_name_removed(self) -> None:
        _write(self.root / "store" / "index.d.ts")
        _write(self.root / "store" / "index.d.ts.map")
        self.src.mkdir()
        os.symlink("../store/index.d.ts", self.src / "index.js")
        os.symlink("../store/index.d.ts.map", self.src / "index.js.map")
        strip_types(self.src, self.out)
        self.assertEqual(_files(self.out), [])

    @unittest.skipIf(sys.platform == "win32", "creating symlinks needs privileges")
    def test_directory_linked_twice_copied_twice(self) -> None:
        _write(self.root / "store" / "index.js")
        self.src.mkdir()
        os.symlink("../store", self.src / "a", target_is_directory=True)
        os.symlink("../store", self.src / "b", target_is_directory=True)
        strip_types(self.src, self.out)
        self.assertEqual(_files(self.out), ["a/index.js", "b/index.js"])

    @unittest.skipIf(sys.platform == "win32", "creating symlinks needs privileges")
    def test_symlink_cycle_fails(self) -> None:
        for target in (".", ".."):
            with self.subTest(target=target):
                tmp = tempfile.TemporaryDirectory()
                self.addCleanup(tmp.cleanup)
                root = Path(tmp.name)
                _write(root / "src" / "pkg" / "index.js")
                os.symlink(
                    target, root / "src" / "pkg" / "loop", target_is_directory=True
                )
                with self.assertRaisesRegex(ValueError, "symlink cycle: .*loop"):
                    strip_types(root / "src", root / "out")

    @unittest.skipIf(sys.platform == "win32", "creating symlinks needs privileges")
    def test_dangling_symlink_fails(self) -> None:
        self.src.mkdir()
        os.symlink("missing.js", self.src / "index.js")
        with self.assertRaises(FileNotFoundError):
            strip_types(self.src, self.out)

    @unittest.skipIf(sys.platform == "win32", "creating symlinks needs privileges")
    def test_cli_reports_dangling_symlink(self) -> None:
        self.src.mkdir()
        os.symlink("missing.js", self.src / "index.js")
        argv = ["package_tree.py", "strip-types", "--src", str(self.src)]
        stderr = io.StringIO()
        with (
            mock.patch.object(sys, "argv", argv + ["--out", str(self.out)]),
            contextlib.redirect_stderr(stderr),
        ):
            self.assertEqual(main(), 1)
        self.assertRegex(stderr.getvalue(), r"^strip-types: .*index\.js'\n$")
