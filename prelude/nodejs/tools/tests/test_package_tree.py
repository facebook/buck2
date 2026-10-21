# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

from __future__ import annotations

import contextlib
import errno
import io
import os
import stat
import sys
import tempfile
import unittest
from pathlib import Path
from unittest import mock

from package_tree import assemble_package, main, merge, strip_types


def _write(path: Path, text: str = "") -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text(text)


def _files(root: Path) -> list[str]:
    return sorted(
        p.relative_to(root).as_posix() for p in root.rglob("*") if p.is_file()
    )


class TestAssemblePackage(unittest.TestCase):
    def setUp(self) -> None:
        tmp = tempfile.TemporaryDirectory()
        self.addCleanup(tmp.cleanup)
        self.root = Path(tmp.name)
        self.emit = self.root / "emit"
        self.out = self.root / "out"
        _write(self.emit / "cmd" / "render.js", "js")
        _write(self.emit / "cmd" / "render.d.ts", "dts")
        _write(self.root / "package.json", "supplied")

    def _assemble(
        self, package_subdir: str, *entries: tuple[str, str], package_json: bool = False
    ) -> list[str]:
        return assemble_package(
            self.emit,
            package_subdir,
            [(path, self.root / src, f"entry '{src}'") for path, src in entries],
            self.root / "package.json" if package_json else None,
            self.out,
        )

    def assertErrorsMention(
        self, errors: list[str], *expected: tuple[str, ...]
    ) -> None:
        self.assertEqual(len(errors), len(expected), errors)
        for error, parts in zip(errors, expected):
            for part in parts:
                self.assertIn(part, error)

    def test_emit_under_package_subdir_beside_package_json(self) -> None:
        self.assertEqual(self._assemble("lib/js", package_json=True), [])
        self.assertEqual(
            _files(self.out),
            ["lib/js/cmd/render.d.ts", "lib/js/cmd/render.js", "package.json"],
        )
        self.assertEqual((self.out / "lib/js/cmd/render.js").read_text(), "js")
        self.assertEqual((self.out / "package.json").read_text(), "supplied")

    def test_emit_at_root(self) -> None:
        self.assertEqual(self._assemble(""), [])
        self.assertEqual(_files(self.out), ["cmd/render.d.ts", "cmd/render.js"])

    def test_package_json_replaces_emitted_manifest(self) -> None:
        _write(self.emit / "package.json", "emitted")
        os.chmod(self.emit / "package.json", 0o444)
        self.assertEqual(self._assemble("", package_json=True), [])
        self.assertEqual((self.out / "package.json").read_text(), "supplied")

    def test_package_json_on_emitted_directory_collides(self) -> None:
        _write(self.emit / "package.json" / "index.js", "js")
        self.assertErrorsMention(
            self._assemble("", package_json=True),
            ("`package_json`", "collides", "directory 'package.json'"),
        )
        self.assertEqual((self.out / "package.json" / "index.js").read_text(), "js")

    def test_package_json_that_cannot_be_copied_is_reported(self) -> None:
        (self.root / "package.json").unlink()
        self.assertErrorsMention(
            self._assemble("", package_json=True),
            ("`package_json`", "'package.json'", os.strerror(errno.ENOENT)),
        )
        self.assertEqual(_files(self.out), ["cmd/render.d.ts", "cmd/render.js"])

    def test_entries_merge_into_emitted_directories(self) -> None:
        _write(self.root / "data.json", "data")
        _write(self.root / "templates" / "a.txt", "a")
        _write(self.root / "templates" / "nested" / "b.txt", "b")
        _write(self.root / "assets" / "render.css", "css")
        self.assertEqual(
            self._assemble(
                "dist",
                ("dist/cmd/data.json", "data.json"),
                ("dist/templates", "templates"),
                ("dist/cmd", "assets"),
            ),
            [],
        )
        self.assertEqual(
            _files(self.out),
            [
                "dist/cmd/data.json",
                "dist/cmd/render.css",
                "dist/cmd/render.d.ts",
                "dist/cmd/render.js",
                "dist/templates/a.txt",
                "dist/templates/nested/b.txt",
            ],
        )
        self.assertEqual((self.out / "dist/templates/nested/b.txt").read_text(), "b")

    def test_collision_names_the_entry_and_the_emitted_path(self) -> None:
        _write(self.root / "file.txt")
        _write(self.root / "dir" / "render.js")
        _write(self.root / "dir" / "render.css", "css")
        for i, (path, src, message) in enumerate(
            [
                ("dist/cmd/render.js", "file.txt", "file 'dist/cmd/render.js'"),
                ("dist/cmd", "file.txt", "directory 'dist/cmd'"),
                (
                    "dist/cmd/render.d.ts/x.txt",
                    "file.txt",
                    "file 'dist/cmd/render.d.ts'",
                ),
                ("dist/cmd/render.js", "dir", "file 'dist/cmd/render.js'"),
                ("dist/cmd", "dir", "file 'dist/cmd/render.js'"),
            ]
        ):
            with self.subTest(path=path, src=src):
                self.out = self.root / f"out{i}"
                self.assertErrorsMention(
                    self._assemble("dist", (path, src)),
                    (f"entry '{src}'", "collides", message),
                )
                self.assertEqual((self.out / "dist/cmd/render.js").read_text(), "js")
        self.assertEqual((self.root / "out4/dist/cmd/render.css").read_text(), "css")

    def test_every_collision_reported(self) -> None:
        _write(self.root / "a.txt")
        _write(self.root / "dir" / "render.js")
        _write(self.root / "dir" / "render.d.ts")
        _write(self.root / "dir" / "extra.txt")
        self.assertErrorsMention(
            self._assemble(
                "",
                ("cmd/render.d.ts", "a.txt"),
                ("cmd/render.js", "a.txt"),
                ("ok.txt", "a.txt"),
            ),
            ("entry 'a.txt'", "collides", "file 'cmd/render.d.ts'"),
            ("entry 'a.txt'", "collides", "file 'cmd/render.js'"),
        )
        self.assertTrue((self.out / "ok.txt").exists())
        self.out = self.root / "out2"
        self.assertErrorsMention(
            self._assemble("", ("cmd", "dir")),
            ("entry 'dir'", "collides", "file 'cmd/render.d.ts'"),
            ("entry 'dir'", "collides", "file 'cmd/render.js'"),
        )
        self.assertTrue((self.out / "cmd" / "extra.txt").exists())

    @unittest.skipIf(sys.platform == "win32", "Windows has no executable bit")
    def test_executable_bit_kept(self) -> None:
        _write(self.root / "bin" / "run.sh", "#!/bin/sh")
        os.chmod(self.root / "bin" / "run.sh", 0o755)
        self.assertEqual(self._assemble("dist", ("bin", "bin")), [])
        mode = stat.S_IMODE(os.stat(self.out / "bin" / "run.sh").st_mode)
        self.assertEqual(mode & 0o111, 0o111)

    @unittest.skipIf(sys.platform == "win32", "creating symlinks needs privileges")
    def test_symlinks_copied_as_targets(self) -> None:
        _write(self.root / "store" / "a.txt", "a")
        _write(self.root / "res" / "b.txt", "b")
        os.symlink("../store", self.root / "res" / "linked", target_is_directory=True)
        os.symlink("b.txt", self.root / "res" / "c.txt")
        os.symlink("../store", self.emit / "linked", target_is_directory=True)
        os.symlink("render.js", self.emit / "cmd" / "alias.js")
        self.assertEqual(self._assemble("dist", ("dist/res", "res")), [])
        self.assertEqual(
            _files(self.out),
            [
                "dist/cmd/alias.js",
                "dist/cmd/render.d.ts",
                "dist/cmd/render.js",
                "dist/linked/a.txt",
                "dist/res/b.txt",
                "dist/res/c.txt",
                "dist/res/linked/a.txt",
            ],
        )
        for path in _files(self.out) + ["dist/linked", "dist/res/linked"]:
            self.assertFalse((self.out / path).is_symlink(), path)
        self.assertEqual((self.out / "dist/res/c.txt").read_text(), "b")
        self.assertEqual((self.out / "dist/cmd/alias.js").read_text(), "js")

    @unittest.skipIf(sys.platform == "win32", "creating symlinks needs privileges")
    def test_symlink_cycle_in_emit_fails(self) -> None:
        for target in (".", ".."):
            with self.subTest(target=target):
                self.out = self.root / f"out{len(target)}"
                loop = self.emit / "cmd" / "loop"
                loop.unlink(missing_ok=True)
                os.symlink(target, loop, target_is_directory=True)
                with self.assertRaisesRegex(ValueError, "symlink cycle: .*loop"):
                    self._assemble("dist")

    @unittest.skipIf(sys.platform == "win32", "creating symlinks needs privileges")
    def test_symlink_cycle_in_entry_reported_with_other_errors(self) -> None:
        _write(self.root / "a.txt")
        _write(self.root / "res" / "sub" / "b.txt", "b")
        for target in (".", ".."):
            with self.subTest(target=target):
                self.out = self.root / f"out{len(target)}"
                loop = self.root / "res" / "sub" / "loop"
                loop.unlink(missing_ok=True)
                os.symlink(target, loop, target_is_directory=True)
                errors = self._assemble("", ("cmd/render.js", "a.txt"), ("res", "res"))
                self.assertEqual(len(errors), 2, errors)
                self.assertEqual(
                    errors[0],
                    "entry 'a.txt' collides with the emitted file 'cmd/render.js'",
                )
                self.assertRegex(
                    errors[1],
                    r"^entry 'res': cannot copy 'res/sub/loop(/\w+)*': symlink cycle: .*loop",
                )
                self.assertEqual((self.out / "res/sub/b.txt").read_text(), "b")

    @unittest.skipIf(sys.platform == "win32", "creating symlinks needs privileges")
    def test_file_that_cannot_be_copied_names_the_entry(self) -> None:
        _write(self.root / "res" / "a.txt", "a")
        os.symlink("missing.txt", self.root / "res" / "dangling.txt")
        self.assertErrorsMention(
            self._assemble("dist", ("dist/res", "res")),
            ("entry 'res'", "'dist/res/dangling.txt'", os.strerror(errno.ENOENT)),
        )
        self.assertEqual((self.out / "dist/res/a.txt").read_text(), "a")

    @unittest.skipIf(
        sys.platform == "win32" or os.geteuid() == 0,
        "needs POSIX permissions that apply to the current user",
    )
    def test_directory_that_cannot_be_read_names_the_entry(self) -> None:
        _write(self.root / "res" / "a.txt", "a")
        _write(self.root / "res" / "locked" / "b.txt", "b")
        os.chmod(self.root / "res" / "locked", 0)
        self.addCleanup(os.chmod, self.root / "res" / "locked", 0o755)
        self.assertErrorsMention(
            self._assemble("dist", ("dist/res", "res")),
            ("entry 'res'", "'dist/res/locked'", os.strerror(errno.EACCES)),
        )
        self.assertEqual((self.out / "dist/res/a.txt").read_text(), "a")

    def test_parent_directory_that_cannot_be_created_names_the_entry(self) -> None:
        _write(self.root / "a.txt", "a")
        _write(self.root / "b.txt", "b")
        mkdir = Path.mkdir

        def failing_mkdir(path: Path, *args: object, **kwargs: object) -> None:
            if path == self.out / "locked":
                raise PermissionError(errno.EACCES, os.strerror(errno.EACCES))
            mkdir(path, *args, **kwargs)

        with mock.patch.object(Path, "mkdir", autospec=True, side_effect=failing_mkdir):
            errors = self._assemble(
                "dist", ("locked/a.txt", "a.txt"), ("b.txt", "b.txt")
            )
        self.assertErrorsMention(
            errors, ("entry 'a.txt'", "'locked/a.txt'", os.strerror(errno.EACCES))
        )
        self.assertEqual((self.out / "b.txt").read_text(), "b")

    def test_cli_reports_missing_package_json(self) -> None:
        _write(self.root / "entries.json", "[]")
        argv = ["package_tree.py", "assemble-package", "--emit", str(self.emit)]
        argv += ["--entries", str(self.root / "entries.json")]
        argv += ["--package-json", str(self.root / "missing.json")]
        stderr = io.StringIO()
        with (
            mock.patch.object(sys, "argv", argv + ["--out", str(self.root / "out")]),
            contextlib.redirect_stderr(stderr),
        ):
            self.assertEqual(main(), 1)
        self.assertEqual(
            stderr.getvalue(),
            "`package_json`: cannot copy 'package.json': "
            f"{os.strerror(errno.ENOENT)}\n",
        )


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
