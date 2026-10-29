# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

import os
import sys
import tempfile
import unittest
from pathlib import Path
from unittest import mock

import make_swift_interface

_BASE = [
    "-sdk",
    "/SDK",
    "-target",
    "arm64-apple-ios15.0",
    "-swift-version",
    "5",
    "-color-diagnostics",
    "-file-compilation-dir",
    ".",
    "-module-name",
    "Foo",
    "-Xfrontend",
    "-enable-cross-import-overlays",
]
_SEARCH = [
    "-F/repo/Vendor/Frameworks",
    "-Xcc",
    "-F/repo/Vendor/Frameworks",
    "-I/repo/hmap/foo.hmap",
    "-DDEBUG",
    "-D",
    "RELEASE",
]


class TestMakeSwiftInterface(unittest.TestCase):
    """Runs `main()` with the argsfile layout `swift_compilation.bzl` writes (the shared swiftc
    flags, no compiler path) against a swift-ide-test stand-in that echoes its arguments.
    """

    def setUp(self) -> None:
        self.dir = Path(tempfile.mkdtemp())
        self.tool = self.dir / "swift-ide-test"
        self.tool.write_text('#!/bin/sh\nfor a in "$@"; do printf "%s\\n" "$a"; done\n')
        os.chmod(self.tool, 0o755)

    def run_tool(self, flags: list[str]) -> list[str]:
        args = self.dir / "args"
        args.write_text("".join(flag + "\n" for flag in flags))
        out = self.dir / "out"
        argv = [
            "make_swift_interface",
            "--swift-ide-test-tool",
            str(self.tool),
            "--module",
            "Foo",
            "--out",
            str(out),
            "--",
            "@" + str(args),
        ]
        with mock.patch.object(sys, "argv", argv):
            make_swift_interface.main()
        return out.read_text().splitlines()

    def test_joined_search_paths_and_defines_are_dropped(self):
        forwarded = self.run_tool(_BASE + ["-resource-dir", "/res"] + _SEARCH)
        self.assertEqual(forwarded.count("-F"), 0)
        self.assertEqual(forwarded.count("-I"), 0)
        self.assertEqual(forwarded[forwarded.index("-D") + 1], "RELEASE")
        self.assertNotIn("DEBUG", forwarded)
        self.assertNotIn("/repo/hmap/foo.hmap", forwarded)
        self.assertEqual(
            forwarded[forwarded.index("--Xcc") + 1], "-F/repo/Vendor/Frameworks"
        )

    def test_missing_resource_dir_trips_an_assert(self):
        with self.assertRaises(AssertionError):
            self.run_tool(_BASE + _SEARCH)
