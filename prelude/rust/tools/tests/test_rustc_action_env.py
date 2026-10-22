#!/usr/bin/env fbpython
# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

import json
import pathlib
import runpy
import shutil
import sys
import tempfile
import unittest
from unittest import mock

# Stands in for rustc: records the environment it was started with.
FAKE_RUSTC = (
    "import json, os; "
    "json.dump(dict(os.environ), open(os.environ['FAKE_DUMP_ENV'], 'w'))"
)


def run_rustc_action(argfile: pathlib.Path) -> int:
    with mock.patch.object(sys, "argv", ["rustc_action", f"@{argfile}"]):
        try:
            runpy.run_module("rustc_action", run_name="__main__")
        except SystemExit as e:
            return e.code or 0
    raise AssertionError("rustc_action did not exit")


class EnvTransportTest(unittest.TestCase):
    """`rust_library(env = {"K": value})` reaches rustc through `process_env` in build.bzl, which
    escapes the value onto one line of the `rustc_action` argfile, and `rustc_action.py`, which
    decodes it into rustc's environment."""

    def setUp(self) -> None:
        self.tmp = pathlib.Path(tempfile.mkdtemp())
        self.addCleanup(shutil.rmtree, self.tmp)

    def transport(self, value: str) -> str:
        """The value rustc sees for `K`."""
        dump = self.tmp / "env.json"
        dump.unlink(missing_ok=True)
        args = [
            f"--env=FAKE_DUMP_ENV={dump}",
            "--env=K=" + escape_like_process_env(value),
            f"--diag-json={self.tmp / 'diag.json'}",
            f"--diag-txt={self.tmp / 'diag.txt'}",
            sys.executable,
            "-c",
            FAKE_RUSTC,
            "--rustc-action-separator",
        ]
        argfile = self.tmp / "rustc.args"
        # `at_argfile(allow_args = True)` writes one argument per line.
        argfile.write_text("".join(arg + "\n" for arg in args), encoding="utf-8")
        self.assertEqual(run_rustc_action(argfile), 0)
        return json.loads(dump.read_text())["K"]


def escape_like_process_env(value: str) -> str:
    """The `replace_regex` rules of `process_env` in build.bzl, in order."""
    return value.replace("\\n", "\\\n").replace("\n", "\\n").replace("\r", "\\r")


class EnvTransportPinnedTest(EnvTransportTest):
    def test_newlines_round_trip(self) -> None:
        for value in ["a\nb", "a\\nb", "\\n\n"]:
            self.assertEqual(self.transport(value), value)

    def test_a_carriage_return_reaches_rustc_as_backslash_r(self) -> None:
        self.assertEqual(self.transport("a\rb"), "a\\rb")

    def test_a_backslash_before_a_newline_reaches_rustc_as_backslash_n(self) -> None:
        # Both values are escaped to the same text.
        self.assertEqual(
            escape_like_process_env("a\\\nb"), escape_like_process_env("a\\nb")
        )
        self.assertEqual(self.transport("a\\\nb"), "a\\nb")

    def test_a_form_feed_breaks_the_action(self) -> None:
        # The argfile line is split at the form feed; the tail becomes the rustc command.
        with self.assertRaises(FileNotFoundError):
            self.transport("a\x0cb")

    def test_a_dollar_cat_in_a_value_is_expanded(self) -> None:
        secret = self.tmp / "secret.txt"
        secret.write_text("FILE-CONTENTS")
        self.assertEqual(self.transport(f"echo $(cat {secret})"), "echo FILE-CONTENTS")
