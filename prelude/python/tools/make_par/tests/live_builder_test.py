# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

# pyre-ignore-all-errors

import os
import subprocess
import sys
import tempfile
import unittest
from pathlib import Path

import gen_lpar_bootstrap

TEMPLATE = Path(__file__).resolve().parent.parent / "_lpar_bootstrap.sh.template"


def _generate(tmp, *runtime_env):
    """Run the generator the way `make_py_package.bzl` does, on `sys.executable`."""
    bootstrap = tmp / "boot.sh"
    header = tmp / "bin.par"
    argv = [
        "gen_lpar_bootstrap.py",
        "--template",
        str(TEMPLATE),
        "--bootstrap-output",
        str(bootstrap),
        "--output",
        str(header),
        "--main-runner",
        "__par__.run_as_main",
        "--main-module",
        "app.main",
        "--python",
        sys.executable,
    ]
    argv.extend(f"--runtime_env={entry}" for entry in runtime_env)
    old_argv = sys.argv
    sys.argv = argv
    try:
        gen_lpar_bootstrap.main()
    finally:
        sys.argv = old_argv
    return bootstrap, header


class RuntimeEnvTest(unittest.TestCase):
    """`runtime_env` values reach the program through the generated bootstrap."""

    def _values_seen_by_the_program(self, *runtime_env):
        with tempfile.TemporaryDirectory() as tmpdir:
            tmp = Path(tmpdir)
            bootstrap, _ = _generate(tmp, *runtime_env)
            show = tmp / "show_env.py"
            show.write_text(
                "import os\n"
                "for name in ('JAVA_OPTS', 'PRICE', 'GREETING'):\n"
                "    print(repr(os.environ.get(name)))\n",
                encoding="utf8",
            )
            proc = subprocess.run(
                ["bash", str(bootstrap), str(show)],
                capture_output=True,
                encoding="utf8",
                env={"PATH": os.environ.get("PATH", "/usr/bin:/bin")},
            )
            return proc.stdout.splitlines(), proc.stderr

    def _syntax_check(self, *runtime_env):
        with tempfile.TemporaryDirectory() as tmpdir:
            bootstrap, _ = _generate(Path(tmpdir), *runtime_env)
            return subprocess.run(
                ["bash", "-n", str(bootstrap)], capture_output=True, encoding="utf8"
            )

    def test_a_value_with_a_space_or_a_dollar_is_cut(self):
        values, stderr = self._values_seen_by_the_program(
            "JAVA_OPTS=-Xmx1g -Xms1g", "PRICE=$5"
        )
        self.assertEqual(values, ["'-Xmx1g'", "''", "None"])
        self.assertIn("not a valid identifier", stderr)

    def test_a_value_with_an_apostrophe_breaks_the_bootstrap(self):
        proc = self._syntax_check("GREETING=it's")
        self.assertEqual(proc.returncode, 2, proc.stderr)
