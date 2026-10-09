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
                "for name in ('JAVA_OPTS', 'PRICE', 'GREETING', 'MSG', 'PATHX', 'WIN'):\n"
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

    def test_values_are_double_quoted_like_the_fastzip_wrapper(self):
        values, stderr = self._values_seen_by_the_program(
            "JAVA_OPTS=-Xmx1g -Xms1g",
            "PRICE=$5",
            "GREETING=it's",
            'MSG=say "hi"',
            "PATHX=x:$PATH",
            "WIN=C:\\dir\\",
        )
        path = os.environ.get("PATH", "/usr/bin:/bin")
        self.assertEqual(
            values,
            [
                "'-Xmx1g -Xms1g'",
                "''",
                '"it\'s"',
                "'say \"hi\"'",
                repr(f"x:{path}"),
                "'C:\\\\dir\\\\'",
            ],
        )
        self.assertEqual(stderr, "")

    def test_a_value_with_an_apostrophe_keeps_the_bootstrap_valid(self):
        proc = self._syntax_check("GREETING=it's", 'MSG=say "hi"', "WIN=C:\\dir\\")
        self.assertEqual(proc.returncode, 0, proc.stderr)


class HeaderTest(unittest.TestCase):
    """The header finds its link tree next to the par it was invoked as."""

    def _run_from(self, dirname):
        with tempfile.TemporaryDirectory() as tmpdir:
            tmp = Path(tmpdir)
            _, header = _generate(tmp)
            run_dir = tmp / dirname
            linktree = run_dir / "bin#link-tree"
            linktree.mkdir(parents=True)
            stub = linktree / "_bootstrap.sh"
            stub.write_text('#!/bin/bash\necho "BOOTSTRAP REACHED"\n', encoding="utf8")
            stub.chmod(0o755)
            par = run_dir / "bin.par"
            par.write_text(header.read_text(encoding="utf8"), encoding="utf8")
            par.chmod(0o755)
            return subprocess.run(
                ["./bin.par"], cwd=run_dir, capture_output=True, encoding="utf8"
            )

    def test_a_path_with_a_space_reaches_the_bootstrap(self):
        self.assertIn("BOOTSTRAP REACHED", self._run_from("nospace").stdout)
        self.assertIn("BOOTSTRAP REACHED", self._run_from("with space").stdout)
