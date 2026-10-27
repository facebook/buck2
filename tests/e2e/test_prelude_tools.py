# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

# pyre-strict

"""Runs prelude tools the way the rules run them: as actions of a buck2 build, through the
tools' command lines, in a workspace that uses the bundled prelude."""

from pathlib import Path

from buck2.tests.e2e_util.api.buck import Buck
from buck2.tests.e2e_util.buck_workspace import buck_test, BuckTestCase


async def _output(buck: Buck, target: str) -> Path:
    res = await buck.build_without_report(target, "--show-full-simple-output")
    return Path(res.stdout.strip())


async def _lines(buck: Buck, target: str) -> list[str]:
    return (await _output(buck, target)).read_text().splitlines()


class PreludeToolsTest(BuckTestCase):
    # prelude/python/tools/make_py_package_manifest_module.py
    @buck_test(inplace=False)
    async def test_manifest_module_lists_import_names(self, buck: Buck) -> None:
        namespace: dict[str, object] = {}
        exec((await _output(buck, "root//:manifest_module")).read_text(), namespace)
        assert namespace["modules"] == ["pkg.__init__", "pkg._speedups"]
