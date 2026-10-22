# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

"""
With `buck2.readonly_outputs` on, every file the disk-state core tracks is read-only from the
moment it is tracked, directories stay writable, and an incremental action's previous outputs
are made writable for its run and read-only again after it.
"""

import stat
import sys
from pathlib import Path

from buck2.tests.e2e_util.api.buck import Buck
from buck2.tests.e2e_util.buck_workspace import buck_test, BuckTestCase
from buck2.tests.e2e_util.helper.utils import random_string


def _read_only(path: Path) -> bool:
    mode = path.stat().st_mode
    if sys.platform == "win32":
        return not mode & stat.S_IWRITE
    return mode & 0o222 == 0


class ReadonlyOutputsTest(BuckTestCase):
    @buck_test(skip_for_materializer=["deferred"])
    async def test_tracked_files_are_read_only(self, buck: Buck) -> None:
        for target in ["//:write", "//:produce", "//:copy_src"]:
            res = await buck.build(target, "--local-only")
            out = res.get_build_report().output_for_target(target)
            assert _read_only(out), target
            assert not _read_only(out.parent), "directories stay writable"
            try:
                out.write_text("edited")
            except PermissionError:
                pass
            else:
                raise AssertionError(f"{target}: a write to the output went through")

    @buck_test(skip_for_materializer=["deferred"])
    async def test_an_incremental_action_writes_its_previous_output(
        self, buck: Buck
    ) -> None:
        expected = "foo"
        seed = ""
        for _ in range(3):
            seed = random_string()
            res = await buck.run(
                "//:incremental", "--local-only", "-c", f"test.seed={seed}"
            )
            assert res.stdout == expected
            expected += " bar"

        # Read-only again once the run is over.
        res = await buck.build(
            "//:incremental", "--local-only", "-c", f"test.seed={seed}"
        )
        assert _read_only(res.get_build_report().output_for_target("//:incremental"))

    @buck_test(skip_for_materializer=["deferred"])
    async def test_clean_removes_read_only_outputs(self, buck: Buck) -> None:
        res = await buck.build("//:produce", "--local-only")
        out = res.get_build_report().output_for_target("//:produce")
        assert out.exists()
        await buck.clean()
        assert not out.exists()
        res = await buck.build("//:produce", "--local-only")
        assert _read_only(res.get_build_report().output_for_target("//:produce"))
