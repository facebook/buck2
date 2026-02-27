# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

# pyre-strict


from buck2.tests.e2e_util.api.buck import Buck
from buck2.tests.e2e_util.buck_workspace import buck_test


@buck_test()
async def test_clean_stale_bxl(buck: Buck) -> None:
    await buck.bxl("//clean_stale/build.bxl:build_test")

    art_files = [path.name for path in (buck.cwd / "buck-out/v2/art").glob("**/*")]
    assert "out.json" in art_files

    # Check that artifacts written to art by bxl are not deleted
    await buck.clean("--stale")
    art_files = [path.name for path in (buck.cwd / "buck-out/v2/art").glob("**/*")]
    assert "out.json" in art_files

    # Force clean of tracked artifacts, check that art is deleted but not bxl
    await buck.kill()
    await buck.clean("--stale=0s")

    art_files = [path.name for path in (buck.cwd / "buck-out/v2/art").glob("**/*")]
    assert "out.json" not in art_files

    # TODO these should probably be tracked and cleaned too (write to art instead?)
    art_bxl_files = [
        path.name for path in (buck.cwd / "buck-out/v2/art-bxl").glob("**/*")
    ]
    assert "foo_out" in art_bxl_files
