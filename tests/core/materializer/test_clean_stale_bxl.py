# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

# pyre-strict


from buck2.tests.e2e_util.api.buck import Buck
from buck2.tests.e2e_util.buck_workspace import buck_test


@buck_test()
async def test_clean_stale_bxl(buck: Buck) -> None:
    await buck.bxl("//clean_stale/build.bxl:build_test")

    gen_files = [path.name for path in (buck.cwd / "buck-out/v2/gen").glob("**/*")]
    assert "out.json" in gen_files

    # Check that artifacts written to gen by bxl are not deleted
    await buck.clean("--stale")
    gen_files = [path.name for path in (buck.cwd / "buck-out/v2/gen").glob("**/*")]
    assert "out.json" in gen_files

    # Force clean of tracked artifacts, check that gen is deleted but not bxl
    await buck.kill()
    await buck.clean("--stale=0s")

    gen_files = [path.name for path in (buck.cwd / "buck-out/v2/gen").glob("**/*")]
    assert "out.json" not in gen_files

    # TODO these should probably be tracked and cleaned too (write to gen instead?)
    gen_bxl_files = [
        path.name for path in (buck.cwd / "buck-out/v2/gen-bxl").glob("**/*")
    ]
    assert "foo_out" in gen_bxl_files
