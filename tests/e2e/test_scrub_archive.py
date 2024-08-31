# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

# pyre-strict


import subprocess
from time import sleep

from buck2.tests.e2e_util.api.buck import Buck
from buck2.tests.e2e_util.buck_workspace import buck_test, env


# TODO(marwhal): Fix and enable on Windows
@buck_test(inplace=True, skip_for_os=["windows"])
@env("BUCK2_TEST_DISABLE_CACHING", "true")
async def test_scrub_archive(buck: Buck) -> None:
    # FIXME: This test is timing out regluarly in CI and causing flakiness
    if True:
        return
    target = "fbsource//fbobjc/buck2/tests/use_system_frameworks:library"
    args = [target, "--prefer-local", "--show-output", "--no-remote-cache"]
    result1 = await buck.build(*args)
    output1 = result1.get_build_report().output_for_target(target)
    await buck.kill()
    sleep(2)  # so that timestamp of static lib changes
    result2 = await buck.build(*args)
    output2 = result2.get_build_report().output_for_target(target)
    ret = subprocess.call(["diff", "-s", output1, output2])
    assert ret == 0


# TODO(marwhal): Add this back one at least one test in this file passes on Windows
@buck_test(inplace=True)
async def test_noop(buck: Buck) -> None:
    return
