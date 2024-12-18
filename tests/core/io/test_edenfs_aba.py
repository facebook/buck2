# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

# pyre-strict


import json
import subprocess

from buck2.tests.e2e_util.api.buck import Buck
from buck2.tests.e2e_util.buck_workspace import buck_test


async def _get_files(buck: Buck) -> list[str]:
    res = await buck.targets("root//:")
    for x in res.stderr.splitlines():
        p = x.split("Files: ", 1)
        if len(p) > 1:
            return json.loads(p[1])
    raise Exception("Missing files output: " + res.stderr)


async def _run_test(buck: Buck) -> None:
    # Fails on eden because the repo exists, that's ok
    subprocess.run(["sl", "init"], cwd=buck.cwd)
    subprocess.run(["sl", "commit", "--addremove", "-m", "temp"], cwd=buck.cwd)
    subprocess.run(["sl", "bookmark", "main"], cwd=buck.cwd, check=True)

    assert subprocess.check_output(["sl", "status"], cwd=buck.cwd) == b""

    assert (await _get_files(buck)) == ["files/abc", "files/d/empty"]

    subprocess.run(["sl", "mv", "files/abc", "files/d/"], cwd=buck.cwd, check=True)
    assert (await _get_files(buck)) == ["files/d/abc", "files/d/empty"]

    subprocess.run(["sl", "shelve"], cwd=buck.cwd, check=True)
    assert (await _get_files(buck)) == ["files/abc", "files/d/empty"]


@buck_test(setup_eden=True)
async def test_edenfs_aba_eden(buck: Buck) -> None:
    await _run_test(buck)
