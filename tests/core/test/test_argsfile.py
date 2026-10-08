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
from buck2.tests.e2e_util.helper.utils import read_what_ran

# The test binaries assert on the shape of the command they were given, so a
# passing run means Buck expanded the argsfile to `@<path>` and wrote a file
# holding the case, and a failing one means it did not. Nothing else covers the
# Buck side of this: TPX's own suites stop at the command it hands over.


async def _test_argsfile(buck: Buck, *targets: str, remote: bool) -> None:
    # One invocation for all targets: a `@buck_test` builds its own workspace, so splitting them up
    # would pay for that repeatedly to assert parts of the same behaviour. `argsfile` and
    # `argsfile_absolute` differ only in whether the suite takes project-relative paths, which is
    # what decides how the `@<path>` is rendered.
    await buck.test(
        "-c",
        f"test.local_enabled={str(not remote).lower()}",
        "-c",
        f"test.remote_enabled={str(remote).lower()}",
        *targets,
    )

    # Only a run that really happened remotely shows the argsfile reaching the executor as an
    # action input, so a silent fallback to local would make the remote variant meaningless.
    executors = {
        entry["identity"]: entry["reproducer"]["executor"]
        for entry in await read_what_ran(buck)
        if entry["reason"] == "test.run"
    }
    expected = "Re" if remote else "Local"
    assert executors == {f"root{t}": expected for t in targets}


@buck_test()
async def test_runner_argsfile_reaches_the_test_locally(buck: Buck) -> None:
    await _test_argsfile(
        buck, "//:argsfile", "//:argsfile_absolute", "//:inline", remote=False
    )


@buck_test()
async def test_runner_argsfile_reaches_the_test_remotely(buck: Buck) -> None:
    # No `argsfile_absolute`: a test that takes absolute paths can only run locally.
    await _test_argsfile(buck, "//:argsfile", "//:inline", remote=True)
