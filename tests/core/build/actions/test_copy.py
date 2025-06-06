# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

# pyre-strict


import pytest
from buck2.tests.e2e_util.api.buck import Buck
from buck2.tests.e2e_util.buck_workspace import buck_test


@pytest.mark.parametrize("executable_bit_override", [None, True, False])
@pytest.mark.parametrize("write_executable_bit", [None, True, False])
@pytest.mark.parametrize(
    "src,is_executable",
    [
        (None, None),
        ("files/is_executable.sh", True),
        ("files/not_executable.sh", False),
        ("files/executable_scripts", True),
        ("files/not_executable_scripts", False),
    ],
)
@buck_test(skip_for_os=["windows"])  # Exec bit and all
async def test_exec_bit_of_copied_file(
    buck: Buck,
    executable_bit_override: bool | None,
    write_executable_bit: bool | None,
    src: str | None,
    is_executable: bool | None,
) -> None:
    if src is None and write_executable_bit is None:
        return
    if src is not None and write_executable_bit is not None:
        return

    if executable_bit_override is not None:
        is_executable = executable_bit_override
    elif write_executable_bit is not None:
        is_executable = write_executable_bit

    assert is_executable is not None

    name = "perms_{}_{}_{}".format(executable_bit_override, write_executable_bit, src)

    res = await buck.build_without_report(
        f":{name}", "--out=-", "--local-only", "--no-remote-cache"
    )

    expected_val = "x" if is_executable else "-"

    for line in res.stdout.strip().split():
        line = line.strip()
        assert line[3] == expected_val
        assert line[6] == expected_val
        assert line[9] == expected_val


@buck_test()  # Make sure there's at least one test defined
async def test_dummy(buck: Buck) -> None:
    pass
