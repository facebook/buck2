# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

# pyre-strict


import os
from pathlib import Path
from typing import Any, Union

import pytest
from buck2.tests.e2e_util.api.buck import Buck
from buck2.tests.e2e_util.buck_workspace import buck_test

output_cleanup_targets = [
    "local_action",
    "remote_action",
    "symlinked_dir",
    "write",
    "copy",
    "local_readonly_file",
    "local_readonly_dir",
    "local_nonexec_dir",
    "remote_readonly_file",
    "remote_readonly_dir",
    "remote_nonexec_dir",
]


@buck_test()
# Note: listing these second...first makes the parameterization appear [first-second-...] in the job names
@pytest.mark.parametrize(
    "second",
    output_cleanup_targets,
)
@pytest.mark.parametrize(
    "first",
    output_cleanup_targets,
)
async def test_output_cleanup(
    buck: Buck, tmp_path: Path, first: str, second: str
) -> None:
    def read_dir(d: Path) -> dict[str, Any]:
        steps = 0
        out: dict[str, Any] = {}

        for root, dirs, files in os.walk(d, topdown=False):
            for name in files:
                path = os.path.join(root, name)
                out[os.path.relpath(path, d)] = open(path).read()
                steps += 1
            for name in dirs:
                path = os.path.join(root, name)
                out.setdefault(os.path.relpath(path, d), {})
                steps += 1

        return out

    rebuild = tmp_path / "rebuild"
    clean = tmp_path / "clean"
    first = f"{first}-a"
    second = f"{second}-b"

    await buck.build(":main", "-c", f"test.main={first}")
    await buck.build(":main", "-c", f"test.main={second}", "--out", str(rebuild))

    await buck.clean()
    await buck.build(":main", "-c", f"test.main={second}", "--out", str(clean))

    assert read_dir(rebuild) == read_dir(clean)


@buck_test()
@pytest.mark.parametrize("kind", ["readonly_file", "readonly_dir", "nonexec_dir"])
async def test_permissions_match_local_remote(
    buck: Buck, tmp_path: Path, kind: str
) -> None:
    target = "root//:main"
    lhs = f"local_{kind}-a"
    rhs = f"remote_{kind}-a"

    def add_entry(
        ret: dict[str, tuple[Union[int, str], ...]],
        root: Path,
        path: Path,
        with_file_contents: bool,
    ) -> None:
        key = path.relative_to(root)
        s = path.stat(follow_symlinks=False)
        entry = (s.st_mode,)
        if path.exists() and path.is_file() and with_file_contents:
            with open(path) as f:
                entry += (f.read(),)
        ret[str(key)] = entry

    def get_entries(
        root: Path, with_file_contents: bool
    ) -> dict[str, tuple[Union[int, str], ...]]:
        ret = {}
        if root.is_file():
            add_entry(ret, root, root, with_file_contents)
        else:
            for dirname, _, filenames in os.walk(root):
                dirname = Path(dirname)
                add_entry(ret, root, dirname, with_file_contents)
                for filename in filenames:
                    add_entry(ret, root, dirname / filename, with_file_contents)
        return ret

    lhs_res = (
        (await buck.build(":main", "-c", f"test.main={lhs}"))
        .get_build_report()
        .output_for_target(target)
    )
    lhs_entries = get_entries(lhs_res, False)
    lhs_entries_and_contents = get_entries(lhs_res, True)

    await buck.clean()
    rhs_res = (
        (await buck.build(":main", "-c", f"test.main={rhs}"))
        .get_build_report()
        .output_for_target(target)
    )
    rhs_entries = get_entries(rhs_res, False)
    rhs_entries_and_contents = get_entries(rhs_res, True)

    if lhs_entries != rhs_entries:
        raise ValueError(
            f"Permissions mismatch between local and remote: lhs_entries != rhs_entries: {lhs_entries} != {rhs_entries}"
        )
    if lhs_entries_and_contents == rhs_entries_and_contents:
        raise ValueError(
            f"Test error, contents of files weren't different: {lhs_entries_and_contents} != {rhs_entries_and_contents}"
        )
