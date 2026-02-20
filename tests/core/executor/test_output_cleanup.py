# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

# pyre-strict


import os
import sys
from pathlib import Path
from typing import Any

import pytest
from buck2.tests.e2e_util.api.buck import Buck
from buck2.tests.e2e_util.api.buck_result import BuckException
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


# RE is unable to list and upload files from nonexec directories on mac
DARWIN_EXPECTED_UPLOAD_ERROR = "re_message=CAS operation[uploading_directories] failed"
EXPECTED_FAILURES: dict[str, str] = {
    k: DARWIN_EXPECTED_UPLOAD_ERROR
    for k in [
        "darwin-test_permissions_match_local_remote[nonexec_dir]",
    ]
}
for x in output_cleanup_targets:
    EXPECTED_FAILURES[f"darwin-test_output_cleanup[remote_nonexec_dir-{x}]"] = (
        DARWIN_EXPECTED_UPLOAD_ERROR
    )
    EXPECTED_FAILURES[f"darwin-test_output_cleanup[{x}-remote_nonexec_dir]"] = (
        DARWIN_EXPECTED_UPLOAD_ERROR
    )

# TODO: remove keep_temp when we remove expected_failures
expected_failures_linux: set[str] = {
    "symlinked_dir-local_nonexec_dir",
    "write-local_readonly_dir",
    "local_readonly_file-local_readonly_dir",
}

# TODO: remove keep_temp when we remove expected_failures
expected_failures_darwin: set[str] = {
    "write-local_readonly_dir",
    "symlinked_dir-local_nonexec_dir",
    "local_readonly_file-local_readonly_dir",
}

# TODO: remove keep_temp when we remove expected_failures
expected_failures_nonwin: set[str] = {
    "copy-local_nonexec_dir",
    "copy-local_readonly_dir",
    "local_action-local_nonexec_dir",
    "local_action-local_readonly_dir",
    "local_nonexec_dir-copy",
    "local_nonexec_dir-local_action",
    "local_nonexec_dir-local_nonexec_dir",
    "local_nonexec_dir-local_readonly_dir",
    "local_nonexec_dir-local_readonly_file",
    "local_nonexec_dir-remote_action",
    "local_nonexec_dir-remote_nonexec_dir",
    "local_nonexec_dir-remote_readonly_dir",
    "local_nonexec_dir-remote_readonly_file",
    "local_nonexec_dir-symlinked_dir",
    "local_nonexec_dir-write",
    "local_readonly_dir-copy",
    "local_readonly_dir-local_action",
    "local_readonly_dir-local_nonexec_dir",
    "local_readonly_dir-local_readonly_dir",
    "local_readonly_dir-local_readonly_file",
    "local_readonly_dir-remote_action",
    "local_readonly_dir-remote_nonexec_dir",
    "local_readonly_dir-remote_readonly_dir",
    "local_readonly_dir-remote_readonly_file",
    "local_readonly_dir-symlinked_dir",
    "local_readonly_dir-write",
    "local_readonly_file-local_nonexec_dir",
    "remote_action-local_nonexec_dir",
    "remote_action-local_readonly_dir",
    "remote_nonexec_dir-local_nonexec_dir",
    "remote_nonexec_dir-local_readonly_dir",
    "remote_readonly_dir-local_nonexec_dir",
    "remote_readonly_dir-local_readonly_dir",
    "remote_readonly_file-local_nonexec_dir",
    "remote_readonly_file-local_readonly_dir",
    "symlinked_dir-local_readonly_dir",
    "write-local_nonexec_dir",
}

expected_failures: set[tuple[str, str]] = (
    (expected_failures_nonwin if sys.platform != "win32" else set())
    | (expected_failures_linux if sys.platform == "linux" else set())
    | (expected_failures_darwin if sys.platform == "darwin" else set())
)


# TODO: remove
def or_perm(path: Path, mode: int, *, domod: bool) -> None:
    before = path.stat().st_mode
    if domod:
        os.chmod(str(path), before | mode)
    print(
        f"MOD {path!s} {before:o} => {path.stat().st_mode:o}",
        file=sys.stderr,
    )


# TODO: remove
def walkit(root: Path, domod: bool = False) -> None:
    for dirname, dirnames, filenames in os.walk(root):
        dirname = Path(dirname)

        or_perm(dirname, 0o755, domod=domod)
        for name in filenames:
            or_perm(dirname / name, 0o644, domod=domod)
        for name in dirnames:
            or_perm(dirname / name, 0o755, domod=domod)


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
    test_key = f"{sys.platform}-test_output_cleanup[{first}-{second}]"
    expect_key = f"{first}-{second}"

    first = f"{first}-a"
    second = f"{second}-b"

    expect_failure = expect_key in expected_failures

    try:
        await buck.build(":main", "-c", f"test.main={first}")
        await buck.build(":main", "-c", f"test.main={second}", "--out", str(rebuild))

        await buck.clean()
        await buck.build(":main", "-c", f"test.main={second}", "--out", str(clean))

        assert read_dir(rebuild) == read_dir(clean)

        if test_key in EXPECTED_FAILURES:
            raise Exception(f"Expected failure for {test_key}")
        if expect_failure:
            raise RuntimeError(f"Expected failure from these targets {first} {second}")
    except BuckException as e:
        if expect_failure:
            return
        msg = EXPECTED_FAILURES.get(test_key)
        if msg is not None and e.stderr and msg in e.stderr:
            return
        raise e

    finally:
        # TODO: for now, we have to make this deletable so `buck.clean` can remote the temp dir. Remove this when we fix the permissions.
        walkit(Path(buck.cwd) / "buck-out/v2/gen", domod=True)
