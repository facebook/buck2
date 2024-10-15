# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

# pyre-strict


import os
import platform
from typing import Iterable

from buck2.tests.e2e_util.api.buck import Buck
from buck2.tests.e2e_util.buck_workspace import buck_test


@buck_test()
async def test_clean(buck: Buck) -> None:
    build_result = await buck.build("root//:trivial_build")
    build_report = build_result.get_build_report()
    build_report_outputs = [
        str(output)
        for output in build_report.outputs_for_target("root//:trivial_build")
    ]

    clean_result = await buck.clean()
    clean_paths = tuple(filter(None, clean_result.stderr.split("\n")))

    for output in build_report_outputs:
        assert output.startswith(clean_paths)

    _assert_all_paths_do_not_exist(build_report_outputs)


@buck_test()
async def test_clean_dry_run(buck: Buck) -> None:
    build_result = await buck.build("root//:trivial_build", "--show-output")
    build_report = build_result.get_build_report()
    build_report_outputs = [
        str(output)
        for output in build_report.outputs_for_target("root//:trivial_build")
    ]

    dry_clean_result = await buck.clean("--dry-run")

    dry_clean_paths = set(
        filter(
            is_buck_path,
            dry_clean_result.stderr.split("\n"),
        )
    )
    _assert_all_paths_exist(dry_clean_paths)

    dry_clean_paths = tuple(i for i in dry_clean_paths)
    for output in build_report_outputs:
        assert output.startswith(dry_clean_paths)

    _assert_all_paths_exist(build_report_outputs)

    # Run clean without dry-run and make sure all files are removed now
    clean_result = await buck.clean()
    clean_paths = set(
        filter(
            is_buck_path,
            clean_result.stderr.split("\n"),
        )
    )
    # dry_clean_paths and clean_paths should be the same
    for clean_path in clean_paths:
        assert clean_path in dry_clean_paths
    for dry_clean_path in dry_clean_paths:
        assert dry_clean_path in clean_paths

    _assert_all_paths_do_not_exist(clean_paths)


def is_buck_path(x: str) -> bool:
    if platform.system() == "Windows":
        return "\\.buck\\buckd\\" in x or "\\buck-out\\" in x
    else:
        return "/.buck/buckd/" in x or "/buck-out/" in x


def _assert_all_paths_exist(paths: Iterable[str]) -> None:
    for path in paths:
        assert os.path.exists(path) is True


def _assert_all_paths_do_not_exist(paths: Iterable[str]) -> None:
    for path in paths:
        if os.path.exists(f"{path}/buckd.lifecycle"):
            # Clean keeps lifecycle file in daemon dir.
            assert ["buckd.lifecycle"] == os.listdir(path)
        else:
            assert os.path.exists(path) is False
