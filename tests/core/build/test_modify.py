# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

# pyre-strict


import fileinput
import os
from pathlib import Path

from buck2.tests.e2e_util.api.buck import Buck
from buck2.tests.e2e_util.asserts import expect_failure
from buck2.tests.e2e_util.buck_workspace import buck_test
from buck2.tests.e2e_util.helper.utils import random_string


@buck_test(data_dir="modify")
async def test_modify_genrule(buck: Buck) -> None:
    result = await buck.build("//:writer")
    output = result.get_build_report().output_for_target("root//:writer")
    assert Path(output).read_text() == "HELLO\n"

    # Change "HELLO" in TARGETS to "GOODBYE"
    with fileinput.input(buck.cwd / "TARGETS.fixture", inplace=True) as f:
        for line in f:
            print(line.replace("HELLO", "GOODBYE"), end="")

    result = await buck.build("//:writer")
    output = result.get_build_report().output_for_target("root//:writer")
    assert Path(output).read_text() == "GOODBYE\n"


@buck_test(data_dir="modify")
async def test_modify_src(buck: Buck) -> None:
    result = await buck.build("//:mysrcrule")
    output = result.get_build_report().output_for_target("root//:mysrcrule")
    assert Path(output).read_text() == "HELLO\n"

    (buck.cwd / "src.txt").write_text("GOODBYE\n")
    result = await buck.build("//:mysrcrule")
    output = result.get_build_report().output_for_target("root//:mysrcrule")
    assert Path(output).read_text() == "GOODBYE\n"


@buck_test(data_dir="modify")
async def test_modify_genrule_notify(buck: Buck) -> None:
    with open(buck.cwd / ".buckconfig", "a") as buckconfig:
        buckconfig.write("\n[buck2]\nfile_watcher = notify")
    await buck.kill()  # Ensure the config gets picked up
    await test_modify_genrule(buck)


@buck_test(data_dir="modify")
async def test_modify_directory(buck: Buck) -> None:
    # Test for the bug reported in T99593442
    os.mkdir(buck.cwd / "a_dir")
    with open(buck.cwd / "a_dir" / "test.txt", "w") as file:
        file.write("test")
    await buck.build("//:writer")
    # Remove a directory, and change a file, so the file gets spotted,
    # and we'd better note that the directory no longer exists
    os.remove(buck.cwd / "a_dir" / "test.txt")
    os.rmdir(buck.cwd / "a_dir")
    await buck.build("//:writer")


@buck_test(data_dir="modify_file_during_build")
async def test_modify_file_during_build(buck: Buck) -> None:
    # We need to write some random stuff to the file first so that Buck will
    # have to attempt to upload it to RE (which will fail because by that time
    # we will have overwritten it with other content).
    with open(buck.cwd / "text", "w", encoding="utf-8") as f:
        f.write(random_string())

    await expect_failure(
        buck.build("//:check"),
        stderr_regex="modified files while the build was in progress",
    )


@buck_test(data_dir="modify_file_during_build")
async def test_file_notify(buck: Buck) -> None:
    # We need to write some random stuff to the file first so that Buck will
    # have to attempt to upload it to RE (which will fail because by that time
    # we will have overwritten it with other content).
    with open(buck.cwd / "text", "w", encoding="utf-8") as f:
        f.write(random_string())

    await expect_failure(
        buck.build("//:check"),
        stderr_regex="modified files while the build was in progress",
    )
