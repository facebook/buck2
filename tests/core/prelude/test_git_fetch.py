# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

# pyre-strict

import subprocess
from pathlib import Path

from buck2.tests.e2e_util.api.buck import Buck
from buck2.tests.e2e_util.buck_workspace import buck_test


def _git(args: list[str], cwd: Path) -> str:
    return subprocess.check_output(["git", *args], cwd=cwd, encoding="utf-8").strip()


def _init_repo(cwd: Path) -> str:
    """Create the repository to fetch from, and return the commit to pin.

    It sits next to the project rather than inside it, so that buck neither sees it as
    source nor watches it.
    """
    repo = (cwd.parent / "remote").absolute()
    repo.mkdir()
    _git(["init"], cwd=repo)
    _git(["config", "user.name", "notarealuser"], cwd=repo)
    _git(["config", "user.email", "notarealuser@fb.com"], cwd=repo)
    # git_fetch asks for a commit by hash, which a repository only serves when told to.
    # The hosts it is aimed at allow that; a freshly created repository does not.
    _git(["config", "uploadpack.allowAnySHA1InWant", "true"], cwd=repo)

    (repo / "hello.txt").write_text("hello\n")
    (repo / "subdir").mkdir()
    (repo / "subdir" / "nested.txt").write_text("nested\n")
    _git(["add", "."], cwd=repo)
    _git(["commit", "-m", "Commit name"], cwd=repo)
    rev = _git(["log", "--format=format:%H", "-1"], cwd=repo)

    with open(cwd / ".buckconfig", "a") as f:
        f.write(f"\n[test_git_fetch]\n  repo = {repo.as_uri()}\n  rev = {rev}\n")
    return rev


@buck_test()
async def test_fetches_the_work_tree(buck: Buck) -> None:
    _init_repo(cwd=buck.cwd)

    res = await buck.build_without_report(
        "root//:fetch.git", "--show-full-simple-output"
    )
    assert (Path(res.stdout.strip()) / "hello.txt").read_text() == "hello\n"


@buck_test()
async def test_requested_paths_are_sub_targets(buck: Buck) -> None:
    _init_repo(cwd=buck.cwd)

    res = await buck.build_without_report(
        "root//:fetch.git[subdir]", "--show-full-simple-output"
    )
    assert (Path(res.stdout.strip()) / "nested.txt").read_text() == "nested\n"


@buck_test()
async def test_git_dir_sub_target_is_a_repository(buck: Buck) -> None:
    rev = _init_repo(cwd=buck.cwd)

    res = await buck.build_without_report(
        "root//:fetch.git[.git]", "--show-full-simple-output"
    )
    git_dir = Path(res.stdout.strip())

    # Validate this is a proper .git tree
    assert _git(["--git-dir", str(git_dir), "cat-file", "-t", rev], cwd=buck.cwd) == (
        "commit"
    )
