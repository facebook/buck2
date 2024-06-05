# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

# pyre-unsafe

import os
import re


def _prepend_header(content: str) -> str:
    return (
        f"# This file is {'@'}generated, "
        f"regenerate by re-running test with `-- --env BUCK2_UPDATE_GOLDEN=1` appended to the test command\n\n{content}"
    )


def _remove_ci_labels(content: str) -> str:
    # this label is only added for CI jobs, causing inconsistenty between local test and ci test.
    new_content = []
    for line in content.splitlines():
        if "ci:overwrite" in line:
            continue
        new_content.append(line)
    return "\n".join(new_content)


def _replace_windows_newlines(content: str) -> str:
    """
    We use golden() with text data so in the interest of being a bit more
    platform independent we just normalize the newlines.
    """
    return content.replace("\r\n", "\n")


def _test_repo_data_src() -> str:
    # `TEST_REPO_DATA_SRC` is set in the test runner
    dir = os.getenv("TEST_REPO_DATA_SRC")
    assert dir, "TEST_REPO_DATA_SRC must be set"
    return dir


def golden(*, output: str, rel_path: str) -> None:
    assert "golden" in rel_path, f"Golden path `{rel_path}` must contain `golden`"

    output = _prepend_header(output)
    output = _replace_windows_newlines(output)

    path_in_src = os.path.join(_test_repo_data_src(), rel_path)

    if os.getenv("BUCK2_UPDATE_GOLDEN"):
        with open(path_in_src, "w") as f:
            f.write(output)
        return

    assert os.path.exists(path_in_src), f"Golden path `{path_in_src}` must exist"

    with open(path_in_src, "r") as f:
        expected = f.read()

    if _remove_ci_labels(expected) != _remove_ci_labels(output):
        raise AssertionError(
            f"Expected golden file {path_in_src} to match actual\n"
            f"Expected:\n\n{expected}\n\n"
            "End of expected.\n"
            f"Actual:\n\n{output}\n"
            "End of actual.\n"
            "Re-run test with `-- --env BUCK2_UPDATE_GOLDEN=1` appended to the test command to regenerate the files"
        )


# Replace 128-bit configuration with placeholder.
def _replace_cfg_hash(s: str) -> str:
    return re.sub(r"\b[0-9a-f]{16}\b", "<HASH>", s)


def golden_replace_cfg_hash(*, output: str, rel_path: str) -> None:
    golden(
        output=_replace_cfg_hash(output),
        rel_path=rel_path,
    )
