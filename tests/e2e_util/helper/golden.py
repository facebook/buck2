# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

# pyre-unsafe

import os
import re
import typing

from pathlib import Path


def _prepend_header(content: str) -> str:
    return (
        f"# This file is {'@'}generated, "
        f"regenerate by re-running test with `-- --env BUCK2_UPDATE_GOLDEN=1` appended to the test command\n\n{content}"
    )


def _remove_ci_labels(content: str) -> str:
    # this label is only added for CI jobs, causing inconsistenty between local test and ci test.
    # Examples:
    #  "ci:overwrite",
    #  "ci:diff:linux:@fbcode//mode/dev-lg",
    #  "ci:continuous:linux:@fbcode//mode/dev-lg",
    new_content = []
    for line in content.splitlines():
        if "ci:" in line:
            continue
        new_content.append(line)
    return "\n".join(new_content)


def _normalize_newlines(content: str) -> str:
    """
    We use golden() with text data so in the interest of being a bit more
    platform independent we just normalize the newlines.
    """
    return "".join([line + "\n" for line in content.splitlines()])


def _test_repo_data_src() -> str:
    # `TEST_REPO_DATA_SRC` is set in the test runner
    dir = os.getenv("TEST_REPO_DATA_SRC")
    assert dir, "TEST_REPO_DATA_SRC must be set"
    return dir


def _unified_diff(
    *,
    left: str,
    right: str,
    file: str,
    context: int,
) -> str:
    import difflib

    return "".join(
        difflib.unified_diff(
            left.splitlines(keepends=True),
            right.splitlines(keepends=True),
            fromfile=file,
            tofile=file,
            n=context,
        ),
    )


def _is_update_invocation() -> bool:
    return os.getenv("BUCK2_UPDATE_GOLDEN") is not None


# Output is a map of `rel_path`-relative files to their expected values
def golden_dir(*, output: typing.Dict[str, str], rel_path: str) -> None:
    assert "golden" in rel_path, f"Golden path `{rel_path}` must contain `golden`"

    rel_path_path = Path(rel_path)

    for file, contents in output.items():
        golden(
            output=contents,
            rel_path=str(rel_path_path.joinpath(Path(file))),
        )

    # Check that there are no extra files
    path_in_src = Path(_test_repo_data_src()).joinpath(rel_path_path)

    for file in path_in_src.glob("**/*"):
        if file.is_dir():
            continue
        rel_file_path = str(file.relative_to(path_in_src))
        if rel_file_path not in output:
            if _is_update_invocation():
                file.unlink()
            else:
                raise AssertionError(
                    f"Extra golden file `{rel_file_path}` found, please remove it"
                )


def golden(*, output: str, rel_path: str) -> None:
    assert "golden" in rel_path, f"Golden path `{rel_path}` must contain `golden`"

    output = _prepend_header(output)
    output = _normalize_newlines(output)

    path_in_src = os.path.join(_test_repo_data_src(), rel_path)

    if _is_update_invocation():
        Path(path_in_src).parent.mkdir(parents=True, exist_ok=True)
        with open(path_in_src, "w") as f:
            f.write(output)
        return

    assert os.path.exists(path_in_src), f"Golden path `{path_in_src}` must exist"

    with open(path_in_src, "r") as f:
        expected = f.read()

    if _remove_ci_labels(expected) != _remove_ci_labels(output):
        unified_diff = _unified_diff(
            left=expected,
            right=output,
            file=path_in_src,
            context=3,
        )
        raise AssertionError(
            f"Expected golden file to match actual\n"
            f"\n\n{unified_diff}\n\n"
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


def golden_replace_temp_path(*, output: str, rel_path: str, tmp_path: str) -> None:
    # Escaping backslashes are needed for windows paths
    tmp_path_escaped = tmp_path.replace("\\", "\\\\")
    golden(
        output=output.replace(tmp_path_escaped, "tmp-path").replace("\\\\", "/"),
        rel_path=rel_path,
    )
