# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

# pyre-strict

import json

import pytest
from buck2.tests.e2e_util.api.buck import Buck
from buck2.tests.e2e_util.buck_workspace import buck_test


@pytest.mark.parametrize("hash_function", ["fast", "strong"])
@pytest.mark.parametrize("recursive", ["false", "true"])
@buck_test()
async def test_hash_function_preserves_128_bit_length(
    buck: Buck, hash_function: str, recursive: str
) -> None:
    result = await buck.targets(
        ":foo_dep",
        "--show-unconfigured-target-hash",
        "--json",
        f"--target-hash-function={hash_function}",
        f"--target-hash-recursive={recursive}",
    )
    output = json.loads(result.stdout)
    assert len(output) == 1, output
    assert len(output[0]["buck.target_hash"]) == 32


@buck_test()
async def test_unconfigured_target_hashing(
    buck: Buck,
) -> None:
    await assert_hashes(buck, ":foo", "foo.txt", False)
    await assert_hashes(buck, ":foo", "bar.txt", True)
    await assert_hashes(buck, ":foo_dep", "foo.txt", False)
    await assert_hashes(buck, ":foo_dep", "bar.txt", True)
    await assert_hashes(buck, ":none", "bar.txt", True)


async def assert_hashes(
    buck: Buck, target: str, modified_path: str, same_hash: bool
) -> None:
    result = await buck.targets(
        target,
        "--show-unconfigured-target-hash",
        "--json",
        "--target-hash-file-mode",
        "PATHS_ONLY",
        "--target-hash-recursive=true",
    )

    modified_result = await buck.targets(
        target,
        "--show-unconfigured-target-hash",
        "--json",
        "--target-hash-file-mode",
        "PATHS_ONLY",
        "--target-hash-recursive=true",
        "--target-hash-modified-paths",
        modified_path,
    )
    output = json.loads(result.stdout)
    modified_output = json.loads(modified_result.stdout)

    # Hash should change if modified path belongs to target or to any of its dependencies
    if same_hash:
        assert output[0]["buck.target_hash"] == modified_output[0]["buck.target_hash"]
    else:
        assert output[0]["buck.target_hash"] != modified_output[0]["buck.target_hash"]


@buck_test()
async def test_cfg_modifiers_change_target_hash(buck: Buck) -> None:
    result = await buck.targets(
        ":foo",
        "--show-unconfigured-target-hash",
        "--target-hash-recursive=false",
        "--json",
    )

    with open(buck.cwd / "PACKAGE", "w") as package:
        package.write("set_modifiers(['aaabbbccc'])")

    modified_result = await buck.targets(
        ":foo",
        "--show-unconfigured-target-hash",
        "--target-hash-recursive=false",
        "--json",
    )
    output = json.loads(result.stdout)
    modified_output = json.loads(modified_result.stdout)

    # modifiers should change target hash
    assert output[0]["buck.target_hash"] != modified_output[0]["buck.target_hash"]


@buck_test()
async def test_visibility_intersection_change_target_hash(buck: Buck) -> None:
    result = await buck.targets(
        ":public_lib",
        "--show-unconfigured-target-hash",
        "--target-hash-recursive=false",
        "--json",
    )

    # `enforce_visibility_intersection()` restricts visibility at the PACKAGE
    # level without touching the target's `visibility` attribute (here
    # `PUBLIC`), so without hashing the intersection this change would be
    # invisible to the target hash and thus to target determination.
    # Regression for T279420508.
    with open(buck.cwd / "PACKAGE", "w") as package:
        package.write(
            'package(visibility = ["//foo/..."])\nenforce_visibility_intersection()\n'
        )

    modified_result = await buck.targets(
        ":public_lib",
        "--show-unconfigured-target-hash",
        "--target-hash-recursive=false",
        "--json",
    )
    output = json.loads(result.stdout)
    modified_output = json.loads(modified_result.stdout)

    assert output[0]["buck.target_hash"] != modified_output[0]["buck.target_hash"]


@buck_test()
async def test_parent_cfg_modifiers_change_target_hash(buck: Buck) -> None:
    result = await buck.targets(
        "foo:bar",
        "--show-unconfigured-target-hash",
        "--target-hash-recursive=false",
        "--json",
    )

    with open(buck.cwd / "PACKAGE", "w") as package:
        package.write("set_modifiers(['aaabbbccc'])")

    modified_result = await buck.targets(
        "foo:bar",
        "--show-unconfigured-target-hash",
        "--target-hash-recursive=false",
        "--json",
    )
    output = json.loads(result.stdout)
    modified_output = json.loads(modified_result.stdout)

    # parent set_modifiers value should change target hash
    # note that we merge parent modifiers and current package modifiers
    assert output[0]["buck.target_hash"] != modified_output[0]["buck.target_hash"]
