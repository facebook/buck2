# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

import json
import re

import pytest
from buck2.tests.e2e_util.api.buck import Buck
from buck2.tests.e2e_util.buck_workspace import buck_test
from buck2.tests.e2e_util.helper.golden import golden_replace_cfg_hash


_HASH_REGEX: re.Pattern[str] = re.compile(r"^(?:[0-9a-f]{32}|[0-9a-f]{64})$")


async def _target_hash(buck: Buck, target: str, *args: str) -> str:
    result = await buck.ctargets(
        target,
        "--show-target-hash",
        "--json",
        *args,
    )
    output = json.loads(result.stdout)
    assert len(output) == 1, output
    target_hash = output[0]["buck.target_hash"]
    assert _HASH_REGEX.fullmatch(target_hash), target_hash
    return target_hash


@pytest.mark.parametrize("output_format", ["json", "text"])
@pytest.mark.parametrize("hash_function", ["fast", "strong"])
@pytest.mark.parametrize("recursive", [False, True])
@buck_test()
async def test_hash_output_golden(
    buck: Buck, output_format: str, hash_function: str, recursive: bool
) -> None:
    args = ["--json"] if output_format == "json" else []
    if recursive:
        args.append("--target-hash-recursive")
    result = await buck.ctargets(
        "root//:parent?root//:linux",
        "--show-target-hash",
        f"--target-hash-function={hash_function}",
        *args,
    )
    golden_replace_cfg_hash(
        output=re.sub(
            r"\b(?:[0-9a-f]{32}|[0-9a-f]{64})\b", "<TARGET_HASH>", result.stdout
        ),
        rel_path=f"golden/hash.{output_format}.golden",
    )


@buck_test()
async def test_hash_ignores_configuration_but_tracks_configured_attrs(
    buck: Buck,
) -> None:
    same_result = await buck.ctargets(
        "root//:same?root//:linux",
        "root//:same?root//:macos",
        "--show-target-hash",
        "--json",
    )
    same_outputs = json.loads(same_result.stdout)
    assert len(same_outputs) == 2, same_outputs

    selected_result = await buck.ctargets(
        "root//:selected?root//:linux",
        "root//:selected?root//:macos",
        "--show-target-hash",
        "--json",
    )
    selected_outputs = json.loads(selected_result.stdout)
    assert len(selected_outputs) == 2, selected_outputs

    assert same_outputs[0]["buck.target_hash"] == same_outputs[1]["buck.target_hash"]
    assert (
        selected_outputs[0]["buck.target_hash"]
        != selected_outputs[1]["buck.target_hash"]
    )


@buck_test()
async def test_hash_is_available_in_all_output_formats(buck: Buck) -> None:
    target = "root//:same?root//:linux"

    plain_json = json.loads((await buck.ctargets(target, "--json")).stdout)
    assert "buck.target_hash" not in plain_json[0]

    text = (await buck.ctargets(target, "--show-target-hash")).stdout
    assert re.search(r" [0-9a-f]{32}\n$", text), text

    report = json.loads(
        (
            await buck.ctargets(
                target,
                "--show-target-hash",
                "--json-report",
            )
        ).stdout
    )
    assert _HASH_REGEX.fullmatch(report["compatible_targets"][0]["buck.target_hash"])


@pytest.mark.parametrize("recursive", [False, True])
@buck_test()
async def test_hash_tracks_dependency_attributes_only(
    buck: Buck, recursive: bool
) -> None:
    args = ("--target-hash-recursive",) if recursive else ()
    baseline = await _target_hash(buck, "root//:parent?root//:linux", *args)

    targets_file = buck.cwd / "TARGETS.fixture"
    targets_file.write_text(
        targets_file.read_text().replace("unrelated-before", "unrelated-after")
    )
    await buck.kill()
    unrelated_changed = await _target_hash(buck, "root//:parent?root//:linux", *args)
    assert baseline == unrelated_changed

    targets_file.write_text(targets_file.read_text().replace("dep-before", "dep-after"))
    await buck.kill()
    dependency_changed = await _target_hash(buck, "root//:parent?root//:linux", *args)
    assert (baseline != dependency_changed) == recursive


@buck_test()
async def test_hash_does_not_read_source_contents(buck: Buck) -> None:
    baseline = await _target_hash(buck, "root//:with_src?root//:linux")

    (buck.cwd / "source.txt").write_text("changed contents\n")
    await buck.kill()

    assert baseline == await _target_hash(buck, "root//:with_src?root//:linux")


@pytest.mark.parametrize("recursive", [False, True])
@buck_test()
async def test_hash_function_selects_fast_or_strong(
    buck: Buck, recursive: bool
) -> None:
    target = "root//:parent?root//:linux"
    args = ("--target-hash-recursive",) if recursive else ()
    fast = await _target_hash(buck, target, "--target-hash-function=fast", *args)
    strong = await _target_hash(buck, target, "--target-hash-function=strong", *args)

    assert len(fast) == 32
    assert len(strong) == 64
    assert fast == await _target_hash(
        buck, target, "--target-hash-function=fast", *args
    )
    assert strong == await _target_hash(
        buck, target, "--target-hash-function=strong", *args
    )


@pytest.mark.parametrize("target", ["attribute_order", "list_order", "split_order"])
@pytest.mark.parametrize("hash_function", ["fast", "strong"])
@buck_test()
async def test_hash_preserves_dependency_order(
    buck: Buck, target: str, hash_function: str
) -> None:
    label = f"root//:{target}"
    hash_arg = f"--target-hash-function={hash_function}"
    swapped_args = ("-c", "test.swap_platforms=true")
    recursive_arg = "--target-hash-recursive"

    assert await _target_hash(buck, label, hash_arg) == await _target_hash(
        buck, label, hash_arg, *swapped_args
    )
    assert await _target_hash(
        buck, label, hash_arg, recursive_arg
    ) != await _target_hash(buck, label, hash_arg, recursive_arg, *swapped_args)


@buck_test()
async def test_hash_ignores_configuration_of_equal_dependencies(buck: Buck) -> None:
    assert await _target_hash(
        buck, "root//:equal_deps", "--target-hash-recursive"
    ) == await _target_hash(
        buck,
        "root//:equal_deps",
        "--target-hash-recursive",
        "-c",
        "test.swap_platforms=true",
    )


@buck_test()
async def test_hash_tracks_swapped_dependency_contents(buck: Buck) -> None:
    target = "root//:attribute_order"
    recursive_arg = "--target-hash-recursive"
    baseline = await _target_hash(buck, target, recursive_arg)
    local_baseline = await _target_hash(buck, target)

    targets_file = buck.cwd / "TARGETS.fixture"
    targets_file.write_text(
        targets_file.read_text()
        .replace('":linux": "linux"', '":linux": "macos"')
        .replace('":macos": "macos"', '":macos": "linux"')
    )
    await buck.kill()

    assert local_baseline == await _target_hash(buck, target)
    assert baseline != await _target_hash(buck, target, recursive_arg)
