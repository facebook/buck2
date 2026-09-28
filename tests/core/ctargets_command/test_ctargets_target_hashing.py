# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

from __future__ import annotations

import json
import re

import pytest
from buck2.tests.e2e_util.api.buck import Buck
from buck2.tests.e2e_util.asserts import expect_failure
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


@pytest.mark.parametrize("output_format", ["text", "json", "json-report"])
@buck_test()
async def test_hash_deduplicates_targets_after_transition(
    buck: Buck, output_format: str
) -> None:
    args = [] if output_format == "text" else [f"--{output_format}"]
    result = await buck.ctargets(
        "root//:transitioned?root//:linux",
        "root//:transitioned?root//:macos",
        "--show-target-hash",
        *args,
    )
    if output_format == "text":
        [line] = result.stdout.splitlines()
        assert line.startswith("root//:transitioned (")
        assert _HASH_REGEX.fullmatch(line.rsplit(" ", 1)[1])
        return

    targets = json.loads(result.stdout)
    if output_format == "json-report":
        targets = targets["compatible_targets"]
    [target] = targets
    assert target["buck.target"] == "root//:transitioned"
    assert target["buck.type"] == "root//defs.bzl:transitioned"
    assert _HASH_REGEX.fullmatch(target["buck.target_hash"])


@pytest.mark.parametrize("hash_function", ["fast", "strong"])
@pytest.mark.parametrize("recursive", [False, True])
@buck_test()
async def test_hash_ignores_forward_dependency_nodes(
    buck: Buck, hash_function: str, recursive: bool
) -> None:
    target = "root//:transition_parent"
    with_forward_dep = f"{target}?root//:linux"
    args = [f"--target-hash-function={hash_function}"]
    if recursive:
        args.append("--target-hash-recursive")

    baseline = await _target_hash(buck, target, *args)
    assert baseline == await _target_hash(buck, with_forward_dep, *args)

    changed_args = (*args, "-c", "test.transitioned_value=after")
    changed = await _target_hash(buck, target, *changed_args)
    assert (baseline != changed) == recursive
    assert changed == await _target_hash(buck, with_forward_dep, *changed_args)

    forced_args = (*args, "--require-hash-change-deps", "root//:transitioned")
    forced = await _target_hash(buck, target, *forced_args)
    assert baseline != forced
    assert forced == await _target_hash(buck, with_forward_dep, *forced_args)


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
    assert baseline != await _target_hash(
        buck,
        "root//:with_src?root//:linux",
        "--require-hash-change-deps",
        "root//:with_src",
    )


@pytest.mark.parametrize("hash_function", ["fast", "strong"])
@pytest.mark.parametrize("recursive", [False, True])
@buck_test()
async def test_require_hash_change_deps(
    buck: Buck, hash_function: str, recursive: bool
) -> None:
    targets = [
        "dep",
        "parent",
        "grandparent",
        "great_grandparent",
        "with_src",
        "unrelated",
    ]
    args = [f"--target-hash-function={hash_function}", "--show-target-hash", "--json"]
    if recursive:
        args.append("--target-hash-recursive")
    args.extend(f"root//:{target}?root//:linux" for target in targets)
    baseline = json.loads((await buck.ctargets(*args)).stdout)
    changed = json.loads(
        (
            await buck.ctargets(
                *args, "--require-hash-change-deps", "root//:dep", ":with_src"
            )
        ).stdout
    )
    expected = {"root//:dep", "root//:parent", "root//:with_src"}
    if recursive:
        expected.update({"root//:grandparent", "root//:great_grandparent"})
    baseline_hashes = {
        node["buck.target"]: node["buck.target_hash"] for node in baseline
    }
    changed_hashes = {node["buck.target"]: node["buck.target_hash"] for node in changed}
    assert (
        baseline_hashes.keys()
        == changed_hashes.keys()
        == {f"root//:{target}" for target in targets}
    )
    assert {
        target
        for target in baseline_hashes
        if baseline_hashes[target] != changed_hashes[target]
    } == expected


@pytest.mark.parametrize("hash_function", ["fast", "strong"])
@pytest.mark.parametrize("recursive", [False, True])
@buck_test()
async def test_require_hash_change_deps_ignores_order_and_unrelated_targets(
    buck: Buck, hash_function: str, recursive: bool
) -> None:
    target = "root//:parent?root//:linux"
    args = [f"--target-hash-function={hash_function}"]
    if recursive:
        args.append("--target-hash-recursive")
    baseline = await _target_hash(buck, target, *args)
    args.append("--require-hash-change-deps")
    assert baseline == await _target_hash(buck, target, *args, "root//:unrelated")
    changed = await _target_hash(buck, target, *args, "root//:dep", "root//:unrelated")
    assert baseline != changed
    assert changed == await _target_hash(
        buck,
        target,
        *args,
        "root//:unrelated",
        ":dep",
        "--require-hash-change-deps",
        "root//:dep",
    )


@pytest.mark.parametrize("recursive", [False, True])
@buck_test()
async def test_require_hash_change_deps_matches_all_configurations(
    buck: Buck, recursive: bool
) -> None:
    args = ["--target-hash-recursive"] if recursive else []
    baseline = await _target_hash(buck, "root//:same?root//:linux", *args)
    args.extend(["--require-hash-change-deps", "root//:same"])
    changed = await _target_hash(buck, "root//:same?root//:linux", *args)
    assert baseline != changed
    assert changed == await _target_hash(buck, "root//:same?root//:macos", *args)


@buck_test()
async def test_require_hash_change_deps_requires_hash_output(buck: Buck) -> None:
    await expect_failure(
        buck.ctargets("root//:same", "--require-hash-change-deps", "root//:same"),
        stderr_regex=r"required arguments were not provided:[\s\S]*--show-target-hash",
    )


@buck_test()
async def test_require_hash_change_deps_requires_target_labels(buck: Buck) -> None:
    await expect_failure(
        buck.ctargets(
            "root//:same",
            "--show-target-hash",
            "--require-hash-change-deps",
            "root//...",
        ),
        stderr_regex="Required a target literal, but got a non-literal pattern",
    )


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
