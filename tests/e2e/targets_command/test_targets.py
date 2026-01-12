# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

# pyre-strict


import gzip
import json
import re
import subprocess
import tempfile
from typing import List

import pytest
from buck2.tests.e2e_util.api.buck import Buck
from buck2.tests.e2e_util.api.buck_result import BuckResult
from buck2.tests.e2e_util.buck_workspace import buck_test

FOO_UNMODIFIED = ["4", "5", "6", "7"]
BAR_UNMODIFIED = ["1", "2", "3", "4", "5"]


# TODO: Make this test isolated, i.e. inplace=False.
@buck_test(inplace=True)
@pytest.mark.parametrize(
    "rule, unmodified, src_changed",
    [
        ("1", FOO_UNMODIFIED, "foo.txt"),
        ("2", FOO_UNMODIFIED, "foo.txt"),
        ("3", FOO_UNMODIFIED, "foo.txt"),
        ("4", FOO_UNMODIFIED, "foo.txt"),
        ("5", FOO_UNMODIFIED, "foo.txt"),
        ("6", FOO_UNMODIFIED, "foo.txt"),
        ("7", FOO_UNMODIFIED, "foo.txt"),
        ("1", BAR_UNMODIFIED, "bar.txt"),
        ("2", BAR_UNMODIFIED, "bar.txt"),
        ("3", BAR_UNMODIFIED, "bar.txt"),
        ("4", BAR_UNMODIFIED, "bar.txt"),
        ("5", BAR_UNMODIFIED, "bar.txt"),
        ("6", BAR_UNMODIFIED, "bar.txt"),
        ("7", BAR_UNMODIFIED, "bar.txt"),
    ],
)
async def test_configured_target_hashing(
    buck: Buck,
    rule: str,
    unmodified: List[str],
    src_changed: str,
) -> None:
    target = "fbcode//buck2/tests/targets/target_hashing:rule{}".format(rule)
    result = await buck.targets(
        target,
        "--show-target-hash",
        "--json",
        "--target-hash-file-mode",
        "PATHS_ONLY",
    )

    # Modify a target
    modified_result = await buck.targets(
        target,
        "--show-target-hash",
        "--json",
        "--target-hash-file-mode",
        "PATHS_ONLY",
        "--target-hash-modified-paths",
        "buck2/tests/targets/target_hashing/{}".format(src_changed),
    )
    output = json.loads(result.stdout)
    modified_output = json.loads(modified_result.stdout)

    # Hash should change iff the target is the modified target or depends on the modified target
    if rule in unmodified:
        assert output[0]["buck.target_hash"] == modified_output[0]["buck.target_hash"]
    else:
        assert output[0]["buck.target_hash"] != modified_output[0]["buck.target_hash"]


@buck_test(inplace=True)
async def test_configured_ignores_unconfigured(buck: Buck) -> None:
    target = "fbcode//buck2/tests/targets/target_hashing:rule8"
    pre_unconfigured = await buck.targets(
        target, "--show-unconfigured-target-hash", "--json"
    )
    pre_configured = await buck.targets(target, "--show-target-hash", "--json")

    config = "-ctesting.hashing=1"
    post_unconfigured = await buck.targets(
        target, config, "--show-unconfigured-target-hash", "--json"
    )
    post_configured = await buck.targets(target, config, "--show-target-hash", "--json")

    def grab(x: BuckResult) -> str:
        return json.loads(x.stdout)[0]["buck.target_hash"]

    # Hashes differ configured vs unconfigured
    assert grab(pre_unconfigured) != grab(pre_configured)
    # We spot the unconfigured change
    assert grab(pre_unconfigured) != grab(post_unconfigured)
    # But the configured remains consistent
    assert grab(pre_configured) == grab(post_configured)


@buck_test(inplace=True)
async def test_non_recursive_target_hash(buck: Buck) -> None:
    target = "fbcode//buck2/tests/targets/target_hashing:rule9"
    pre_recursive = await buck.targets(target, "--show-target-hash", "--json")
    pre_direct = await buck.targets(
        target, "--show-target-hash", "--json", "--target-hash-recursive=false"
    )

    config = "-ctesting.hashing=1"
    post_recursive = await buck.targets(target, config, "--show-target-hash", "--json")
    post_direct = await buck.targets(
        target, config, "--show-target-hash", "--json", "--target-hash-recursive=false"
    )

    def grab(x: BuckResult) -> str:
        return json.loads(x.stdout)[0]["buck.target_hash"]

    # Hashes changed for recursive
    assert grab(pre_recursive) != grab(post_recursive)
    # But not for non-recursive
    assert grab(pre_direct) == grab(post_direct)


@buck_test(inplace=True)
async def test_show_inputs(buck: Buck) -> None:
    target = "fbcode//buck2/tests/targets/target_hashing:rule1"
    result = await buck.targets(target, "--json")
    assert (
        "fbcode//buck2/tests/targets/target_hashing:rule5"
        in json.loads(result.stdout)[0]["buck.deps"]
    )
    assert json.loads(result.stdout)[0]["buck.inputs"] == [
        "fbcode//buck2/tests/targets/target_hashing/foo.txt"
    ]


@buck_test(inplace=True)
async def test_streaming_uncached(buck: Buck) -> None:
    # This test aims to check the kind of things TD might do - the streaming plus other related features
    with tempfile.NamedTemporaryFile() as file:
        await buck.targets(
            "fbcode//buck2:buck2",
            "--json-lines",
            "--streaming",
            "--imports",
            "--output-attribute",
            "^buck\\.|name",
            "--no-cache",
            "--show-unconfigured-target-hash",
            "--output=" + file.name,
        )
        found = 0
        for x in file.readlines():
            x = json.loads(x)
            if x.get("buck.package") == "fbcode//buck2":
                if x.get("name") == "buck2":
                    assert re.match("^[0-9a-f]+$", x["buck.target_hash"])
                    found += 1
                elif "buck.imports" in x:
                    assert "prelude//prelude.bzl" in x["buck.imports"]
                    found += 1
        assert found == 2


@buck_test(inplace=True)
async def test_compression(buck: Buck) -> None:
    with tempfile.TemporaryDirectory() as name:
        await buck.targets("fbcode//buck2:", "--output=" + name + "/out.txt")
        await buck.targets(
            "fbcode//buck2:", "--output=" + name + "/out.txt.gz", "--compression=gzip"
        )
        await buck.targets(
            "fbcode//buck2:", "--output=" + name + "/out.txt.zst", "--compression=zstd"
        )
        with open(name + "/out.txt", "rb") as file:
            out_uncompressed = file.read()
        with gzip.open(name + "/out.txt.gz") as file:
            out_gzip = file.read()
        subprocess.run(
            ["zstd", "-d", name + "/out.txt.zst", "-o", name + "/out.txt.unzst"],
            check=True,
        )
        with open(name + "/out.txt.unzst", "rb") as file:
            out_zstd = file.read()
    assert out_uncompressed == out_gzip
    assert out_uncompressed == out_zstd
