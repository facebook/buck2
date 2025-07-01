# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

# pyre-strict


import json
import re

from buck2.tests.e2e_util.api.buck import Buck
from buck2.tests.e2e_util.buck_workspace import buck_test


def _replace_hash(s: str) -> str:
    return re.sub(r"\b[0-9a-f]{16}\b", "<HASH>", s)


@buck_test()
async def test_ctargets_basic(buck: Buck) -> None:
    result = await buck.ctargets(
        "root//:gum",
        "--target-platforms=root//:p",
    )
    [line] = result.stdout.splitlines()
    line = _replace_hash(line)
    assert line == "root//:gum (root//:p#<HASH>)"


@buck_test()
async def test_ctargets_json(buck: Buck) -> None:
    result = await buck.ctargets(
        "root//:chocolate",
        "--json",
    )

    [output] = json.loads(result.stdout)

    output["buck.type"]
    output["buck.deps"]
    output["buck.inputs"]
    output["buck.package"]
    output["name"]
    assert (
        _replace_hash(output["default_target_platform"]) == "root//:p (root//:p#<HASH>)"
    )
    output["visibility"]
    output["within_view"]


@buck_test()
async def test_ctargets_multi_json(buck: Buck) -> None:
    result = await buck.ctargets(
        "root//:",
        "--json",
    )

    outputs = json.loads(result.stdout)

    assert len(outputs) == 3

    for output in outputs:
        output["buck.type"]
        output["buck.deps"]
        output["buck.inputs"]
        output["buck.package"]

        name = output["name"]
        if name == "chocolate":
            assert (
                _replace_hash(output["default_target_platform"])
                == "root//:p (root//:p#<HASH>)"
            )

        output["visibility"]
        output["within_view"]


@buck_test()
async def test_ctargets_output_attribute(buck: Buck) -> None:
    result = await buck.ctargets(
        "root//:chocolate", "--output-attribute=default_*", "--output-attribute=name"
    )

    [output] = json.loads(_replace_hash(result.stdout))

    assert {
        "name": "chocolate",
        "default_target_platform": "root//:p (root//:p#<HASH>)",
    } == output
