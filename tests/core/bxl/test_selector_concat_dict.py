# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.


import json

from buck2.tests.e2e_util.api.buck import Buck
from buck2.tests.e2e_util.buck_workspace import buck_test


@buck_test()
async def test_not_selector_attr(buck: Buck) -> None:
    await buck.bxl(
        "//:selector_concat_dict.bxl:not_selector_attr",
    )


@buck_test()
async def test_selector_dict_attr(buck: Buck) -> None:
    await buck.bxl(
        "//:selector_concat_dict.bxl:selector_dict_attr",
    )


@buck_test()
async def test_selector_concat_attr(buck: Buck) -> None:
    await buck.bxl(
        "//:selector_concat_dict.bxl:selector_concat_attr",
    )


@buck_test()
async def test_selector_dict_write_json(buck: Buck) -> None:
    res = await buck.bxl(
        "//:selector_concat_dict.bxl:selector_dict_write_json",
    )
    file_path = res.stdout.strip()
    with open(file_path, "r") as f:
        content = f.read()
    expected_content = {
        "__type": "selector",
        "entries": {
            "DEFAULT": ["--foo", "--bar"],
            "root//constraints:macos": ["--foo-macos", "--bar-macos"],
            "root//constraints:x86": ["--foo-x86", "--bar-x86"],
        },
    }
    assert json.loads(content) == expected_content


@buck_test()
async def test_selector_concat_write_json(buck: Buck) -> None:
    res = await buck.bxl(
        "//:selector_concat_dict.bxl:selector_concat_write_json",
    )
    file_path = res.stdout.strip()
    with open(file_path, "r") as f:
        content = f.read()
    expected_content = {
        "__type": "concat",
        "items": [
            ["--flag", "--baz"],
            {
                "__type": "selector",
                "entries": {
                    "DEFAULT": ["--foo", "--bar"],
                    "root//constraints:macos": ["--foo-macos", "--bar-macos"],
                    "root//constraints:x86": ["--foo-x86", "--bar-x86"],
                },
            },
        ],
    }
    assert json.loads(content) == expected_content
