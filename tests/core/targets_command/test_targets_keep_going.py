# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

# pyre-strict


import json

from buck2.tests.e2e_util.api.buck import Buck
from buck2.tests.e2e_util.buck_workspace import buck_test


@buck_test()
async def test_keep_going_json(buck: Buck) -> None:
    result = await buck.targets("//...", "--json", "--keep-going")
    xs = json.loads(result.stdout)
    # I expect six records, one which is an error
    assert len(xs) == 6
    for x in xs:
        if x["buck.package"] == "root//a":
            assert x["name"].startswith("target")
        else:
            assert x["buck.package"] == "root//b"
            assert "test_error" in x["buck.error"]


@buck_test()
async def test_keep_going(buck: Buck) -> None:
    result = await buck.targets("//...", "--keep-going")
    assert "test_error" in result.stderr


@buck_test()
async def test_keep_going_streaming(buck: Buck) -> None:
    result = await buck.targets("//...", "--streaming", "--keep-going")
    assert "test_error" in result.stderr


@buck_test()
async def test_streaming_keep_going_missing_targets(buck: Buck) -> None:
    targets = [
        "//a:target1",
        "//a:target2",
        "//a:bogus_target",
        "//a:worse_target",
        "//a:target5",
        "//d:bogus_package",
    ]
    result = await buck.targets(*targets, "--json", "--streaming", "--keep-going")
    xs = json.loads(result.stdout)
    assert len(xs) == 5  # 3 success, 2 errors
    bad_packages = []
    good_targets = []
    for x in xs:
        if "buck.error" in x:
            bad_packages.append(x["buck.package"])
            if x["buck.package"] == "root//a":
                assert "`bogus_target`" in x["buck.error"]
                assert "`worse_target`" in x["buck.error"]
        else:
            good_targets.append(x["name"])
    bad_packages.sort()
    good_targets.sort()
    assert bad_packages == ["root//a", "root//d"]
    assert good_targets == ["target1", "target2", "target5"]


@buck_test()
async def test_streaming_keep_going_with_single_failure(buck: Buck) -> None:
    targets = [
        "//a:does_not_exist",
    ]
    result = await buck.targets(*targets, "--json", "--streaming", "--keep-going")
    try:
        # TODO(T213880451) we shouldn't emit malformed json on an error
        json.loads(result.stdout)
        raise AssertionError("Expected json to fail to parse")
    except json.decoder.JSONDecodeError:
        pass


@buck_test()
async def test_streaming_keep_going_with_single_failing_target_and_one_other_target_in_different_package(
    buck: Buck,
) -> None:
    targets = [
        "//a:target1",
        "//c:does_not_exist",
    ]
    result = await buck.targets(
        *targets,
        "-a",
        "type",
        "--streaming",
        "--keep-going",
    )

    try:
        # TODO(T213880451) we shouldn't emit malformed json on an error
        json.loads(result.stdout)
        raise AssertionError("Expected json to fail to parse")
    except json.decoder.JSONDecodeError:
        pass
