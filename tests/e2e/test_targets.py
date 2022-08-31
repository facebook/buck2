import json

import pytest

from xplat.build_infra.buck_e2e.api.buck import Buck
from xplat.build_infra.buck_e2e.buck_workspace import buck_test

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
    buck: Buck, rule: str, unmodified: list, src_changed: str
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
        assert output[0]["$target_hash"] == modified_output[0]["$target_hash"]
    else:
        assert output[0]["$target_hash"] != modified_output[0]["$target_hash"]


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
async def test_unconfigured_target_hashing(
    buck: Buck, rule: str, unmodified: list, src_changed: str
) -> None:

    target = "fbcode//buck2/tests/targets/target_hashing:rule{}".format(rule)
    result = await buck.targets(
        target,
        "--show-unconfigured-target-hash",
        "--json",
        "--target-hash-file-mode",
        "PATHS_ONLY",
    )

    # Modify a target
    modified_result = await buck.targets(
        target,
        "--show-unconfigured-target-hash",
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
        assert output[0]["$target_hash"] == modified_output[0]["$target_hash"]
    else:
        assert output[0]["$target_hash"] != modified_output[0]["$target_hash"]
