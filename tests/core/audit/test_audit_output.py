# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

# pyre-strict


import json
import platform

from buck2.tests.e2e_util.api.buck import Buck
from buck2.tests.e2e_util.asserts import expect_failure
from buck2.tests.e2e_util.buck_workspace import buck_test


DUMMY_CONTENT_HASH = "aaaabbbbccccdddd"


@buck_test()
async def test_audit_output_malformed_path(buck: Buck) -> None:
    await expect_failure(
        buck.audit_output(
            "blah",
        ),
        stderr_regex="Path does not start with buck-out",
    )


@buck_test()
async def test_audit_output_scratch_path_unsupported(buck: Buck) -> None:
    # pick a random target, we just want the config hash
    config_hash = await _get_config_hash(buck, "root//:dummy")
    await expect_failure(
        buck.audit_output(
            f"buck-out/v2/tmp/cell1/{config_hash}/path/to/target/__target__/output",
        ),
        stderr_regex="not supported for audit output",
    )


@buck_test()
async def test_audit_output_bxl_unsupported(buck: Buck) -> None:
    # pick a random target, we just want the config hash
    config_hash = await _get_config_hash(buck, "root//:dummy")
    await expect_failure(
        buck.audit_output(
            f"buck-out/v2/art-bxl/cell1/{config_hash}/path/to/function.bxl/__function__/output",
        ),
        stderr_regex="not supported for audit output",
    )


@buck_test()
async def test_audit_output_anon_targets_unsupported(buck: Buck) -> None:
    # pick a random target, we just want the config hash
    config_hash = await _get_config_hash(buck, "root//:dummy")
    await expect_failure(
        buck.audit_output(
            f"buck-out/v2/art-anon/cell1/{config_hash}/path/to/target/rule_hash/__target__/output",
        ),
        stderr_regex="not supported for audit output",
    )


@buck_test()
async def test_audit_output_invalid_prefix(buck: Buck) -> None:
    # invalid prefix (i.e. not art, art-anon, art-bxl, temp, or test)
    # pick a random target, we just want the config hash
    config_hash = await _get_config_hash(buck, "root//:dummy")
    await expect_failure(
        buck.audit_output(
            f"buck-out/v2/not_art/cell1/{config_hash}/path/to/target/rule_hash/__target__/output",
        ),
        stderr_regex="Malformed buck-out path",
    )


@buck_test()
async def test_audit_output_nonexistent_cell(buck: Buck) -> None:
    # pick a random target, we just want the config hash
    config_hash = await _get_config_hash(buck, "root//:dummy")
    await expect_failure(
        buck.audit_output(
            f"buck-out/v2/art/made_up_cell/{config_hash}/path/to/target/rule_hash/__target__/output",
        ),
        stderr_regex="unknown cell name",
    )


@buck_test()
async def test_audit_output_in_root_directory(buck: Buck) -> None:
    target = "root//:dummy"
    config_hash = await _get_config_hash(buck, target)
    result = await buck.audit_output(
        f"buck-out/v2/art/root/{config_hash}/__dummy__/foo.txt",
        "--output-all-attributes",
    )

    action = json.loads(result.stdout)
    assert len(action.keys()) == 1
    action_key = list(action.keys())[0]
    assert target in action_key


@buck_test()
async def test_audit_content_based_output_in_root_directory(buck: Buck) -> None:
    target = "root//:dummy"
    result = await buck.audit_output(
        f"buck-out/v2/art/root/__dummy__/{DUMMY_CONTENT_HASH}/foo.txt",
        "-c",
        "test.has_content_based_path=true",
    )

    assert result.stdout.strip() == target


@buck_test()
async def test_non_root_cell(buck: Buck) -> None:
    target = "cell1//:dummy2"
    config_hash = await _get_config_hash(buck, target)
    result = await buck.audit_output(
        f"buck-out/v2/art/cell1/{config_hash}/__dummy2__/foo.txt",
        "--output-all-attributes",
    )

    action = json.loads(result.stdout)
    assert len(action.keys()) == 1
    action_key = list(action.keys())[0]
    assert target in action_key


@buck_test()
async def test_fixed_target_platform(buck: Buck) -> None:
    target_platform = "root//:linux_platform"
    target_platforms_arg = f"--target-platforms={target_platform}"

    target = "root//directory:dummy"
    config_hash = await _get_config_hash(buck, target, target_platforms_arg)
    result = await buck.audit_output(
        f"buck-out/v2/art/root/{config_hash}/directory/__dummy__/foo.txt",
        target_platforms_arg,
    )

    action = result.stdout
    assert target in action
    assert target_platform in action
    assert "id" in action


@buck_test()
async def test_dynamic_output_declared_in_rule_bound_in_dynamic(buck: Buck) -> None:
    target = "root//dynamic_output:dynamic_output"
    config_hash = await _get_config_hash(buck, target)

    result = await buck.audit_output(
        f"buck-out/v2/art/root/{config_hash}/dynamic_output/__dynamic_output__/bound_dynamic.txt",
    )
    action = result.stdout
    assert target in action
    assert "id" in action


@buck_test()
async def test_content_based_dynamic_output_declared_in_rule_bound_in_dynamic(
    buck: Buck,
) -> None:
    target = "root//dynamic_output:dynamic_output"

    result = await buck.audit_output(
        f"buck-out/v2/art/root/dynamic_output/__dynamic_output__/{DUMMY_CONTENT_HASH}/bound_dynamic.txt",
        "-c",
        "test.has_content_based_path=true",
    )
    assert result.stdout.strip() == target


@buck_test()
async def test_dynamic_output_declared_and_bound_in_dynamic(buck: Buck) -> None:
    target = "root//dynamic_output:dynamic_output"
    config_hash = await _get_config_hash(buck, target)
    result = await buck.audit_output(
        f"buck-out/v2/art/root/{config_hash}/dynamic_output/__dynamic_output__/defined_dynamic.txt",
    )
    # FIXME(JakobDegen): Why isn't this an error?
    assert "Failed to find an action that produced the output path" in result.stdout


@buck_test()
async def test_wrong_config_hash(buck: Buck) -> None:
    # Should return the unconfigured target label
    target_platform = "root//:linux_platform"
    target_platforms_arg = f"--target-platforms={target_platform}"
    result = await buck.audit_output(
        "buck-out/v2/art/root/aaaabbbbccccdddd/directory/__dummy__/foo.txt",
        target_platforms_arg,
    )

    output = result.stdout
    # unconfigure target label should be in the output
    assert "root//directory:dummy" in output
    # configuration platform should not be in the output
    assert target_platform not in output
    assert "Platform configuration" in output
    assert "did not match" in output


@buck_test()
async def test_output_directory(buck: Buck) -> None:
    # Test a rule that outputs to a directory
    target = "root//directory:empty_dir"
    config_hash = await _get_config_hash(buck, target)
    result = await buck.audit_output(
        f"buck-out/v2/art/root/{config_hash}/directory/__empty_dir__/outputdir",
    )

    action = result.stdout
    assert target in action
    assert "id" in action


@buck_test()
async def test_content_based_output_directory(buck: Buck) -> None:
    # Test a rule that outputs to a directory
    target = "root//directory:empty_dir"
    result = await buck.audit_output(
        f"buck-out/v2/art/root/directory/__empty_dir__/{DUMMY_CONTENT_HASH}/outputdir",
        "-c",
        "test.has_content_based_path=true",
    )

    assert result.stdout.strip() == target


# TODO(@wendyy) - remove this config hash hack
# Config hash might change, so let's build a target with linux target platform
# and get the current config hash, which is the 4th index in the buck-out path.
async def _get_config_hash(buck: Buck, target: str, *args: str) -> str:
    result = await buck.build(target, *args)
    delim = "/"
    if platform.system() == "Windows":
        delim = "\\"
    config_hash = str(
        result.get_build_report().output_for_target(target, rel_path=True)
    ).split(delim)[4]

    return config_hash
