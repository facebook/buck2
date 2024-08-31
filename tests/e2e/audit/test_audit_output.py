# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

# pyre-strict


import json
import platform

from buck2.tests.e2e_util.api.buck import Buck
from buck2.tests.e2e_util.asserts import expect_failure
from buck2.tests.e2e_util.buck_workspace import buck_test

TEST_PATH = "buck2/tests/e2e/audit/test_audit_output_data"
TEST_CELL_PATH = f"fbcode//{TEST_PATH}"


@buck_test(inplace=True)
async def test_audit_output_malformed_path(buck: Buck) -> None:
    await expect_failure(
        buck.audit_output(
            "blah",
        ),
        stderr_regex="Malformed buck-out path",
    )


@buck_test(inplace=True)
async def test_audit_output_scratch_path_unsupported(buck: Buck) -> None:
    # pick a random target, we just want the config hash
    target = f"{TEST_CELL_PATH}:dummy"
    config_hash = await _get_config_hash(buck, target)
    await expect_failure(
        buck.audit_output(
            f"buck-out/v2/tmp/fbcode/{config_hash}/path/to/target/__target__/output",
        ),
        stderr_regex="not supported for audit output",
    )


@buck_test(inplace=True)
async def test_audit_output_bxl_unsupported(buck: Buck) -> None:
    # pick a random target, we just want the config hash
    target = f"{TEST_CELL_PATH}:dummy"
    config_hash = await _get_config_hash(buck, target)
    await expect_failure(
        buck.audit_output(
            f"buck-out/v2/gen-bxl/fbcode/{config_hash}/path/to/function.bxl/__function__/output",
        ),
        stderr_regex="not supported for audit output",
    )


@buck_test(inplace=True)
async def test_audit_output_anon_targets_unsupported(buck: Buck) -> None:
    # pick a random target, we just want the config hash
    target = f"{TEST_CELL_PATH}:dummy"
    config_hash = await _get_config_hash(buck, target)
    await expect_failure(
        buck.audit_output(
            f"buck-out/v2/gen-anon/fbcode/{config_hash}/path/to/target/rule_hash/__target__/output",
        ),
        stderr_regex="not supported for audit output",
    )


@buck_test(inplace=True)
async def test_audit_output_invalid_prefix(buck: Buck) -> None:
    # invalid prefix (i.e. not gen, gen-anon, gen-bxl, temp, or test)
    # pick a random target, we just want the config hash
    target = f"{TEST_CELL_PATH}:dummy"
    config_hash = await _get_config_hash(buck, target)
    await expect_failure(
        buck.audit_output(
            f"buck-out/v2/not_gen/fbcode/{config_hash}/path/to/target/rule_hash/__target__/output",
        ),
        stderr_regex="Malformed buck-out path",
    )


@buck_test(inplace=True)
async def test_audit_output_nonexistent_cell(buck: Buck) -> None:
    # pick a random target, we just want the config hash
    target = f"{TEST_CELL_PATH}:dummy"
    config_hash = await _get_config_hash(buck, target)
    await expect_failure(
        buck.audit_output(
            f"buck-out/v2/gen/made_up_cell/{config_hash}/path/to/target/rule_hash/__target__/output",
        ),
        stderr_regex="unknown cell name",
    )


@buck_test(inplace=True, skip_for_os=["windows"])
async def test_audit_output_fbsource(buck: Buck) -> None:
    # TODO(@wendyy) - should have separate test data for this rather than reusing an existing target
    # Test fbsource, a special case because fbsource is both a project root for fbcode and a cell
    target = "fbsource//fbobjc/buck2/samples/hello_world:HelloWorldBundle"
    config_hash = await _get_config_hash(buck, target)
    result = await buck.audit_output(
        f"buck-out/v2/gen/fbsource/{config_hash}/fbobjc/buck2/samples/hello_world/__HelloWorldBundle__/HelloWorldBundle.app",
        "--output-all-attributes",
    )

    action = json.loads(result.stdout)
    assert len(action.keys()) == 1
    action_key = list(action.keys())[0]
    assert target in action_key


@buck_test(inplace=True, skip_for_os=["windows", "darwin"])
async def test_audit_output(buck: Buck) -> None:
    target_platform = "ovr_config//platform/linux:x86_64-fbcode"
    target_platforms_arg = f"--target-platforms={target_platform}"

    target = "fbcode//buck2/tests/targets/rules/cxx:my_cpp1"
    config_hash = await _get_config_hash(buck, target, target_platforms_arg)
    result = await buck.audit_output(
        f"buck-out/v2/gen/fbcode/{config_hash}/buck2/tests/targets/rules/cxx/__my_cpp1__/my_cpp1",
        target_platforms_arg,
    )

    action = result.stdout
    assert target in action
    assert target_platform in action
    assert "id" in action

    target = "fbcode//buck2/tests/targets/rules/ocaml:native"
    config_hash = await _get_config_hash(buck, target, target_platforms_arg)
    # Test dynamic output
    result = await buck.audit_output(
        f"buck-out/v2/gen/fbcode/{config_hash}/buck2/tests/targets/rules/ocaml/__native__/cmxs_order_native.lst",
        target_platforms_arg,
    )

    action = result.stdout
    assert target in action
    assert target_platform in action
    assert "id" in action

    # Test wrong config hash, should return the unconfigured target label
    result = await buck.audit_output(
        "buck-out/v2/gen/fbcode/wrong_config_hash/buck2/tests/targets/rules/cxx/__my_cpp1__/my_cpp1",
        target_platforms_arg,
    )

    output = result.stdout
    # unconfigure target label should be in the output
    assert "fbcode//buck2/tests/targets/rules/cxx:my_cpp1" in output
    # configuration platform should not be in the output
    assert target_platform not in output
    assert "Platform configuration" in output
    assert "did not match" in output

    # Test a rule that outputs to a directory
    target = "fbcode//buck2/tests/targets/rules/genrule/hello_world:my_genrule_output_to_directory"
    config_hash = await _get_config_hash(buck, target, target_platforms_arg)
    result = await buck.audit_output(
        f"buck-out/v2/gen/fbcode/{config_hash}/buck2/tests/targets/rules/genrule/hello_world/__my_genrule_output_to_directory__/out/my_directory/out.txt",
        target_platforms_arg,
    )

    action = result.stdout
    assert target in action
    assert target_platform in action
    assert "id" in action


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
