# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

# pyre-strict
import json

from buck2.tests.e2e_util.api.buck import Buck
from buck2.tests.e2e_util.asserts import expect_failure
from buck2.tests.e2e_util.buck_workspace import buck_test
from buck2.tests.e2e_util.helper.golden import golden, GOLDEN_DIRECTORY


# These test will be updated in the future when the build report is updated to include
# the modifier syntax.


@buck_test()
async def test_build_with_single_modifier(buck: Buck) -> None:
    result = await buck.build("root//:dummy?root//:macos")

    output = json.loads(result.stdout)

    [configuration] = output["results"]["root//:dummy"]["configured"].keys()

    cfg = await buck.audit_configurations(configuration)

    assert "root//:macos" in cfg.stdout


@buck_test()
async def test_build_with_multiple_modifiers(buck: Buck) -> None:
    result = await buck.build("root//:dummy?root//:macos+root//:arm")

    output = json.loads(result.stdout)

    [configuration] = output["results"]["root//:dummy"]["configured"].keys()

    cfg = await buck.audit_configurations(configuration)

    assert "root//:macos" in cfg.stdout
    assert "root//:arm" in cfg.stdout


@buck_test()
async def test_build_order_of_modifiers(buck: Buck) -> None:
    # if passing in modifiers of the same constraint setting,
    # the last one should be the one that applies
    result = await buck.build("root//:dummy?root//:linux+root//:macos")

    output = json.loads(result.stdout)

    [configuration] = output["results"]["root//:dummy"]["configured"].keys()

    cfg = await buck.audit_configurations(configuration)

    assert "root//:macos" in cfg.stdout
    assert "root//:linux" not in cfg.stdout


@buck_test()
async def test_build_with_different_targets_and_modifiers(buck: Buck) -> None:
    result = await buck.build("root//:dummy?root//:macos", "root//:dummy2?root//:linux")

    output = json.loads(result.stdout)

    [configuration] = output["results"]["root//:dummy"]["configured"].keys()
    cfg = await buck.audit_configurations(configuration)
    assert "root//:macos" in cfg.stdout

    [configuration] = output["results"]["root//:dummy2"]["configured"].keys()
    cfg = await buck.audit_configurations(configuration)
    assert "root//:linux" in cfg.stdout


@buck_test()
async def test_build_with_same_target_different_modifiers(buck: Buck) -> None:
    result = await buck.build("root//:dummy?root//:macos", "root//:dummy?root//:linux")

    output = json.loads(result.stdout)

    configurations = output["results"]["root//:dummy"]["configured"].keys()

    assert len(configurations) == 2

    # Need to do this as the order of the configurations is not guranteedd
    # due to random hashes. This should change once build report is updated.
    macos_found = False
    linux_found = False
    for configuration in configurations:
        cfg = await buck.audit_configurations(configuration)
        if "root//:macos" in cfg.stdout:
            macos_found = True
        if "root//:linux" in cfg.stdout:
            linux_found = True

    assert macos_found
    assert linux_found


@buck_test()
async def test_build_with_same_target_and_modifiers(buck: Buck) -> None:
    result = await buck.build("root//:dummy?root//:macos", "root//:dummy?root//:macos")

    output = json.loads(result.stdout)

    [configuration] = output["results"]["root//:dummy"]["configured"].keys()

    cfg = await buck.audit_configurations(configuration)
    assert "root//:macos" in cfg.stdout


@buck_test()
async def test_build_with_target_universe(buck: Buck) -> None:
    result = await buck.build(
        "root//:dummy",
        "--target-universe",
        "root//:universe?root//:linux",
    )

    output = json.loads(result.stdout)

    [configuration] = output["results"]["root//:dummy"]["configured"].keys()

    cfg = await buck.audit_configurations(configuration)

    assert "root//:linux" in cfg.stdout


@buck_test()
async def test_build_with_target_universe_multiple_modifiers(buck: Buck) -> None:
    result = await buck.build(
        "root//:dummy",
        "--target-universe",
        "root//:universe?root//:linux+root//:arm",
    )

    output = json.loads(result.stdout)

    [configuration] = output["results"]["root//:dummy"]["configured"].keys()

    cfg = await buck.audit_configurations(configuration)

    assert "root//:linux" in cfg.stdout
    assert "root//:arm" in cfg.stdout


@buck_test()
async def test_build_with_mutliple_target_universes(buck: Buck) -> None:
    result = await buck.build(
        "root//:dummy",
        "--target-universe",
        "root//:universe?root//:linux,root//:dummy?root//:macos+root//:arm",
    )

    output = json.loads(result.stdout)

    configurations = output["results"]["root//:dummy"]["configured"].keys()

    assert len(configurations) == 2

    # Need to do this as the order of the configurations is not guranteedd
    # due to random hashes. This should change once build report is updated.
    linux_found = False
    macos_found = False
    for configuration in configurations:
        cfg = await buck.audit_configurations(configuration)
        if "root//:linux" in cfg.stdout:
            linux_found = True
        if "root//:macos" in cfg.stdout and "root//:arm" in cfg.stdout:
            macos_found = True

    assert linux_found
    assert macos_found


@buck_test()
async def test_build_with_package_pattern(buck: Buck) -> None:
    result = await buck.build("root//:?root//:macos")

    output = json.loads(result.stdout)

    [configuration] = output["results"]["root//:dummy"]["configured"].keys()
    cfg = await buck.audit_configurations(configuration)
    assert "root//:macos" in cfg.stdout

    [configuration] = output["results"]["root//:dummy2"]["configured"].keys()
    cfg = await buck.audit_configurations(configuration)
    assert "root//:macos" in cfg.stdout


@buck_test()
async def test_build_with_recursive_pattern(buck: Buck) -> None:
    result = await buck.build("root//...?root//:macos")

    output = json.loads(result.stdout)

    [configuration] = output["results"]["root//:dummy"]["configured"].keys()
    cfg = await buck.audit_configurations(configuration)
    assert "root//:macos" in cfg.stdout

    [configuration] = output["results"]["root//:dummy2"]["configured"].keys()
    cfg = await buck.audit_configurations(configuration)
    assert "root//:macos" in cfg.stdout

    [configuration] = output["results"]["root//recursive_pattern:recursive_target"][
        "configured"
    ].keys()
    cfg = await buck.audit_configurations(configuration)
    assert "root//:macos" in cfg.stdout


@buck_test()
async def test_build_fails_with_global_modifiers(buck: Buck) -> None:
    await expect_failure(
        buck.build("--modifier", "root//:macos", "root//:dummy?root//:linux"),
        stderr_regex=r"Cannot specify modifiers with \?modifier syntax when global CLI modifiers are set with --modifier flag",
    )

    await expect_failure(
        buck.build(
            "--modifier",
            "root//:macos",
            "root//:dummy",
            "--target-universe",
            "root//:dummy?root//:linux",
        ),
        stderr_regex=r"Cannot specify modifiers with \?modifier syntax when global CLI modifiers are set with --modifier flag",
    )


@buck_test()
async def test_build_fails_with_pattern_modifier_and_target_universe_modifier(
    buck: Buck,
) -> None:
    await expect_failure(
        buck.build(
            "root//:dummy?root//:macos",
            "--target-universe",
            "root//:dummy?root//:linux",
        ),
        stderr_regex=r"Cannot use \?modifier syntax in target pattern expression with --target-universe flag",
    )


async def run_all_output_flags(buck: Buck, *argv: str) -> str:
    flags = [
        "--show-output",
        "--show-full-output",
        "--show-simple-output",
        "--show-full-simple-output",
        "--show-json-output",
        "--show-full-json-output",
    ]

    results = []
    for flag in flags:
        result = await buck.build_without_report(flag, *argv)
        results.append(f"{flag}\n{result.stdout}")

    output = "\n\n".join(results)
    output = output.replace("\\\\", "\\")  # Windows path separators in json
    output = output.replace(str(buck.cwd), "/abs/project/root")
    output = output.replace("\\", "/")  # Windows path separators not in json

    return output


@buck_test()
async def test_build_modifiers_output_single_modifier(buck: Buck) -> None:
    result = await run_all_output_flags(
        buck,
        "root//:dummy?root//:macos",
    )

    golden(
        output=result,
        rel_path=GOLDEN_DIRECTORY
        + "test_build_modifiers_output_single_modifier.golden.txt",
    )


@buck_test()
async def test_build_modifiers_output_multiple_modifiers(buck: Buck) -> None:
    result = await run_all_output_flags(
        buck,
        "root//:dummy?root//:macos+root//:arm",
    )

    golden(
        output=result,
        rel_path=GOLDEN_DIRECTORY
        + "test_build_modifiers_output_multiple_modifiers.golden.txt",
    )


@buck_test()
async def test_build_modifiers_output_multiple_patterns(
    buck: Buck,
) -> None:
    result = await run_all_output_flags(
        buck, "root//:dummy?root//:macos", "root//:dummy?root//:linux"
    )

    golden(
        output=result,
        rel_path=GOLDEN_DIRECTORY
        + "test_build_modifiers_output_multiple_patterns.golden.txt",
    )


@buck_test()
async def test_build_modifiers_output_multiple_modifiers_multiple_patterns(
    buck: Buck,
) -> None:
    result = await run_all_output_flags(
        buck,
        "root//:dummy?root//:macos+root//:arm",
        "root//:dummy?root//:linux",
    )

    golden(
        output=result,
        rel_path=GOLDEN_DIRECTORY
        + "test_build_modifiers_output_multiple_modifiers_multiple_patterns.golden.txt",
    )


@buck_test()
async def test_build_modifiers_output_duplicate_patterns(
    buck: Buck,
) -> None:
    # Note: switching the order of the modifiers will make it so that both patterns are still in the output
    result = await run_all_output_flags(
        buck,
        "root//:dummy?root//:macos+root//:arm",
        "root//:dummy?root//:macos+root//:arm",
    )

    golden(
        output=result,
        rel_path=GOLDEN_DIRECTORY
        + "test_build_modifiers_output_duplicate_patterns.golden.txt",
    )


@buck_test()
async def test_build_modifiers_output_with_target_universe(
    buck: Buck,
) -> None:
    # Modifiers defined in target universe should not be included in the output
    result = await run_all_output_flags(
        buck,
        "root//:dummy",
        "--target-universe",
        "root//:dummy?root//:macos+root//:linux",
    )

    golden(
        output=result,
        rel_path=GOLDEN_DIRECTORY
        + "test_build_modifiers_output_with_target_universe.golden.txt",
    )
