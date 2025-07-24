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
from pathlib import Path
from typing import List

from buck2.tests.e2e_util.api.buck import Buck
from buck2.tests.e2e_util.asserts import expect_failure
from buck2.tests.e2e_util.buck_workspace import buck_test
from buck2.tests.e2e_util.helper.golden import golden, GOLDEN_DIRECTORY, sanitize_hashes


# TODO(azhang2542): Factor out sanitize function
def _sanitize(s: str) -> str:
    # Simplify analysis error message (Can change due to line number changes)
    s = re.sub(
        r"Error running analysis for.*\"", 'Error running analysis for <IRRELEVANT>"', s
    )
    # Simplify the Unknown target error (Can change due to number of targets in TARGETS.fixture)
    s = re.sub(
        r"Unknown target `.*` from package .*\"",
        'Unknown target `<TARGET>` from package <IRRELEVANT>"',
        s,
    )

    return sanitize_hashes(s)


def build_report_test(name: str, command: List[str], should_fail: bool) -> None:
    async def impl(buck: Buck, tmp_path: Path) -> None:
        report = tmp_path / "build-report.json"
        if should_fail:
            await expect_failure(
                buck.test(
                    "--build-report",
                    str(report),
                    "--build-report-options",
                    "fill-out-failures",
                    *command,
                )
            )
        else:
            await buck.test("--build-report", str(report), *command)

        with open(report) as file:
            report = json.loads(file.read())
        del report["trace_id"]
        del report["project_root"]

        if should_fail and "strings" in report:
            # TODO(azhang2542): Factor out this string cache sanitization logic
            # string cache keys can vary due to differences in platform hashes within the message,
            # so do something dumb here to still be able to use golden tests on all platforms:
            #
            # 1. sort by sanitized values
            # 2. create a new dict where the keys are 1 + a large number so that we can
            #    sanitize it using the message regex above
            strings = dict(
                sorted(
                    report["strings"].items(),
                    key=lambda item: sanitize_hashes(item[1]),
                )
            )
            # Create a new dict where the keys are 1 + a large number
            updated_strings = {}
            start = 10000000000000000
            for i, v in enumerate(strings.values()):
                updated_strings[i + start] = v

            report["strings"] = updated_strings

        # Build report errors can change based on minor test changes such as
        # 1. Adding a target in TARGETS.fixture
        # 2. Line number changing due to code moving around
        # Sanitize so that we only check the important bits of the error message
        golden(
            output=_sanitize(json.dumps(report, indent=2, sort_keys=True)),
            rel_path=GOLDEN_DIRECTORY + name + ".golden.json",
        )
        pass

    globals()[name] = impl

    return buck_test()(impl)


build_report_test(
    "test_build_report_format_all_ok",
    ["//:ok?root//:macos", "//:ok?root//:linux", "//:ok?root//:macos+root//:arm"],
    False,
)

build_report_test(
    "test_build_report_format_all_fail",
    ["//:fail?root//:macos", "//:fail?root//:linux", "//:fail?root//:macos+root//:arm"],
    True,
)

build_report_test(
    "test_build_report_fail_build",
    ["//:fail_build?root//:linux", "//:fail_build?root//:macos"],
    True,
)

build_report_test(
    "test_build_report_mixed",
    ["//:ok?root//:macos", "//:fail?root//:linux", "//:fail_build?root//:arm"],
    True,
)

build_report_test("test_build_report_package", ["//:?root//:macos"], True)

build_report_test(
    "test_build_report_recursive",
    ["//...?root//:macos"],
    True,
)

build_report_test("test_build_report_ambiguous", ["//subdir?root//:macos"], False)

build_report_test(
    "test_build_report_deduplicate",
    ["//:?root//:macos", "//:ok?root//:macos", "//:ok?root//:linux"],
    True,
)

# FIXME: Both patterns should be displayed in the build report,
# however currently we are only displaying one of them non deterministically.
# build_report_test(
#     "test_build_report_modifiers_that_result_in_same_configured",
#     [
#         "//:ok?root//:linux+root//:arm",
#         "//:ok?root//:arm+root//:linux",
#     ],
#     False,
# )


def modifiers_match_test(
    name: str, command: List[str], expected_configurations: int, should_fail: bool
) -> None:
    async def impl(buck: Buck, tmp_path: Path) -> None:
        report = tmp_path / "build-report.json"
        if should_fail:
            await expect_failure(
                buck.test(
                    "--build-report",
                    str(report),
                    "--build-report-options",
                    "fill-out-failures",
                    *command,
                )
            )
        else:
            await buck.test("--build-report", str(report), *command)

        with open(report) as file:
            report = json.loads(file.read())

        num_configurations = 0
        for target_pattern, entry in report["results"].items():
            # grabs the modifiers from the pattern as a list of strings
            modifiers = target_pattern.split("?")[1].split("+")

            # for configuration id associated with the target_pattern
            # i.e. cfg:<empty>#0899b0510451fef9
            # check if the associated modifiers exist in the configuration
            for configuration_id in entry["configured"].keys():
                num_configurations += 1
                configurations = await buck.audit_configurations(configuration_id)

                for modifier in modifiers:
                    assert modifier in configurations.stdout

        assert num_configurations == expected_configurations

    globals()[name] = impl

    return buck_test()(impl)


modifiers_match_test(
    "test_single_modifier_single_pattern", ["//:ok?root//:macos"], 1, False
)

modifiers_match_test(
    "test_multiple_modifiers_single_pattern",
    ["//:ok?root//:macos+root//:arm"],
    1,
    False,
)

modifiers_match_test(
    "test_single_modifier_multiple_patterns",
    ["//:ok?root//:macos", "//:fail?root//:macos"],
    2,
    True,
)

modifiers_match_test(
    "test_multiple_modifiers_multiple_patterns",
    ["//:ok?root//:macos+root//:arm", "//:fail?root//:linux"],
    2,
    True,
)

modifiers_match_test(
    "test_recursive_modifiers",
    ["//...?root//:arm"],
    4,
    True,
)

modifiers_match_test(
    "test_package_modifiers",
    ["//:?root//:arm"],
    3,
    True,
)

modifiers_match_test(
    "test_ambiguous_modifiers",
    ["//subdir?root//:arm"],
    1,
    False,
)

modifiers_match_test(
    "test_modifiers_complex",
    ["//...?root//:macos+root//:arm", "//:?root//:linux", "//:ok?root//:linux"],
    7,
    True,
)


@buck_test()
async def test_order_of_modifiers(buck: Buck, tmp_path: Path) -> None:
    target_with_modifiers = "root//:ok?root//:linux+root//:macos"
    report = tmp_path / "build-report.json"

    await buck.test("--build-report", str(report), target_with_modifiers)

    with open(report) as file:
        report = json.loads(file.read())

    [configuration] = report["results"][target_with_modifiers]["configured"].keys()

    cfg = await buck.audit_configurations(configuration)

    assert "root//:macos" in cfg.stdout
    assert "root//:linux" not in cfg.stdout


@buck_test()
async def test_modifiers_that_end_up_with_same_configuration(
    buck: Buck, tmp_path: Path
) -> None:
    mac_first = "root//:ok?root//:macos+root//:arm"
    arm_first = "root//:ok?root//:arm+root//:macos"
    report = tmp_path / "build-report.json"

    await buck.test("--build-report", str(report), mac_first, arm_first)

    with open(report) as file:
        report = json.loads(file.read())

    # incorrect behavior, should have 2 results
    assert len(report["results"].keys()) == 1


@buck_test()
async def test_fails_with_global_modifiers(buck: Buck) -> None:
    await expect_failure(
        buck.test("--modifier", "root//:macos", "//:ok?root//:linux"),
        stderr_regex=r"Cannot specify modifiers with \?modifier syntax when global CLI modifiers are set with --modifier flag",
    )
