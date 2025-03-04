# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

load("@fbcode//buck2/tests:buck_e2e.bzl", "buck2_e2e_test")

# This is meant to be Open-source friendly. In our e2e tests, we invoke a variant from
# tools/build_defs/check_dependencies_test.bzl that passes additional arguments for meta specific allowlist.

def _check_dependencies_test(
        name,
        target,
        contacts,
        env,
        labels: list[str],
        deps,
        **kwargs):
    buck2_e2e_test(
        contacts = contacts,
        name = name,
        srcs = {"fbcode//buck2/tests/e2e_util:test_bxl_check_dependencies_template.py": "test_bxl_check_dependencies_template.py"},
        env = env,
        labels = labels,
        heavyweight_label = "heavyweight8_experimental",
        test_with_compiled_buck2 = False,
        test_with_deployed_buck2 = True,
        use_buck_api = False,
        # In order for target determinator to trigger this test when the `target` specified has changed, we need to introduce a dep on `target`.
        # However, we cannot introduce a configured dep, because the `target` may not be compatible with platform of dependencies test.
        # This adds a dep on `target` in a select arm that is never satisfied. This will work for TD because TD only looks at deps on unconfigured
        # target graph.
        deps = (deps or []) + select({
            "DEFAULT": [],
            "ovr_config//:none": [target],
        }),
        **kwargs
    )

def check_dependencies_test(
        name,
        target,
        contacts,
        mode,
        allowlist_patterns = None,
        blocklist_patterns = None,
        expect_failure_msg = None,
        env = None,
        deps = None,
        **kwargs):
    """
    Creates a test target from a buck2 bxl script. BXL script must use "test" as entry
    point.


    There are two modes: "allowlist" mode, "blocklist" mode

    "allowlist" mode:

        Only deps matching patterns in "allowlist_patterns' will pass the check.
        If at least one target in a transitive closure of all dependencies doesn't
        match anything in "allowlist_patterns" the test will fail.

        In this mode "blocklist_patterns" is used to allow for additional restriction
        to be applied. If a specific target from transitive closure of all dependencies
        matches at least one pattern from "blocklist_patterns", the test will fail
        regardless if that target matches anything in "allowlist_patterns".

        In other words, for test to pass each target must match at least one
        pattern from "allowlist_patterns", and must not match anything from the
        "blocklist_patterns".


    "blocklist" mode"

        If at least one target from a transitive closure of dependnecies matches
        at least one pattern from "blocklist_patterns" the test will fail.


        In this mode "allowlist_patterns" modifies how "blocklist_patterns" are
        applied. Specifically if a specific target matches "allowlist_patterns"
        then it will always pass, and will not be checked against "blocklist_patterns".

        The use case is to allow blocking more generic location (for example,
        blocklist; //testing/.*), while still allowing specific exceptions
        (for example, allowlist: //testing/jest/.*).
    """

    bxl_main = "fbcode//buck2/tests/check_dependencies_test.bxl:test"
    allowlist_patterns = ",".join(allowlist_patterns) if allowlist_patterns else ""
    blocklist_patterns = ",".join(blocklist_patterns) if blocklist_patterns else ""
    if not (expect_failure_msg == None or len(expect_failure_msg) > 0):
        fail("Expected failure message can only be None or non-empty string")

    if mode not in ("allowlist", "blocklist"):
        fail("mode must be one of: allowlist, blocklist")

    _check_dependencies_test(
        contacts = contacts,
        name = name,
        target = target,
        env = {
            "ALLOWLIST": allowlist_patterns,
            "BLOCKLIST": blocklist_patterns,
            "BXL_MAIN": bxl_main,
            "EXPECT_FAILURE_MSG": expect_failure_msg or "",
            "FLAVOR": "check_dependencies_test",
            "TARGET": target,
            "VERIFICATION_MODE": mode,
        } | (env or {}),
        labels = ["check_dependencies_test"],
        deps = deps,
        **kwargs
    )

def assert_dependencies_test(
        name,
        target,
        contacts,
        expected_deps,
        expect_failure_msg = None,
        deps = None,
        **kwargs):
    """
    Creates a test target fromfbcode//buck2/tests/assert_dependencies_test.bxl:test bxl script.

    Parameters:
        name: Name of the test target.
        contacts: List of oncalls for the test.
        target: The target to check dependencies for
        expected_deps: list of expected deps
    """
    _check_dependencies_test(
        name = name,
        target = target,
        contacts = contacts,
        env = {
            "BXL_MAIN": "fbcode//buck2/tests/assert_dependencies_test.bxl:test",
            "DEPS": ",".join(expected_deps),
            "EXPECT_FAILURE_MSG": expect_failure_msg or "",
            "FLAVOR": "assert_dependencies_test",
            "TARGET": target,
        },
        labels = ["assert_dependencies_test"],
        deps = deps,
        **kwargs
    )

def audit_dependents_test(
        name,
        target,
        contacts,
        source_target,
        allowlist_patterns,
        expect_failure_msg = None,
        deps = None,
        **kwargs):
    """
    Creates a test target from a buck2 bxl script. BXL script must use "test" as entry
    point.

    Parameters:
        name: Name of the test target.
        contacts: List of oncalls for the test.
        target: The target to check direct dependents for
        source_target: The target universe
        allowlist_patter: a regex of patterns that should be allowed for direct dependents of target
        expect_failure_msg: the test is expected to fail with this message regex
    """
    _check_dependencies_test(
        name = name,
        target = target,
        contacts = contacts,
        env = {
            "ALLOWLIST": ",".join(allowlist_patterns) if allowlist_patterns else "",
            "BXL_MAIN": "fbcode//buck2/tests/audit_dependents_test.bxl:test",
            "EXPECT_FAILURE_MSG": expect_failure_msg or "",
            "FLAVOR": "audit_dependents_test",
            "SOURCE_TARGET": source_target,
            "TARGET": target,
        },
        labels = ["audit_dependents_test"],
        deps = deps,
        **kwargs
    )
