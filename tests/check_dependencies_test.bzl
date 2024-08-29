# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

load("@fbcode//buck2/tests:buck_e2e.bzl", "buck2_e2e_test")

# This is meant to be Open-source friendly. In our e2e tests, we invoke a variant from
# tools/build_defs/check_dependencies_test.bzl that passes additional arguments for meta specific allowlist.

def check_dependencies_test(
        name,
        target,
        contacts,
        allowlist_patterns = None,
        blocklist_patterns = None,
        expect_failure_msg = None,
        env = None,
        deps = None,
        **kwargs):
    """
    Creates a test target from a buck2 bxl script. BXL script must use "test" as entry
    point.

    Parameters:
        name: Name of the test target.
        contacts: List of oncalls for the test.
        target: The target to check dependencies for
        allowlist_patterns: a regex of patterns that should be allowed in transitive deps of the target
        blocklist_patterns: a regex of patterns that should be blocked in transitive deps of the target
        expect_failure_msg: the test is expected to fail with this message regex
        env: additional environment variables to pass to the checking script
    """
    bxl_main = "fbcode//buck2/tests/check_dependencies_test.bxl:test"
    allowlist_patterns = ",".join(allowlist_patterns) if allowlist_patterns else ""
    blocklist_patterns = ",".join(blocklist_patterns) if blocklist_patterns else ""
    if not (expect_failure_msg == None or len(expect_failure_msg) > 0):
        fail("Expected failure message can only be None or non-empty string")

    buck2_e2e_test(
        contacts = contacts,
        name = name,
        srcs = {"fbcode//buck2/tests/e2e_util:test_bxl_check_dependencies_template.py": "test_bxl_check_dependencies_template.py"},
        env = {
            "ALLOWLIST": allowlist_patterns,
            "BLOCKLIST": blocklist_patterns,
            "BXL_MAIN": bxl_main,
            "EXPECT_FAILURE_MSG": expect_failure_msg or "",
            "TARGET": target,
        } | (env or {}),
        # fbcode_macros uses tags instead of labels
        tags = ["check_dependencies_test"],
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
    buck2_e2e_test(
        name = name,
        contacts = contacts,
        srcs = {"fbcode//buck2/tests/e2e_util:test_bxl_assert_dependencies_template.py": "test_bxl_assert_dependencies_template.py"},
        env = {
            "BXL_MAIN": "fbcode//buck2/tests/assert_dependencies_test.bxl:test",
            "DEPS": ",".join(expected_deps),
            "EXPECT_FAILURE_MSG": expect_failure_msg or "",
            "TARGET": target,
        },
        # fbcode_macros uses tags instead of labels
        tags = ["assert_dependencies_test"],
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
    buck2_e2e_test(
        name = name,
        contacts = contacts,
        srcs = {"fbcode//buck2/tests/e2e_util:test_bxl_audit_dependents_template.py": "test_bxl_audit_dependents_template.py"},
        env = {
            "ALLOWLIST": ",".join(allowlist_patterns) if allowlist_patterns else "",
            "BXL_MAIN": "fbcode//buck2/tests/audit_dependents_test.bxl:test",
            "EXPECT_FAILURE_MSG": expect_failure_msg or "",
            "SOURCE_TARGET": source_target,
            "TARGET": target,
        },
        # fbcode_macros uses tags instead of labels
        tags = ["audit_dependents_test"],
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
