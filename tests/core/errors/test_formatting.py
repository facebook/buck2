# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

# pyre-strict


import re
from typing import List

from buck2.tests.e2e_util.api.buck import Buck
from buck2.tests.e2e_util.asserts import expect_failure
from buck2.tests.e2e_util.buck_workspace import buck_test
from buck2.tests.e2e_util.helper.golden import golden


def _sanitize(s: str) -> str:
    # Remove configuration hashes
    s = re.sub(r"\b[0-9a-f]{16}\b", "<HASH>", s)
    # And action digests
    s = re.sub(r"\b[0-9a-f]{40}:[0-9]{1,3}\b", "<DIGEST>", s)
    return s


def error_formatting_test(
    name: str, command: List[str], command_name: str = "build"
) -> None:
    async def impl(buck: Buck) -> None:
        func = getattr(buck, command_name)
        res = await expect_failure(func("--console=none", *command))
        golden(
            output=_sanitize(res.stderr),
            rel_path="fixtures/" + name + ".golden.stderr",
        )

    globals()[name] = impl

    buck_test()(impl)


error_formatting_test(name="test_action_fail", command=["//:action_fail"])

error_formatting_test(
    name="test_missing_dep",
    command=["//:missing_dep"],
)

error_formatting_test(
    name="test_missing_dep_cquery",
    command=["//:missing_dep"],
    command_name="cquery",
)

error_formatting_test(
    name="test_attr_coercion",
    command=["//attr_coercion:int_rule"],
)

error_formatting_test(
    name="test_during_load",
    command=["//during_load:whatever"],
)

error_formatting_test(
    name="test_during_load_via_dep",
    command=["//during_load/via_dep:via_dep"],
)

error_formatting_test(
    name="test_during_parse",
    command=["//during_parse:whatever"],
)

error_formatting_test(
    name="test_during_select_map",
    command=["//during_select:map"],
)

error_formatting_test(
    name="test_bxl_no_stacktrace",
    command=["//fail_no_stacktrace.bxl:fail_no_stacktrace_test"],
    command_name="bxl",
)

error_formatting_test(
    name="test_bxl_no_stacktrace_verbose",
    command=["//fail_no_stacktrace.bxl:fail_no_stacktrace_test", "-v5"],
    command_name="bxl",
)

error_formatting_test(
    name="test_bxl_with_stacktrace",
    command=["//fail_no_stacktrace.bxl:fail_with_stacktrace_test"],
    command_name="bxl",
)

error_formatting_test(
    name="test_bxl_attr_coercion",
    command=["//fail_attr_coercion.bxl:int_rule"],
    command_name="bxl",
)

error_formatting_test(
    name="test_duplicate_target",
    command=["//duplicate_target:foo"],
)

error_formatting_test(
    name="test_duplicate_target_with_stacktrace",
    command=["//duplicate_target:foo", "--stack"],
)
