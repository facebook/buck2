# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

# pyre-strict

import typing
from pathlib import Path

from buck2.tests.e2e_util.api.buck import Buck
from buck2.tests.e2e_util.buck_workspace import buck_test


def complete_test(
    name: str,
    input: str,
    expected: typing.List[str],
    cwd: str = "",
) -> None:
    async def impl(buck: Buck) -> None:
        res = await buck.complete("--target", input, rel_cwd=Path(cwd))
        assert res.stdout.splitlines() == expected

    globals()[name] = buck_test()(impl)


complete_test(
    name="test_target_provides_targets_for_path_ending_with_a_colon",
    input="baredir0/buckdir0b:",
    expected=[
        "baredir0/buckdir0b:target1",
        "baredir0/buckdir0b:target2",
        "baredir0/buckdir0b:target3",
    ],
)

complete_test(
    name="test_provides_targets_in_nested_cell",
    input="buck2:",
    expected=["buck2:buck2", "buck2:symlinked_buck2_and_tpx"],
    cwd="cell1",
)

complete_test(
    name="test_completes_a_partial_target",
    input="buck2:bu",
    expected=["buck2:buck2"],
    cwd="cell1",
)

complete_test(
    name="test_completes_targets_for_fully_qualified_cell",
    input="cell1//:",
    expected=["cell1//:target1"],
)

complete_test(
    name="test_completes_other_cell_from_subdirectory",
    input="cell1//buck2:",
    expected=["cell1//buck2:buck2", "cell1//buck2:symlinked_buck2_and_tpx"],
    cwd="baredir0",
)

complete_test(
    name="test_expands_cell_to_canonical_with_colon",
    input="cell1/buck2:",
    expected=["cell1//buck2:buck2", "cell1//buck2:symlinked_buck2_and_tpx"],
)

complete_test(
    name="test_expands_cell_to_canonical_with_partial_target",
    input="cell1/buck2:bu",
    expected=["cell1//buck2:buck2"],
)

complete_test(
    name="test_expands_target_for_bare_colon",
    input=":",
    expected=[":buck2", ":symlinked_buck2_and_tpx"],
    cwd="cell1/buck2",
)

complete_test(
    name="test_target_completion_with_aliased_cells",
    input="cell1_alias//buck2:",
    expected=["cell1_alias//buck2:buck2", "cell1_alias//buck2:symlinked_buck2_and_tpx"],
    cwd="cell1/buck2/fake_prelude",
)
