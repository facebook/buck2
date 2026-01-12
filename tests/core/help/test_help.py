# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

# pyre-strict


import asyncio
import re

from buck2.tests.e2e_util.api.buck import Buck
from buck2.tests.e2e_util.buck_workspace import buck_test
from buck2.tests.e2e_util.helper.golden import golden


def _normalize(s: str) -> str:
    s = re.sub(
        r"buck2 [a-z0-9]{16,64} (<build-id>|<exe-hash>)",
        "buck2 <version> <version-source>",
        s,
    )
    s = re.sub(r"buck2\.exe", "buck2", s)
    return "\n".join([x.rstrip() for x in s.splitlines()]) + "\n"


def _find_subcommands(help: str) -> list[str]:
    help = re.sub(r".*SUBCOMMANDS:", "", help, flags=re.DOTALL)
    result = re.findall(r"^  ([a-z][a-z0-9_-]*)", help, flags=re.MULTILINE)
    result = list(result)
    return result


semaphore = asyncio.Semaphore(10)


async def _test_help(buck: Buck, command_stack: list[str]) -> int:
    async with semaphore:
        result = await buck.help(*command_stack)

    name = "-".join(["help", *command_stack])
    golden(
        output=_normalize(result.stdout),
        rel_path=f"buck2-{name}.golden.txt",
    )

    subcommands = _find_subcommands(result.stdout)
    subtasks = [
        _test_help(buck, command_stack + [subcommand]) for subcommand in subcommands
    ]
    subresults = await asyncio.gather(*subtasks)

    return sum(subresults) + 1


@buck_test()
async def test_help(buck: Buck) -> None:
    total = await _test_help(buck, [])
    assert total > 4
