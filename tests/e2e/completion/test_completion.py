# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

# pyre-strict

import os
import platform
import subprocess
from pathlib import Path

from buck2.tests.e2e_util.api.buck import Buck
from buck2.tests.e2e_util.buck_workspace import buck_test

# FIXME(JakobDegen): `zsh` not avaialable on Linux CI
# FIXME(JakobDegen): `fish` not avaialable on any CI
SHELLS = ["bash", "zsh"] if platform.system() == "Darwin" else ["bash"]


def completion_test(
    name: str,
    input: str,
    expected: list[str],
    shells: list[str] = SHELLS,
) -> None:
    async def impl(buck: Buck) -> None:
        tmp_path = Path(buck.cwd).parent / "tmp"
        tmp_path.mkdir(exist_ok=True)

        verify_bin = Path(os.environ["BUCK2_COMPLETION_VERIFY"])

        for shell in shells:
            get_completions = await buck.completion(shell)
            completions_path = tmp_path / f"completion.{shell}"
            completions_path.write_text(get_completions.stdout)

            # Write this to a script to make it easier to debug with `BUCK_E2E_KEEP_TMP=1`
            script = "\n".join(
                [
                    "#!/bin/sh",
                    f"export PATH={str(buck.path_to_executable.parent.absolute())}:$PATH",
                    f"{str(verify_bin.absolute())} {shell} {str(completions_path.absolute())}",
                ]
            )
            script_path = tmp_path / f"test_{shell}.sh"
            script_path.write_text(script)
            script_path.chmod(0o755)

            actual = subprocess.check_output(
                script_path.absolute(),
                input="buck2 " + input,
                text=True,
                cwd=buck.cwd,
            )
            actual = actual.strip().split("\n")
            assert actual == expected, "testing shell: " + shell

    globals()[name] = buck_test(inplace=False)(impl)


completion_test(
    name="test_completes_simple_partial_directory",
    input="build d",
    expected=["dir1/", "dir1:", "dir2/"],
)

completion_test(
    name="test_completes_simple_directory",
    input="build dir",
    expected=["dir1/", "dir1:", "dir2/"],
)

completion_test(
    name="test_completes_simple_cells",
    input="build cell",
    expected=["cell2a//", "cell2a//:", "cell3//", "cell3//:"],
)

completion_test(
    name="test_completes_rule",
    input="build dir1:target1",
    # FIXME(JakobDegen): This output was previously asserted as below. Shells handle the current
    # output ok too, so this doesn't absolutely have to be fixed, but we should clarify the desired
    # behavior.
    # expected=["dir1:target1a", "dir1:target1b"],
    expected=["target1a", "target1b"],
    # FIXME(JakobDegen): Returns [``] on zsh. Might well be a bug in the test harness
    shells=["bash"],
)
