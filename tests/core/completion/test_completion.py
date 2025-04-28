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
import typing
from pathlib import Path

from buck2.tests.e2e_util.api.buck import Buck
from buck2.tests.e2e_util.buck_workspace import buck_test

IS_LINUX: bool = platform.system() == "Linux"

# Downloading fish on Mac is not straightforward, so only test it on Linux
SHELLS = ["bash", "fish", "zsh"] if IS_LINUX else ["bash", "zsh"]


def completion_test(
    name: str,
    input: str,
    expected: typing.List[str] | typing.Callable[[list[str]], bool],
    shells: list[str] = SHELLS,
    options_only: bool = False,
    cwd: str = "",
    bin: str = "buck2",
) -> None:
    for shell in shells:
        if shell == "fish" and not IS_LINUX:
            # As above, not supported on Mac
            continue

        # shell=shell is a trick to get the variable captured by value
        async def impl(buck: Buck, shell: str = shell) -> None:
            tmp_path = Path(buck.cwd).parent / "tmp"
            tmp_path.mkdir(exist_ok=True)

            verify_bin = Path(os.environ["BUCK2_COMPLETION_VERIFY"])

            get_completions = await buck.completion(
                shell, *(["--options-only"] if options_only else [])
            )
            completions_path = tmp_path / f"completion.{shell}"
            completions_path.write_text(get_completions.stdout)

            shell_home = (tmp_path / f"{shell}_tmp").absolute()
            shell_home.mkdir(exist_ok=True)

            # Write this to a script to make it easier to debug with `BUCK_E2E_KEEP_TMP=1`
            script = "\n".join(
                [
                    "#!/bin/bash",
                    "shopt -s dotglob",
                    f'export PATH="{buck.path_to_executable.parent.absolute()}:$PATH"',
                    "export BUCK2_COMPLETION_TIMEOUT=30000",
                    f"if [ -n \"$( ls -A '{shell_home}' )\" ]; then",
                    f"    rm -r -- {shell_home}/*",
                    "fi",
                    f"{verify_bin.absolute()} --tempdir {shell_home} --name {bin} {shell} {completions_path.absolute()}",
                ]
            )
            script_path = tmp_path / f"test_{shell}.sh"
            script_path.write_text(script)
            script_path.chmod(0o755)

            # Because shells don't report when they're done generating completions, these tests are
            # fundamentally racey. Improve on that a little bit by "warming up" the daemon before
            # doing the actual test.
            await buck.uquery("//...")

            actual = subprocess.check_output(
                script_path.absolute(),
                input=f"{bin} {input}",
                text=True,
                cwd=buck.cwd.joinpath(cwd),
            )
            actual = actual.splitlines()
            if isinstance(expected, list):
                expected_ = expected
                if shell == "bash":
                    # Bash treats `:` as a separator for the purpose of
                    # completions as long as it's not at the end, so adjust for
                    # that
                    expected_ = []
                    for i in expected:
                        if i.endswith(":"):
                            before = i[:-1]
                            expected_.append(before.split(":")[-1] + ":")
                        else:
                            expected_.append(i.split(":")[-1])
                assert actual == expected_, "testing shell: " + shell
            else:
                assert expected(actual), "testing shell: " + shell

        globals()[name + "_" + shell] = buck_test()(impl)


completion_test(
    name="test_command_name",
    input="t",
    # FIXME(JakobDegen): Should probably not be inconsistent
    expected=["test", "targets"] if IS_LINUX else ["targets", "test"],
    options_only=True,
    # Skip this on zsh and fish because they have fancy formatting with help messages for commands
    shells=["bash"],
)

completion_test(
    name="test_build_flags",
    # Use `--p` so that we don't get too many outputs, which the test framework doesn't handle well
    # on zsh
    input="build --p",
    options_only=True,
    expected=lambda actual: (
        "--prefer-local" in actual and "--prefer-remote" in actual
    ),
)

completion_test(
    name="test_build_flags_buck_bin",
    # Use `--p` so that we don't get too many outputs, which the test framework doesn't handle well
    # on zsh
    input="build --p",
    options_only=True,
    expected=lambda actual: (
        "--prefer-local" in actual and "--prefer-remote" in actual
    ),
    bin="buck",
)

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
    name="test_completes_dir_from_trailing_slash",
    input="build other/",
    # FIXME(JakobDegen): Bug
    # expected=["other/far/", "other/far:", "other/foo/", "other/foo:"],
    expected=[],
    shells=["bash", "zsh"],
)

completion_test(
    name="test_completes_dir_from_trailing_slash",
    input="build other/",
    # FIXME(JakobDegen): Bug
    # expected=["other/far/", "other/far:", "other/foo/", "other/foo:"],
    expected=[
        "other//other/far/",
        "other//other/far:",
        "other//other/foo/",
        "other//other/foo:",
    ],
    shells=["fish"],
)

completion_test(
    name="test_completes_simple_cells",
    input="build cell",
    expected=["cell2a//", "cell2a//:", "cell3//", "cell3//:"],
)

completion_test(
    name="test_completes_rule",
    input="build dir1:target1",
    expected=["dir1:target1a", "dir1:target1b"],
)

completion_test(
    name="test_starts_with_colon",
    input="build :tar",
    expected=[":target1a", ":target1b"],
    cwd="dir1",
)

completion_test(
    name="test_colon_only_arg",
    input="build :",
    expected=[":target1a", ":target1b"],
    cwd="dir1",
)

completion_test(
    name="test_does_not_complete_files_in_target_position",
    input="build TARG",
    expected=[],
    cwd="dir1",
    shells=["fish", "zsh"],
)

completion_test(
    name="test_does_not_complete_files_in_target_position",
    input="build TARG",
    # FIXME(JakobDegen): Bug: expected=[],
    expected=["TARGETS.fixture"],
    cwd="dir1",
    shells=["bash"],
)

completion_test(
    name="test_no_targets_for_required_option_param",
    input="build --console ",
    expected=["auto", "none", "simple", "simplenotty", "simpletty", "super"],
)

completion_test(
    name="test_default_completions_after_run_dash_dash",
    input="run :sometarget -- d",
    expected=["dir1/", "dir2/"],
)

completion_test(
    name="test_generated_target_handling",
    input="build //generated:t_",
    expected=[
        "//generated:t_no_labels",
        "//generated:t_not_generated",
        "//generated:t_not_generated2",
    ],
    shells=["bash", "zsh"],
)

completion_test(
    name="test_generated_target_handling",
    input="build //generated:t_",
    expected=[
        "//generated:t_not_generated",
        "//generated:t_not_generated2",
        "//generated:t_no_labels",
    ],
    # FIXME(JakobDegen): Why is this different from above?
    shells=["fish"],
)
