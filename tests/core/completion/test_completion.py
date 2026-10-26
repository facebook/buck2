# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

# pyre-strict

import os
import platform
import subprocess
import typing
from pathlib import Path
from typing import Any, Callable

from buck2.tests.e2e_util.api.buck import Buck
from buck2.tests.e2e_util.buck_workspace import buck_test, BuckTestCase, parametrize

IS_LINUX: bool = platform.system() == "Linux"

# Downloading fish on Mac is not straightforward, so only test it on Linux
SHELLS = ["bash", "fish", "zsh"] if IS_LINUX else ["bash", "zsh"]


def completion_test(
    input: str,
    expected: typing.List[str] | typing.Callable[[list[str]], bool],
    shells: list[str] = SHELLS,
    options_only: bool = False,
    cwd: str = "",
    bin: str = "buck2",
) -> Callable[..., Any]:
    """
    A test per shell, named `<attribute>_<shell>`. A test of one shell is a single method, so
    its attribute carries the shell's name itself (`test_x_bash`); the same scenario can then
    have one expectation for bash and another for the rest.
    """
    if not IS_LINUX:
        # As above, not supported on Mac
        shells = [shell for shell in shells if shell != "fish"]

    async def impl(self: BuckTestCase, buck: Buck, shell: str) -> None:
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

        # Bash and Zsh infer completion through PTY idle periods, so avoid including daemon
        # startup in that timing window. Fish also uses the warmed daemon for parity.
        await buck.uquery("//...")

        # The generated completion script invokes Buck recursively. It must inherit the same
        # external config and file watcher settings as the warmed daemon.
        actual = subprocess.check_output(
            script_path.absolute(),
            input=f"{bin} {input}",
            text=True,
            cwd=buck.cwd.joinpath(cwd),
            env=buck._env,
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

    if len(shells) == 1:
        (shell,) = shells

        async def for_the_shell(self: BuckTestCase, buck: Buck) -> None:
            await impl(self, buck, shell)

        return buck_test()(for_the_shell)
    return buck_test()(parametrize("shell", shells)(impl))


class CompletionTest(BuckTestCase):
    test_command_name_bash = completion_test(
        input="t",
        # FIXME(JakobDegen): Should probably not be inconsistent
        expected=["test", "targets"] if IS_LINUX else ["targets", "test"],
        options_only=True,
        # Skip this on zsh and fish because they have fancy formatting with help messages for commands
        shells=["bash"],
    )

    test_build_flags = completion_test(
        # Use `--p` so that we don't get too many outputs, which the test framework doesn't handle well
        # on zsh
        input="build --p",
        options_only=True,
        expected=lambda actual: (
            "--prefer-local" in actual and "--prefer-remote" in actual
        ),
    )

    test_build_flags_buck_bin = completion_test(
        # Use `--p` so that we don't get too many outputs, which the test framework doesn't handle well
        # on zsh
        input="build --p",
        options_only=True,
        expected=lambda actual: (
            "--prefer-local" in actual and "--prefer-remote" in actual
        ),
        bin="buck",
    )

    test_completes_simple_partial_directory = completion_test(
        input="build d",
        expected=["dir1/", "dir1:", "dir2/"],
        shells=["zsh", "fish"],
    )

    test_completes_simple_partial_directory_bash = completion_test(
        input="build d",
        # On bash & mac, there's no way to disable sorting
        expected=["dir1/", "dir1:", "dir2/"]
        if IS_LINUX
        else ["dir1:", "dir1/", "dir2/"],
        shells=["bash"],
    )

    test_completes_simple_directory = completion_test(
        input="build dir",
        expected=["dir1/", "dir1:", "dir2/"],
        shells=["zsh", "fish"],
    )

    test_completes_simple_directory_bash = completion_test(
        input="build dir",
        # On bash & mac, there's no way to disable sorting
        expected=["dir1/", "dir1:", "dir2/"]
        if IS_LINUX
        else ["dir1:", "dir1/", "dir2/"],
        shells=["bash"],
    )

    test_completes_dir_from_trailing_slash = completion_test(
        input="build other/",
        expected=["other/far/", "other/far:", "other/foo/", "other/foo:"],
        shells=["zsh", "fish"],
    )

    test_completes_dir_from_trailing_slash_bash = completion_test(
        input="build other/",
        # On bash & mac, there's no way to disable sorting
        expected=(
            ["other/far/", "other/far:", "other/foo/", "other/foo:"]
            if IS_LINUX
            else ["other/far:", "other/far/", "other/foo:", "other/foo/"]
        ),
        shells=["bash"],
    )

    test_completes_simple_cells = completion_test(
        input="build cell",
        expected=["cell2a//", "cell2a//:", "cell3//", "cell3//:"],
    )

    test_completes_rule = completion_test(
        input="build dir1:target1",
        expected=["dir1:target1a", "dir1:target1b"],
    )

    test_starts_with_colon = completion_test(
        input="build :tar",
        expected=[":target1a", ":target1b"],
        cwd="dir1",
    )

    test_colon_only_arg = completion_test(
        input="build :",
        expected=[":target1a", ":target1b"],
        cwd="dir1",
    )

    test_does_not_complete_files_in_target_position = completion_test(
        input="build TARG",
        expected=[],
        cwd="dir1",
        shells=["fish", "zsh"],
    )

    test_does_not_complete_files_in_target_position_bash = completion_test(
        input="build TARG",
        # FIXME(JakobDegen): Bug: expected=[],
        expected=["TARGETS.fixture"],
        cwd="dir1",
        shells=["bash"],
    )

    test_no_targets_for_required_option_param = completion_test(
        input="build --console ",
        expected=["auto", "none", "simple", "simplenotty", "simpletty", "super"],
    )

    test_default_completions_after_run_dash_dash = completion_test(
        input="run :sometarget -- d",
        expected=["dir1/", "dir2/"],
    )

    test_generated_target_handling = completion_test(
        input="build //generated:t_",
        expected=[
            "//generated:t_no_labels",
            "//generated:t_not_generated",
            "//generated:t_not_generated2",
        ],
        shells=["bash", "zsh"],
    )

    test_generated_target_handling_fish = completion_test(
        input="build //generated:t_",
        expected=[
            "//generated:t_not_generated",
            "//generated:t_not_generated2",
            "//generated:t_no_labels",
        ],
        # FIXME(JakobDegen): Why is this different from above?
        shells=["fish"],
    )
