#!/usr/bin/env fbpython
# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

# pyre-unsafe

import contextlib
import hashlib
import json
import os
import platform
import shutil
import subprocess
import sys
import tempfile
from collections import namedtuple
from pathlib import Path
from typing import (
    AsyncGenerator,
    AsyncIterator,
    Awaitable,
    Callable,
    Dict,
    Iterable,
    List,
    Optional,
)

import __manifest__
import pytest
from buck2.tests.e2e_util.api.buck import Buck
from buck2.tests.e2e_util.api.executable import WindowsCmdOption
from decorator import decorator

BuckTestMarker = namedtuple(
    "BuckTestMarker",
    [
        "inplace",
        "data_dir",
        "allow_soft_errors",
        "extra_buck_config",
        "skip_final_kill",
        "setup_eden",
        "disable_daemon_cgroup",
    ],
)


@contextlib.asynccontextmanager
async def buck_fixture(  # noqa C901 : "too complex"
    marker,
) -> AsyncGenerator[Buck, None]:
    """Returns a Buck for testing"""

    is_windows = platform.system() == "Windows"
    test_executable = os.environ["TEST_EXECUTABLE"]

    env: Dict[str, str] = {**os.environ}
    # This is necessary for static linking on Linux.
    if platform.system() != "Windows":
        env["BUCKD_STARTUP_TIMEOUT"] = "120"
        env["BUCKD_STARTUP_INIT_TIMEOUT"] = "120"

    # allow_soft_errors will override any existing environment variable behavior
    if marker.allow_soft_errors or marker.inplace:
        env["BUCK2_HARD_ERROR"] = "false"

    # Use a very small stdin buffer to catch any scenarios in which we
    # don't properly handle partial input.
    env["BUCK2_TEST_STDIN_BUFFER_SIZE"] = "8"
    # Explicitly disable log uploading, we don't care about stats for tests.
    env["BUCK2_TEST_DISABLE_LOG_UPLOAD"] = "true"
    # But still block on it, because the upload process also writes
    # locally, and we want that to be synchronous instead of backgrounded.
    env["BUCK2_TEST_BLOCK_ON_UPLOAD"] = "true"
    # Require the events dispatcher to be set for e2e tests.
    env["ENFORCE_DISPATCHER_SET"] = "true"
    # Inform buck of the test timeout
    env["BUCK2_SELF_TEST_TIMEOUT_S"] = "600"
    # Timeout Watchman requests because we often see it hang and crash.
    env["BUCK2_WATCHMAN_TIMEOUT"] = "30"
    env["BUCK2_RUNTIME_THREADS"] = "8"
    # Avoid noise in stderr.
    env["BUCK2_IGNORE_VERSION_EXTRACTION_FAILURE"] = "true"
    env["SUPERCONSOLE_TESTING_WIDTH"] = "100"
    env["SUPERCONSOLE_TESTING_HEIGHT"] = "100"
    # Don't try to assign to a new cgroup during tests.
    if marker.disable_daemon_cgroup:
        env["BUCK2_TEST_DISABLE_DAEMON_CGROUP"] = "true"

    assert "BUCK2_RUNTIME_THREADS" in env, (
        "BUCK2_RUNTIME_THREADS should be set by the test macros"
    )
    assert "BUCK2_MAX_BLOCKING_THREADS" in env, (
        "BUCK2_MAX_BLOCKING_THREADS should be set by the test macros"
    )
    # Windows uses blocking threads for subprocess I/O so we can't do this there.
    del env["BUCK2_MAX_BLOCKING_THREADS"]

    # Filter out some environment variables that may interfere with the
    # running of tests. Notably, since this framework is used to write
    # Python tests that run Buck, we clear out Python test environment
    # variables so that if we run a Python test via Buck 2, they won't
    # interfere.
    for var in ["PYTEST_CURRENT_TEST", "TEST_PILOT"]:
        env.pop(var, None)

    common_dir = await _get_common_dir()
    base_dir = Path(tempfile.mkdtemp(dir=common_dir))

    isolation_prefix = None
    keep_temp = os.environ.get("BUCK_E2E_KEEP_TEMP") == "1"

    env["BUCK2_TEST_SKIP_DEFAULT_EXTERNAL_CONFIG"] = "true"

    # Because we may change the working directory, create an absolute path to the test data srcs if
    # the exist and make it available in a different envvar. This is used by golden tests
    test_repo_data = os.environ.get("TEST_REPO_DATA")
    if test_repo_data is not None:
        os.environ["TEST_REPO_DATA_SRC"] = str(Path(test_repo_data).absolute())

    # Create a temporary file to store all lines of extra buck config values.
    extra_config_lines = []

    project_dir = base_dir / "project"

    # Temp dir needed for EdenFS, will only be created if necessary
    eden_dir = base_dir / "eden"

    try:
        if marker.setup_eden:
            assert not marker.inplace, (
                "EdenFS for e2e tests is not supported for inplace tests"
            )

            _setup_eden(
                eden_dir,
                project_dir,
                env,
                is_windows,
            )

        if marker.inplace:
            # We need a unique isolated prefix per test case.
            current_test = (
                __manifest__.fbmake["build_rule"] + os.environ["PYTEST_CURRENT_TEST"]
            )
            isolation_prefix = hashlib.sha1(current_test.encode("utf-8")).hexdigest()
            # FIXME(T136079642): Buck2 on Windows has problem with relative symlinks over 260 chars, shorten the hash
            if is_windows:
                isolation_prefix = isolation_prefix[:5]
            else:
                isolation_prefix = ".buck_e2e" + isolation_prefix

            buck_cwd = Path.cwd()

            # NOTE: In theory, this isn't true of all Linux hosts, but all
            # our tests actually rely on it and will break if you ran them
            # on a host without this, so just make it the default.
            if sys.platform == "linux":
                extra_config_lines.append("[host_features]\ngvfs = true\n")
            # NOTE: This buckconfig is depended on by our CI validation for
            # CLI modifiers in tools/build_defs/buck2/cfg/validation/validation.bzl. If
            # the name of this buckconfig ever changes, please update the validation
            # as well.
            extra_config_lines.append("[buildfile]\nextra_for_test = TARGETS.test\n")

        else:
            if marker.data_dir is not None:
                src = Path(os.environ["TEST_REPO_DATA"], marker.data_dir)
                _copytree(src, project_dir)
                _maybe_setup_prelude_and_ovr_config(project_dir)
                with open(Path(project_dir, ".watchmanconfig"), "w") as f:
                    # Use the FS Events watcher, which is more reliable than the default.
                    json.dump(
                        {
                            "ignore_dirs": ["buck-out", ".hg"],
                            "fsevents_watch_files": True,
                            "prefer_split_fsevents_watcher": False,
                        },
                        f,
                    )

            # `edenfs` watcher requires eden to be setup which is too slow to enable on all tests
            # use edenfs watcher whenever possible in test, otherwise use `fs_hash_crawler`
            # FYI: if you remove this, make sure to remove it from external_buckconfig tests too
            if marker.setup_eden:
                extra_config_lines.append("[buck2]\nfile_watcher = edenfs\n")
                extra_config_lines.append("[buck2]\nallow_eden_io = true\n")
            else:
                extra_config_lines.append("[buck2]\nfile_watcher = fs_hash_crawler\n")

            buck_cwd = project_dir

        for section, config in marker.extra_buck_config.items():
            extra_config_lines.append(f"[{section}]\n")
            for key, value in config.items():
                extra_config_lines.append(f"{key} = {value}\n")

        extra_config = os.path.join(base_dir, "extra.bcfg")
        with open(extra_config, "w") as f:
            for line in extra_config_lines:
                f.write(line)
        env["BUCK2_TEST_EXTRA_EXTERNAL_CONFIG"] = extra_config

        buck = Buck(
            Path(test_executable),
            cwd=buck_cwd,
            encoding="utf-8",
            env=env,
        )

        if isolation_prefix is not None:
            buck.set_isolation_prefix(isolation_prefix)

        yield buck

        if not marker.skip_final_kill:
            if keep_temp:
                await buck.kill()
            else:
                await buck.clean()
    finally:
        if keep_temp:
            print(f"Not deleting temporary directory at {base_dir}", file=sys.stderr)
        else:
            if marker.setup_eden:
                _cleanup_eden(eden_dir, project_dir, env)
            shutil.rmtree(base_dir, ignore_errors=True)


@pytest.fixture(scope="function")
async def buck(request) -> AsyncIterator[Buck]:
    marker = request.node.get_closest_marker("buck_test")
    if marker is None:
        raise Exception(
            "Test method must be decorated with @buck_test() to use the buck fixture."
        )
    marker = marker.args[0]
    async with buck_fixture(marker) as buck:
        yield buck


async def _get_common_dir() -> Path:
    from asyncio import subprocess

    """
    Returns a temporary directory using mkscratch.
    The advantage of using mkscratch is that it can return the same directory on multiple calls.
    If mkscratch is not available (e.g., Windows or remote environments), fall back to the system temp directory.
    """
    # Check if mkscratch is available, fall back to tempfile.gettempdir() if not
    if shutil.which("mkscratch") is None:
        return Path(tempfile.gettempdir())

    # Need to use `--hash` over `--subdir` here because the tmp path would be too long and
    # Eden would fail with `Socket path too large to fit into sockaddr_un` otherwise
    mkscratch_proc = await subprocess.create_subprocess_exec(
        "mkscratch",
        "path",
        "--hash",
        stdout=subprocess.PIPE,
    )
    stdout, _ = await mkscratch_proc.communicate()
    assert stdout is not None, "stdout should not be None"
    common_dir = Path(stdout.decode().strip())
    return common_dir


def nobuckd(fn: Callable) -> Callable:
    """Disables buck daemon"""

    def wrapped(fn: Callable, buck: Buck, *args, **kwargs):
        buck.set_buckd(True)
        return fn(buck, *args, **kwargs)

    return decorator(wrapped, fn)


def _eden_base_cmd(eden_dir: Path) -> List[str]:
    config_dir = eden_dir / "config"
    etc_dir = eden_dir / "etc"
    home_dir = eden_dir / "home"

    config_dir.mkdir(exist_ok=True)
    etc_dir.mkdir(exist_ok=True)
    home_dir.mkdir(exist_ok=True)

    return [
        "eden",
        "--config-dir",
        str(config_dir),
        "--home-dir",
        str(home_dir),
        "--etc-eden-dir",
        str(etc_dir),
    ]


# Adapted from Eden integration test, didn't use their code because Eden uses the compiled binary in their buck-out
# which we don't have, extracting that part out would be more work than what was done below.
# https://www.internalfb.com/code/fbsource/[45334ead4a72]/fbcode/eden/integration/lib/testcase.py?lines=123
def _setup_eden(
    eden_dir: Path,
    project_dir: Path,
    env: Dict[str, str],
    is_windows: bool,
):
    eden_dir.mkdir(exist_ok=True)
    # Start up an EdenFS Client and point it to the temp dirs
    subprocess.check_call(
        _eden_base_cmd(eden_dir)
        + [
            "start",
        ],
        stdout=sys.stdout,
        stderr=sys.stderr,
        env=env,
    )

    temp_repo = eden_dir / "temp_repo"
    # Initialize a hg repo, so Eden can mount it
    subprocess.check_call(
        ["hg", "init", str(temp_repo)],
        stdout=sys.stdout,
        stderr=sys.stderr,
        env=env,
    )

    # Use .eden-redirections to force redirection to be setup at mount time
    # The number of concurrent APFS volumes we can create on macOS
    # is limited. Furthermore, cleaning up disk image redirections is non-trivial.
    # Let's use symlink redirections to avoid these issues.
    redirection_type = "symlink" if sys.platform == "darwin" else "bind"
    with open(temp_repo / ".eden-redirections", "w") as f:
        f.write(f'[redirections]\n"buck-out" = "{redirection_type}"\n')

    subprocess.check_call(
        ["hg", "commit", "--addremove", "-m", "init"],
        stdout=sys.stdout,
        stderr=sys.stderr,
        env=env,
        cwd=temp_repo,
    )

    # Mount the hg repo we created
    project_dir.mkdir(exist_ok=True)
    cmd = _eden_base_cmd(eden_dir) + [
        "clone",
        temp_repo,
        project_dir,
        "--allow-empty-repo",
        "--case-insensitive",
    ]

    if is_windows:
        cmd.append("--enable-windows-symlinks")

    subprocess.check_call(
        cmd,
        stdout=sys.stdout,
        stderr=sys.stderr,
        env=env,
    )


def _cleanup_eden(
    eden_dir: Path,
    project_dir: Path,
    env: Dict[str, str],
):
    # Remove the Eden mount created for the test
    subprocess.run(
        _eden_base_cmd(eden_dir)
        + [
            "remove",
            str(project_dir),
            "-y",
        ],
        stdout=sys.stdout,
        stderr=sys.stderr,
        env=env,
    )

    subprocess.run(
        _eden_base_cmd(eden_dir)
        + [
            "shutdown",
        ],
        stdout=sys.stdout,
        stderr=sys.stderr,
        env=env,
    )


def _copytree(
    src: Path,
    dst: Path,
    symlinks: bool = False,
    ignore: Optional[Callable[..., Iterable[str]]] = None,
) -> None:
    """Copies all files and directories from src into dst"""
    dst.mkdir(parents=True, exist_ok=True)
    for item in os.listdir(src):
        if item == "buck-out":
            continue
        s = src / item
        d = dst / item
        if os.path.isdir(s):
            shutil.copytree(s, d, symlinks, ignore, dirs_exist_ok=True)
        else:
            shutil.copy2(s, d)


def _maybe_setup_prelude_and_ovr_config(path: Path) -> None:
    if "PRELUDE" in os.environ or "OVR_CONFIG" in os.environ:
        if os.environ.get("BUCK2_E2E_TEST_FLAVOR") == "isolated":
            raise Exception(
                "Don't set `PRELUDE` or `OVR_CONFIG` in `tests/core` - these tests are always isolated"
            )

    if "PRELUDE" in os.environ:
        prelude = Path(path, "prelude")
        if not prelude.exists():
            _copytree(
                Path(os.environ["PRELUDE"]),
                Path(path, "prelude"),
            )

    # TODO: The toolchain platform definitions we hard-code in the prelude are
    # in the ovr_config cell, so copy them in for now.  Longer-term, D31566140
    # has a discusion on bettter approaches.
    if "OVR_CONFIG" not in os.environ:
        return
    _copytree(
        Path(os.pardir, "arvr", "tools", "build_defs", "config"),
        Path(path, "arvr", "tools", "build_defs", "config"),
    )

    _copytree(
        Path(os.pardir, "tools", "build_defs", "fbcode_macros"),
        Path(path, "tools", "build_defs", "fbcode_macros"),
    )

    with Path(path, ".buckconfig").open("a") as f:
        print(
            "", file=f
        )  # append newline because test `.buckconfig` may not end with newline
        print("# Following lines are added by buck_workspace.py", file=f)
        print("[repositories]", file=f)
        print("ovr_config = arvr/tools/build_defs/config", file=f)
        print("fbcode_macros = tools/build_defs/fbcode_macros", file=f)
        print("config = arvr/tools/build_defs/config", file=f)
    with Path(path, "arvr", "tools", "build_defs", "config", ".buckconfig").open(
        "w"
    ) as f:
        pass
    with Path(path, "tools", "build_defs", "fbcode_macros", ".buckconfig").open(
        "w"
    ) as f:
        pass


BuckTestFn = Callable[..., Awaitable[None]]

SKIPPABLE_PLATFORMS = ["darwin", "linux", "windows"]


def buck_test(
    inplace: bool | None = None,
    data_dir: Optional[str] = "",
    # Accepted values are specified in SKIPPABLE_PLATFORMS
    skip_for_os: List[str] = [],  # noqa: B006 value is read-only
    allow_soft_errors=False,
    extra_buck_config: Optional[Dict[str, Dict[str, str]]] = None,
    skip_final_kill=False,
    setup_eden=False,
    disable_daemon_cgroup=True,
) -> Callable:
    """
    Defines a buck test. This is a must have decorator on all test case functions.

    Parameters:
        inplace:
            A bool for whether to run tests in-repo.
            If false, runs test under a sandbox repo. If `data_dir` or `data` are set on the target,
            the sandbox repo will be initialized with the contents of that directory. This can be
            disabled by setting `data_dir = None` on the test, or the test can set
            `data_dir = "subdir"` to just use the contents of a subdirectory.
            If true, runs test in fbsource.
        data_dir:
            data_dir is an optional string.
            If data_dir is set, then data_dir is the directory that contains test project data to
            copy, or the working directory relative to the cwd.
        skip_for_os:
            List of OS to skip the test on.
        allow_soft_errors:
            Like it says in the arg name. The default is to hard error.
        extra_buck_config:
            A optional dict of extra buck config to add to the test.
            The key is the section name, the value is a dict of key value pairs.
        skip_final_kill:
            Don't run a `buck2 kill` or `buck2 clean` at the end of the test
        setup_eden:
            Whether or not to set up an EdenFS repo for this test. Only matters for inplace=False.
            Note that this will slow the test down, so it should not be widely enabled.
    """

    if inplace and data_dir == "":
        data_dir = None

    if os.environ.get("BUCK2_E2E_TEST_FLAVOR") == "isolated":
        if inplace is not None:
            raise Exception(
                "Don't set `inplace` in `tests/core` - these tests are always isolated"
            )

        inplace = False
    else:
        if inplace is None:
            raise Exception("`inplace` must be set for `buck_test()`")

    # Set up arguments to use for the buck fixture.

    # Just ignore the test, calling pytest.skip() is treated as a failure by tpx unfortunately
    for p in skip_for_os:
        if p not in SKIPPABLE_PLATFORMS:
            raise Exception(f"skip_for_os must specifiy one of {SKIPPABLE_PLATFORMS}")
    if platform.system().lower() in skip_for_os:
        return lambda *args: None

    if data_dir is not None and inplace:
        raise Exception(
            "`data_dir` is not an allowed parameter for an `inplace=True`test"
        )

    return pytest.mark.buck_test(
        BuckTestMarker(
            inplace=inplace,
            data_dir=data_dir,
            allow_soft_errors=allow_soft_errors,
            extra_buck_config=extra_buck_config or {},
            skip_final_kill=skip_final_kill,
            setup_eden=setup_eden,
            disable_daemon_cgroup=disable_daemon_cgroup,
        )
    )


def env(key: str, value: str) -> Callable:
    """
    Decorator for adding an environment variable to a test case.
    For example, @env("BUCK_LOG", "info")
    """

    def inner_decorator(fn: BuckTestFn) -> Callable:
        async def wrapped(fn: BuckTestFn, buck: Buck, *args, **kwargs) -> None:
            buck.set_env(key, value)
            return await fn(buck, *args, **kwargs)

        return decorator(wrapped, fn)

    return inner_decorator


def windows_cmd_option(key: WindowsCmdOption, value: bool) -> Callable:
    """
    Decorator for specifying the state for cmd.exe's specified key feature
    For example, @windows_cmd_option(WindowsCmdOption.DelayedExpansion, True)
    """

    def inner_decorator(fn: BuckTestFn) -> Callable:
        async def wrapped(fn: BuckTestFn, buck: Buck, *args, **kwargs) -> None:
            buck.set_windows_cmd_option(key, value)
            return await fn(buck, *args, **kwargs)

        return decorator(wrapped, fn)

    return inner_decorator


def is_deployed_buck2() -> bool:
    """
    This function detects whether or not you are using a deployed version of buck2
    so you can skip certain rule tests to only use deployed buck2.
    This may break deployed buck2, so please make sure this only affects tests for rules
    that buck2 users are not using.

    Example of skipping test case with deployed buck2:
        @pytest.mark.skipif(
            is_deployed_buck2(),
            reason="Skip if testing with deployed buck2",
        )
    """
    return os.environ.get("TEST_EXECUTABLE") == "buck2"


def get_mode_from_platform(
    mode="dev", prefix=True, skip_validation_i_know_what_im_doing=False
) -> str:
    if not skip_validation_i_know_what_im_doing and (mode not in ("dev", "opt")):
        raise Exception(f"Invalid mode: {mode}")

    def modefile_basename():
        if sys.platform == "darwin":
            if mode.startswith("dev"):
                return "mac"
            else:
                return "opt-mac"
        elif sys.platform == "win32":
            if mode.startswith("dev"):
                return "win"
            else:
                return "opt-win"
        if mode.startswith("dev"):
            return "dev"
        else:
            return "opt"

    if prefix:
        return f"@fbcode//mode/{modefile_basename()}"

    return modefile_basename()
