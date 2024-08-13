#!/usr/bin/env fbpython
# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

# pyre-unsafe

import contextlib
import hashlib
import json
import os
import platform
import shutil
import sys
import tempfile
from asyncio import subprocess
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
from buck2.tests.e2e_util.api.executable_type import ExecutableType
from decorator import decorator

BuckTestMarker = namedtuple(
    "BuckTestMarker",
    [
        "inplace",
        "data_dir",
        "allow_soft_errors",
        "extra_buck_config",
        "skip_final_kill",
    ],
)


@contextlib.asynccontextmanager
async def buck_fixture(  # noqa C901 : "too complex"
    marker,
) -> AsyncGenerator[Buck, None]:
    """Returns a Buck for testing"""

    is_windows = platform.system() == "Windows"
    test_executable_type = ExecutableType(os.environ["TEST_EXECUTABLE_TYPE"])
    test_executable = os.environ["TEST_EXECUTABLE"]

    env: Dict[str, str] = {**os.environ}
    if test_executable_type != ExecutableType.buck1:
        # This is necessary for static linking on Linux.
        if platform.system() != "Windows":
            env["BUCKD_STARTUP_TIMEOUT"] = "120"

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
        # Auto-destroy after a while. This should be longer than the test timeout.
        env["BUCK2_TERMINATE_AFTER"] = "650"
        # Timeout Watchman requests because we often see it hang and crash.
        env["BUCK2_WATCHMAN_TIMEOUT"] = "30"
        # Use little threads. We don't do much work in tests but we do run lots of Bucks.
        env["BUCK2_RUNTIME_THREADS"] = "2"

        # Windows uses blocking threads for subprocess I/O so we can't do this there.
        if not is_windows:
            env["BUCK2_MAX_BLOCKING_THREADS"] = "2"

    # Filter out some environment variables that may interfere with the
    # running of tests. Notably, since this framework is used to write
    # Python tests that run Buck, we clear out Python test environment
    # variables so that if we run a Python test via Buck 2, they won't
    # interfere.
    for var in ["PYTEST_CURRENT_TEST", "TEST_PILOT"]:
        env.pop(var, None)

    common_dir = await _get_common_dir()
    temp_dir = Path(tempfile.mkdtemp(dir=common_dir))

    isolation_prefix = None
    keep_temp = os.environ.get("BUCK_E2E_KEEP_TEMP") == "1"

    env["BUCK2_TEST_SKIP_DEFAULT_EXTERNAL_CONFIG"] = "true"

    # Because we may change the working directory, create an absolute path to the test data srcs if
    # the exist and make it available in a different envvar. This is used by golden tests
    test_repo_data = os.environ.get("TEST_REPO_DATA")
    if test_repo_data is not None:
        os.environ["TEST_REPO_DATA_SRC"] = str(Path(test_repo_data).absolute())

    try:
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

            extra_config = os.path.join(temp_dir, "extra.bcfg")
            with open(extra_config, "w") as f:
                # NOTE: In theory, this isn't true of all Linux hosts, but all
                # our tests actually rely on it and will break if you ran them
                # on a host without this, so just make it the default.
                if sys.platform == "linux":
                    f.write("[host_features]\ngvfs = true\n")
                for section, config in marker.extra_buck_config.items():
                    f.write(f"[{section}]\n")
                    for key, value in config.items():
                        f.write(f"{key} = {value}\n")
                f.write("[buildfile]\nextra_for_test = TARGETS.test\n")

            env["BUCK2_TEST_EXTRA_EXTERNAL_CONFIG"] = extra_config

        else:
            repo_dir = temp_dir / "repo"
            _setup_not_inplace(repo_dir)
            if marker.data_dir is not None:
                src = Path(os.environ["TEST_REPO_DATA"], marker.data_dir)
                _copytree(src, repo_dir)
                _setup_ovr_config(repo_dir)
                with open(Path(repo_dir, ".watchmanconfig"), "w") as f:
                    # Use the FS Events watcher, which is more reliable than the default.
                    json.dump(
                        {
                            "ignore_dirs": ["buck-out"],
                            "fsevents_watch_files": True,
                            "prefer_split_fsevents_watcher": False,
                        },
                        f,
                    )

            buck_cwd = repo_dir

        buck = Buck(
            test_executable_type,
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
            print(f"Not deleting temporary directory at {temp_dir}", file=sys.stderr)
        else:
            shutil.rmtree(temp_dir, ignore_errors=True)


@pytest.fixture(scope="function")
async def buck(request) -> AsyncIterator[Buck]:
    marker = request.node.get_closest_marker("buck_test")
    if marker is None:
        raise Exception("You must use buck_test() to use the buck fixture.")
    marker = marker.args[0]
    async with buck_fixture(marker) as buck:
        yield buck


async def _get_common_dir() -> Path:
    """
    Returns a temporary directory using mkscratch.
    The advantage of using mkscratch is that it can return the same directory on multiple calls.
    """
    mkscratch_proc = await subprocess.create_subprocess_exec(
        "mkscratch",
        "path",
        "--subdir",
        "buck_e2e",
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


def _setup_not_inplace(path: Path) -> None:
    if "PRELUDE" in os.environ:
        _copytree(
            Path(os.environ["PRELUDE"]),
            Path(path, "prelude"),
        )


def _setup_ovr_config(path: Path) -> None:
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
    inplace: bool,
    data_dir: Optional[str] = "",
    # Accepted values are specified in SKIPPABLE_PLATFORMS
    skip_for_os: List[str] = [],  # noqa: B006 value is read-only
    allow_soft_errors=False,
    extra_buck_config: Optional[Dict[str, Dict[str, str]]] = None,
    # Don't run a `buck2 kill` or `buck2 clean` at the end of the test
    skip_final_kill=False,
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
    """

    if inplace and data_dir == "":
        data_dir = None

    # Set up arguments to use for the buck fixture.

    # Just ignore the test, calling pytest.skip() is treated as a failure by tpx unfortunately
    for p in skip_for_os:
        if p not in SKIPPABLE_PLATFORMS:
            raise Exception(f"skip_for_os must specifiy one of {SKIPPABLE_PLATFORMS}")
    if platform.system().lower() in skip_for_os:
        return lambda *args: None

    if extra_buck_config is not None:
        if not inplace:
            raise Exception("extra_buck_config is only useful for inplace tests")

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
