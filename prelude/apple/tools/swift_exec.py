#!/usr/bin/env python3
# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

import os
import subprocess
import sys

_RE_TMPDIR_ENV_VAR = "TMPDIR"
_FILE_WRITE_FAILURE_MARKER = "could not write"


def main():
    env = os.environ.copy()
    if "INSIDE_RE_WORKER" in env and _RE_TMPDIR_ENV_VAR in env:
        # Use $TMPDIR for the module cache location. This
        # will be set to a unique location for each RE action
        # which will avoid sharing modules across RE actions.
        # This is necessary as the inputs to the modules will
        # be transient and can be removed at any point, causing
        # module validation errors to fail builds.
        # https://github.com/llvm/llvm-project/blob/main/clang/lib/Driver/ToolChains/Clang.cpp#L3709
        env["CLANG_MODULE_CACHE_PATH"] = os.path.join(
            env[_RE_TMPDIR_ENV_VAR], "buck-module-cache"
        )
    else:
        # For local actions use a shared module cache location.
        # This should be safe to share across the other local
        # compilation actions.
        env["CLANG_MODULE_CACHE_PATH"] = "/tmp/buck-module-cache"

    command = sys.argv[1:]
    # Apply a debug prefix map for the current directory
    # to make debug info relocatable. To correctly make paths
    # relocatable, we must use that path at which the action
    # is run (be it locally or on RE) and this is not known
    # at the time of action definition.
    command += [
        "-debug-prefix-map",
        f"{os.getcwd()}/=",
    ]
    # Apply a coverage prefix map for the current directory
    # to make file path metadata relocatable stripping
    # the current directory from it.
    command += [
        "-coverage-prefix-map",
        f"{os.getcwd()}=.",
    ]

    result = subprocess.run(
        command,
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
        encoding=sys.stdout.encoding,
    )

    print(result.stdout, file=sys.stdout, end="")
    print(result.stderr, file=sys.stderr, end="")

    if result.returncode == 0:
        # The Swift compiler will return an exit code of 0 and warn when it cannot write auxiliary files.
        # Detect and error so that the action is not cached.
        failed_write = (
            _FILE_WRITE_FAILURE_MARKER in result.stdout
            or _FILE_WRITE_FAILURE_MARKER in result.stderr
        )
        if failed_write:
            print(
                "Detected Swift compiler file write error but compiler exited with code 0, failing command..."
            )
            sys.exit(1)

    sys.exit(result.returncode)


if __name__ == "__main__":
    main()
