#!/usr/bin/env python3
# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

import json
import os
import re
import subprocess
import sys

_RE_TMPDIR_ENV_VAR = "TMPDIR"
_FILE_WRITE_FAILURE_MARKER = "could not write"


def _expand_args(command):
    for arg in command:
        if arg.startswith("@"):
            with open(arg[1:]) as argsfile:
                return [line.rstrip() for line in argsfile.readlines()]

    return []


def _get_modulemap(args):
    for i in range(len(args)):
        if args[i].startswith("-explicit-swift-module-map-file"):
            if args[i + 1].startswith("-Xfrontend"):
                path = args[i + 2]
            else:
                path = args[i + 1]

            with open(path.strip()) as modulemap:
                return json.load(modulemap)

    return None


def _get_modulename(args):
    return args[args.index("-module-name") + 1]


def _get_modules(command):
    args = _expand_args(command)
    modulemap = _get_modulemap(args)
    if modulemap is None:
        return set()

    modules = set()
    for entry in modulemap:
        # We only remove prefixes from first party modules.
        if "sdk_deps" in entry.get("clangModulePath", ""):
            pass
        elif "sdk_deps" in entry.get("modulePath", ""):
            pass
        else:
            modules.add(entry["moduleName"])

    modules.add(_get_modulename(args))

    return modules


def _remove_swiftinterface_module_prefixes(command):
    interface_path = command[command.index("-emit-module-interface-path") + 1]
    modules = _get_modules(command)
    if len(modules) == 0:
        return

    with open(interface_path) as f:
        interface = f.read()

    output = []
    pattern = re.compile(r"(\w+)\.(\w+)")
    for line in interface.splitlines():
        outline = line
        for m in pattern.finditer(line):
            if m.group(1) in modules:
                outline = outline.replace(m.group(0), m.group(2))

        output.append(outline)

    with open(interface_path, "w") as f:
        f.write("\n".join(output))
        f.write("\n")


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

    # Check if we need to strip module prefixes from types in swiftinterface output.
    should_remove_module_prefixes = "-remove-module-prefixes" in command
    if should_remove_module_prefixes:
        command.remove("-remove-module-prefixes")

    # Use relative paths for debug information and index information,
    # so we generate relocatable files.
    #
    # We need to use the path where the action is run (both locally and on RE),
    # which is not known when we define the action.
    command += [
        "-file-prefix-map",
        f"{os.getcwd()}/=",
    ]

    # Apply a coverage prefix map for the current directory
    # to make file path metadata relocatable stripping
    # the current directory from it.
    #
    # This overrides -file-prefix-map.
    command += [
        "-coverage-prefix-map",
        f"{os.getcwd()}=.",
    ]

    result = subprocess.run(
        command,
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
        encoding=sys.stdout.encoding,
        env=env,
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

    # https://github.com/swiftlang/swift/issues/56573
    if should_remove_module_prefixes:
        _remove_swiftinterface_module_prefixes(command)

    sys.exit(result.returncode)


if __name__ == "__main__":
    main()
