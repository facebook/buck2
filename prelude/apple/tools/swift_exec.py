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
import shlex
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
    modules = set()
    modules.add(_get_modulename(args))
    modulemap = _get_modulemap(args)
    if modulemap is None:
        return modules

    for entry in modulemap:
        # We only remove prefixes from first party modules.
        if "sdk_deps" in entry.get("clangModulePath", ""):
            pass
        elif "sdk_deps" in entry.get("modulePath", ""):
            pass
        else:
            modules.add(entry["moduleName"])

    return modules


def _remove_swiftinterface_module_prefixes(command):
    interface_path = command[command.index("-emit-module-interface-path") + 1]
    modules = _get_modules(command)
    pattern = re.compile(r"(\w+)\.([\w.]+)")

    def replace_module_prefixes(match):
        if match.group(1) in modules:
            return match.group(2)
        else:
            return match.group(0)

    with open(interface_path, "r+") as f:
        interface = f.read()
        f.seek(0)
        f.write(pattern.sub(replace_module_prefixes, interface))
        f.truncate()


def _rewrite_dependency_file(command):
    # The compiler will output d files in Makefile format with abolute paths,
    # Buck expects line separated relative paths with no input prefix.
    output_file_map_path = command[command.index("-output-file-map") + 1]
    with open(output_file_map_path) as f:
        output_file_map = json.load(f)

    deps_file_path = output_file_map[""]["dependencies"]
    with open(deps_file_path, encoding="utf-8") as f:
        for line in f:
            # We have multiple entries for the supplementary outputs of the
            # compilation action. We have two cases we care about:
            #  1. swiftmodule output
            #  2. object file output
            # Other supplemenatary outputs like swiftsourceinfo are not
            # tracked.
            output, inputs = line.split(" : ")
            if output.endswith(".swiftmodule") or output.endswith(".o"):
                dependencies = shlex.split(inputs)
                break

    # Sanity check that we only track relative paths
    for path in dependencies:
        if path.startswith("/"):
            print(f"Dependency file contains absolute path: {path}", file=sys.stderr)
            sys.exit(1)

    with open(deps_file_path, "w") as f:
        f.write("\n".join(sorted(dependencies)))
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

    should_ignore_errors = "-ignore-errors" in command
    if should_ignore_errors:
        command.remove("-ignore-errors")

    # Use relative paths for debug information and index information,
    # so we generate relocatable files.
    #
    # We need to use the path where the action is run (both locally and on RE),
    # which is not known when we define the action.
    command += [
        "-file-prefix-map",
        f"{os.getcwd()}/=",
        "-file-prefix-map",
        f"{os.getcwd()}=.",
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

    command = _process_skip_incremental_outputs(command)

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

        # Rewrite .d files for the format that Buck requires.
        if "-emit-dependencies" in command:
            _rewrite_dependency_file(command)

    # https://github.com/swiftlang/swift/issues/56573
    if should_remove_module_prefixes:
        _remove_swiftinterface_module_prefixes(command)

    if should_ignore_errors:
        sys.exit(0)
    else:
        sys.exit(result.returncode)


_SKIP_INCREMENTAL_OUTPUTS_ARG = "-skip-incremental-outputs"
_SWIFT_FILES_ARGSFILE = ".swift_files"


def _process_skip_incremental_outputs(command):
    if _SKIP_INCREMENTAL_OUTPUTS_ARG not in command:
        return command

    command.remove(_SKIP_INCREMENTAL_OUTPUTS_ARG)

    output_file_map = command[command.index("-output-file-map") + 1]
    output_dir = os.path.dirname(os.path.dirname(output_file_map))

    module_swiftdeps = (
        f"{output_dir}/__swift_incremental__/swiftdeps/module-build-record.priors"
    )
    output_file_map_content = {
        "": {
            "swift-dependencies": module_swiftdeps,
        },
    }

    swift_files = _get_swift_files_to_compile(command)
    for swift_file in swift_files:
        file_name = swift_file.split("/")[-1]
        output_artifact = f"{output_dir}/__swift_incremental__/objects/{file_name}.o"
        swiftdeps_artifact = (
            f"{output_dir}/__swift_incremental__/swiftdeps/{file_name}.swiftdeps"
        )
        output_file_map_content[swift_file] = {
            "object": output_artifact,
            "swift-dependencies": swiftdeps_artifact,
        }

    with open(output_file_map, "w") as f:
        json.dump(output_file_map_content, f, indent=2)

    return command


def _get_swift_files_to_compile(command):
    for arg in command:
        if arg.endswith(_SWIFT_FILES_ARGSFILE):
            path = arg[1:]  # Remove leading @
            with open(path) as swift_files:
                return [
                    line.rstrip().strip('"').strip("'")
                    for line in swift_files.readlines()
                ]

    raise Exception(f"No {_SWIFT_FILES_ARGSFILE} found!")


if __name__ == "__main__":
    main()
