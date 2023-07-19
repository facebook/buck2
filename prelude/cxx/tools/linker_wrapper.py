#!/usr/bin/env python3
# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

import codecs
import os
import shutil
import subprocess
import sys
import tempfile
import winreg


def unquote(argument):
    if len(argument) > 2 and argument[0] == '"' and argument[-1] == '"':
        return argument[1:-1]
    else:
        return argument


def is_relative_buck_out_path(argument):
    return argument.startswith("buck-out\\")


def is_library_path(argument):
    return argument.endswith(".lib")


def is_whole_archive_option(argument):
    return argument.startswith("/WHOLEARCHIVE")


def expand_library_path(argument, full_library_paths):
    library_name = argument.split(":", maxsplit=1)[-1]
    return "/WHOLEARCHIVE:" + full_library_paths[library_name]


# NOTE: it does not check for cycles
def expand_args(arguments):
    expanded = []
    for argument in arguments:
        unquoted_argument = unquote(argument)
        if unquoted_argument.startswith("@"):
            filename = unquoted_argument[1:]
            with open(filename, mode="rb") as f:
                file_content = f.read()
            if file_content.startswith(codecs.BOM_UTF16):
                string = file_content.decode("utf-16")
            else:
                string = file_content.decode("utf-8")
            expanded.extend(expand_args(string.splitlines()))
        else:
            expanded.append(argument)
    return expanded


def find_windows_sdk(arch):
    registry = winreg.ConnectRegistry(None, winreg.HKEY_LOCAL_MACHINE)
    key_name = "SOFTWARE\\WOW6432Node\\Microsoft\\Microsoft SDKs\\Windows\\v10.0"
    registry_key = winreg.OpenKey(registry, key_name)
    installation_folder = winreg.QueryValueEx(registry_key, "InstallationFolder")[0]
    sdk_version = winreg.QueryValueEx(registry_key, "ProductVersion")[0]
    # Folder name has extra suffix.
    sdk_version += ".0"
    sdk_path = os.path.join(installation_folder, "Lib", sdk_version)
    sdk_libpath = []
    sdk_libpath.append(os.path.join(sdk_path, "ucrt", arch))
    sdk_libpath.append(os.path.join(sdk_path, "um", arch))
    return sdk_libpath


def find_msvc_libpath(linker):
    libpath = []
    linker_path = shutil.which(linker)
    if linker_path is None:
        raise FileNotFoundError("{} not found".format(linker))
    parts = os.path.normpath(linker_path).split(os.sep)
    arch = parts[-2]
    assert arch in ["x86", "x64"], "Unsupported MSVC setup"
    libpath.append(os.sep.join(parts[:-4] + ["lib", arch]))
    try:
        libpath.extend(find_windows_sdk(arch))
    except FileNotFoundError:
        print("Windows SDK is not installed")
        sys.exit(1)
    return ["/LIBPATH:" + p for p in libpath]


def main():
    linker_real, rest = sys.argv[1], sys.argv[2:]
    working_dir = os.getcwd()

    arguments = expand_args(rest)

    full_library_paths = {}
    new_args = []
    for argument in arguments:
        argument = unquote(argument)

        # Save library paths to support expanding them afterwards
        if is_library_path(argument):
            library_name = os.path.basename(argument)
            full_library_paths[library_name] = argument

        # LLVM linker expects full path in /WHOLEARCHIVE option while MSVC
        # expects just the filename. That's why we need to expand the path.
        if is_whole_archive_option(argument):
            argument = expand_library_path(argument, full_library_paths)

        # Make relative paths absolute for support paths longer than 260 chars.
        if is_relative_buck_out_path(argument):
            argument = os.path.join(working_dir, argument)

        new_args.append(argument)

    # Dynamically find LIBPATH for the current linker from system toolchain.
    if linker_real in ["link.exe", "link"]:
        new_args.extend(find_msvc_libpath(linker_real))

    # Based on rustc's @linker-arguments file construction.
    # https://github.com/rust-lang/rust/blob/1.69.0/compiler/rustc_codegen_ssa/src/back/link.rs#L1383-L1407
    quoted_new_args = ('"{}"\n'.format(arg) for arg in new_args)
    new_args_content = "".join(quoted_new_args).encode("utf-16")

    debug = "[\n" + "".join('    "{}"\n'.format(arg) for arg in new_args) + "]"
    print(f"linker_args = {debug}", file=sys.stderr)

    with tempfile.NamedTemporaryFile(mode="wb", delete=False) as new_args_file:
        new_args_file.write(new_args_content)
        new_args_file_name = new_args_file.name

    command = [linker_real, "@" + new_args_file_name]
    subprocess.run(command, check=True)


if __name__ == "__main__":
    main()
