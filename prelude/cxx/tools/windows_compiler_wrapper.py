#!/usr/bin/env python3
# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

import os
import shutil
import subprocess
import sys
import winreg


def find_windows_sdk_includes():
    registry = winreg.ConnectRegistry(None, winreg.HKEY_LOCAL_MACHINE)
    key_name = "SOFTWARE\\WOW6432Node\\Microsoft\\Microsoft SDKs\\Windows\\v10.0"
    registry_key = winreg.OpenKey(registry, key_name)
    installation_folder = winreg.QueryValueEx(registry_key, "InstallationFolder")[0]
    sdk_version = winreg.QueryValueEx(registry_key, "ProductVersion")[0]
    # Folder name has extra suffix.
    sdk_version += ".0"
    sdk_path = os.path.join(installation_folder, "Include", sdk_version)
    sdk_includes = []
    sdk_includes.append(os.path.join(sdk_path, "shared"))
    sdk_includes.append(os.path.join(sdk_path, "ucrt"))
    sdk_includes.append(os.path.join(sdk_path, "um"))
    sdk_includes.append(os.path.join(sdk_path, "winrt"))
    return sdk_includes


def find_msvc_includes(compiler):
    include_paths = []
    compiler_path = shutil.which(compiler)
    if compiler_path is None:
        raise FileNotFoundError("{} not found".format(compiler))
    parts = os.path.normpath(compiler_path).split(os.sep)
    msvc_include = os.sep.join(parts[:-4] + ["include"])
    assert os.path.isdir(msvc_include), "MSVC include directory is not found"
    include_paths.append(msvc_include)
    try:
        include_paths.extend(find_windows_sdk_includes())
    except FileNotFoundError:
        print("Windows SDK is not installed")
        sys.exit(1)
    return ["/I" + p for p in include_paths]


def main():
    arguments = sys.argv[1:]
    compiler_real = arguments[0]
    if compiler_real in ["cl.exe", "cl"]:
        arguments.extend(find_msvc_includes(compiler_real))
    subprocess.run(arguments, check=True)


if __name__ == "__main__":
    main()
