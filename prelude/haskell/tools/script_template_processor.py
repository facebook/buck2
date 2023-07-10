#!/usr/bin/env python3
# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.


"""
Helper script to generate scripts required by haskell tools from script
template files.

These script templates require GHC and/or toolchain values, e.g. GHC flags used,
paths to cxx tools.

The macros in the script should be in the format `<MACRO_NAME>`.
For example, `echo <binutils_path>` is an expression that would be replaced.
"""

# pyre-unsafe

import argparse
import os
import re
import sys
from typing import Dict, List

# Paths in the toolchain that are passed relative to the project's cell and
# need to be canonicalized before being replaced in the script.
TOOLCHAIN_PATHS = [
    "binutils_path",
    "ghci_lib_path",
    "cc_path",
    "cpp_path",
    "cxx_path",
]


def _replace_template_values(
    template_file,
    output_fpath: str,
    user_ghci_path: str,
    rel_toolchain_paths: Dict[str, str],
    exposed_packages: List[str],
    package_dbs: str,
    prebuilt_package_dbs: str,
):
    script_template = template_file.read()

    for macro_name, rel_path in rel_toolchain_paths.items():
        canonical_path = os.path.realpath(rel_path)

        script_template = re.sub(
            pattern=f"<{macro_name}>",
            repl=canonical_path,
            string=script_template,
        )

    # user_ghci_path has to be handled separately because it needs to be passed
    # with the ghci_lib_path as the `-B` argument.
    ghci_lib_canonical_path = os.path.realpath(
        rel_toolchain_paths["ghci_lib_path"],
    )
    script_template = re.sub(
        pattern="<user_ghci_path>",
        repl="${{DIR}}/{user_ghci_path} -B{ghci_lib_path}".format(
            user_ghci_path=user_ghci_path,
            ghci_lib_path=ghci_lib_canonical_path,
        ),
        string=script_template,
    )

    script_template = re.sub(
        pattern="<exposed_packages>",
        repl=" ".join(exposed_packages),
        string=script_template,
    )

    prebuilt_pkgs_canonical_paths = [
        os.path.realpath(
            rel_path,
        )
        for rel_path in prebuilt_package_dbs.split(" ")
    ]

    prebuilt_package_dbs_args = " ".join(
        ["-package-db {}".format(pkg) for pkg in prebuilt_pkgs_canonical_paths]
    )

    package_dbs_args = " ".join(
        ["-package-db {}".format(pkg) for pkg in package_dbs.split(" ")]
    )
    script_template = re.sub(
        pattern="<package_dbs>",
        repl=" ".join([prebuilt_package_dbs_args, package_dbs_args]),
        string=script_template,
    )

    with open(output_fpath, "w") as output_file:
        output_file.write(script_template)

    os.chmod(output_fpath, 0o777)  # Script should be executable


def main() -> int:
    parser = argparse.ArgumentParser()

    parser.add_argument(
        "--script-template",
        type=argparse.FileType("r"),
    )

    for tpath in TOOLCHAIN_PATHS:
        parser.add_argument(
            "--{}".format(tpath),
        )

    parser.add_argument(
        "--user_ghci_path",
    )

    parser.add_argument(
        "--exposed_packages",
        type=str,
        nargs="+",
    )
    parser.add_argument(
        "--package_dbs",
        type=str,
    )
    parser.add_argument(
        "--prebuilt_package_dbs",
        type=str,
    )

    parser.add_argument(
        "--output",
    )
    args = parser.parse_args()

    rel_toolchain_paths = {}
    for tpath in TOOLCHAIN_PATHS:
        rel_toolchain_paths[tpath] = getattr(args, tpath)

    _replace_template_values(
        args.script_template,
        args.output,
        args.user_ghci_path,
        rel_toolchain_paths,
        args.exposed_packages,
        args.package_dbs,
        args.prebuilt_package_dbs,
    )

    return 0


sys.exit(main())
