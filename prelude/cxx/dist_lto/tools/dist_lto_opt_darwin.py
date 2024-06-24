#!/usr/bin/env python3
# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

"""
Python wrapper around clang intended to optimize and codegen bitcode files
to native object files for distributed thin lto. This script munges compiler
flags to prepare a suitable clang invocation.
"""

import argparse
import subprocess
import sys

from typing import List


def main(argv: List[str]) -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument("--out", help="The output native object file.")
    parser.add_argument("--input", help="The input bitcode object file.")
    parser.add_argument("--index", help="The thinlto index file.")
    # Split dwarf isn't applicable to Darwin, ignore the flag
    parser.add_argument("--split-dwarf", required=False, help="Split dwarf option.")
    parser.add_argument(
        "--args", help="The argsfile containing unfiltered and unprocessed flags."
    )
    parser.add_argument("opt_args", nargs=argparse.REMAINDER)
    args = parser.parse_args(argv[1:])

    with open(args.args, "r") as argsfile:
        clang_opt_flags = argsfile.read().splitlines()

    clang_opt_flags.extend(
        [
            "-o",
            args.out,
            "-x",
            "ir",  # Without this the input file type is incorrectly inferred.
            "-c",
            args.input,
            f"-fthinlto-index={args.index}",
            # When lto_mode=thin/full all compile actions are passed `-flto=thin/full`. We
            # want to generate a native object file here.
            "-fno-lto",
            "-Werror=unused-command-line-argument",
        ]
    )

    # TODO(T187767988) - Check if returning the subprocesses exit code is sufficient. Server LLVM created such a wrapper
    # script in the first place because of a bug in Clang where it fails but does not set a non-zero exit code (T116695431). Fbcode's
    # version of this script measure the size of the output file to determine success. The task is closed, but if the bug
    # still persists, we may need to do the same.
    result = subprocess.run(clang_opt_flags)
    return result.returncode


if __name__ == "__main__":
    sys.exit(main(sys.argv))
