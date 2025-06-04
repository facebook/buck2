#!/usr/bin/env python3
# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

"""
Python wrapper around Clang intended to optimize and codegen bitcode files
to native object files for distributed thin lto targeting darwin. This script
exists to work around Clang bugs where Clang will fail silently.
"""

import argparse
import os
import subprocess
import sys

from typing import List

EXIT_SUCCESS, EXIT_FAILURE = 0, 1


def main(argv: List[str]) -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument("--out", help="The output native object file.")
    parser.add_argument("--input", help="The input bitcode object file.")
    parser.add_argument("--index", help="The thinlto index file.")
    parser.add_argument(
        "--args", help="The argsfile containing unfiltered and unprocessed flags."
    )
    parser.add_argument("--compiler", help="The path to the Clang compiler binary.")
    parser.add_argument(
        "--print-command",
        action="store_true",
        help="Print the clang invocation and exit.",
    )
    args = parser.parse_args(argv[1:])

    clang_invocation = [args.compiler, f"@{args.args}"]
    clang_invocation.extend(
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

    if args.print_command:
        print(" ".join(clang_invocation))
        return EXIT_SUCCESS

    subprocess.check_call(clang_invocation)
    # Work around Clang bug where it fails silently: T187767815
    if os.stat(args.out).st_size == 0:
        print("error: opt produced empty file")
        return EXIT_FAILURE
    return EXIT_SUCCESS


if __name__ == "__main__":
    sys.exit(main(sys.argv))
