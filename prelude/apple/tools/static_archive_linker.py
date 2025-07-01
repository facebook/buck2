# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

import argparse
import subprocess
import sys


def _filter_files(args):
    # Filter out any arg that starts with '-'.  Also filter out passed arg
    # pairs like '-framework Cocoa'. Assumes that other args are files that
    # should be passed to libtool.
    # Rationale: There's no way of checking if an arg is intended to be a file.
    # Having a filter that checks if a file with a given path exists will
    # silently remove the arg in the event of an error where arg is a file and
    # the file does not exist.
    filtered_args = []
    skip_next = False
    for arg in args:
        if skip_next:
            skip_next = False
            continue
        if arg.startswith("-"):
            if arg == "-framework":
                skip_next = True
            continue
        filtered_args.append(arg)
    return filtered_args


def _main() -> None:
    parser = argparse.ArgumentParser(
        description="""
            Tool which accepts a list of link argments and passes only the
            files to `libtool` to create a static archive of only those
            files for distribuion.
        """
    )
    # the libtool executable that is passed in can be mulitple args
    # i.e. it can be a run.sh wrapper followed by a supplementary arg
    # hence nargs="+"
    parser.add_argument(
        "--libtool", nargs="+", action="extend", required=True, help="path to libtool"
    )
    parser.add_argument("--output", required=True, help="path to output")
    parser.add_argument("args", nargs=argparse.REMAINDER)

    args = parser.parse_args()
    file_args = _filter_files(args.args)
    result = subprocess.run(
        args.libtool + ["-static", "-o", args.output] + file_args,
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
        encoding=sys.stdout.encoding,
    )
    print(result.stdout, file=sys.stdout, end="")
    print(result.stderr, file=sys.stderr, end="")
    sys.exit(result.returncode)


if __name__ == "__main__":
    _main()
