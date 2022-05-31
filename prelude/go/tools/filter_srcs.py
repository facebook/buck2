#!/usr/bin/env python3

"""
Run on a directory of Go source files and print out a list of srcs that should
be compiled.

Example:

 $ ./filter_srcs.py --output srcs.txt src/dir/

"""

# pyre-unsafe

import argparse
import json
import os
import subprocess
import sys
from pathlib import Path


def main(argv):
    parser = argparse.ArgumentParser()
    parser.add_argument("--go", default="go", type=Path)
    parser.add_argument("--tests", action="store_true")
    parser.add_argument("--output", type=argparse.FileType("w"), default=sys.stdout)
    parser.add_argument("srcdir", type=Path)
    args = parser.parse_args(argv[1:])

    # Run `go list` to filter input sources by build pragmas.
    out = subprocess.check_output(
        [
            "env",
            "-i",
            # TODO: Do we need to add e.g. GOOS/GOARCH?
            "GO111MODULE=off",
            "GOCACHE=/tmp",
            args.go.resolve(),
            "list",
            "-e",
            "-json",
            "./...",
        ],
        cwd=args.srcdir,
    ).decode("utf-8")

    # Parse JSON output and print out sources.
    idx = 0
    decoder = json.JSONDecoder()
    while idx < len(out) - 1:
        # The raw_decode method fails if there are any leading spaces, e.g. " {}" fails
        # so manually trim the prefix of the string
        if out[idx].isspace():
            idx += 1
            continue

        obj, idx = decoder.raw_decode(out, idx)
        if args.tests:
            types = ["GoFiles", "TestGoFiles", "XTestGoFiles"]
        else:
            types = ["GoFiles", "SFiles"]
        for typ in types:
            for src in obj.get(typ, []):
                src = Path(obj["Dir"]) / src
                src = src.resolve().relative_to(os.getcwd())
                print(src, file=args.output)


sys.exit(main(sys.argv))
