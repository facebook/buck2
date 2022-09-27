#!/usr/bin/env python3
# (c) Facebook, Inc. and its affiliates. Confidential and proprietary.

import os
import sys
from pathlib import Path


def _print_file_contents(file):
    print("===BEGIN")
    print(Path(file).read_text().strip())
    print("=====END")


if __name__ == "__main__":
    if len(sys.argv) != 2:
        print("Pass single argument to print its tree structure", file=sys.stderr)
        sys.exit(1)

    argument = sys.argv[1]
    if os.path.isfile(argument):
        print("it's a file")
        _print_file_contents(argument)
    elif os.path.isdir(argument):
        print("it's a dir", file=sys.stdout)
        for root, dirs, files in os.walk(argument):
            # force stable order
            dirs.sort()
            for file_name in sorted(files):
                file_path = os.path.join(root, file_name)
                print("========")
                print(os.path.relpath(file_path, argument))
                _print_file_contents(file_path)
    else:
        print("path do not exist", file=sys.stdout)
