#!/usr/bin/env fbpython
# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

import argparse
import sys
from pathlib import Path
from typing import List


def _write_to_dep_file(dep_file: Path, files: List[Path]):
    dep_file_text = ""
    for file in files:
        assert isinstance(file, Path)
        file_str = str(file)
        # Check that backslashes in paths work on windows
        if sys.platform == "windows":
            assert "\\" in file_str
        dep_file_text += file_str + "\n"
    dep_file.write_text(dep_file_text)


# 1. Create a file at dep_file0 / dep_file1 (optional) which looks like "file1\nfile2\n..."
# 2. Write something to out so that it exists
def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("--used-files0", type=Path, required=True, nargs="*")
    parser.add_argument("--used-files1", type=Path, required=False, nargs="*")
    parser.add_argument("--out", type=Path, required=True)
    parser.add_argument("--dep-file0", type=Path, required=True)
    parser.add_argument("--dep-file1", type=Path, required=False, default=None)
    parser.add_argument("--fail", action="store_true")
    args = parser.parse_args()

    if args.fail:
        print("Failing on purpose", file=sys.stderr)
        sys.exit(1)

    # Just copy the contents of the inputs to the output
    all_used_files = args.used_files0 + (args.used_files1 or [])
    with args.out.open("a") as f:
        for used_file in all_used_files:
            f.write(Path(used_file).read_text())

    _write_to_dep_file(args.dep_file0, args.used_files0)

    if args.dep_file1:
        _write_to_dep_file(args.dep_file1, args.used_files1)


if __name__ == "__main__":
    main()
