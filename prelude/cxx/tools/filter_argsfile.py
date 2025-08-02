#!/usr/bin/env python3
# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

import re
import sys


def main():
    if len(sys.argv) != 4:
        print(f"Usage: {sys.argv[0]} <pattern> <input_file> <output_file>")
        sys.exit(1)
    pattern = sys.argv[1]
    input_file = sys.argv[2]
    output_file = sys.argv[3]
    regex = re.compile(pattern)
    with open(input_file, "r") as infile, open(output_file, "w") as outfile:
        for line in infile:
            if not regex.search(line):
                outfile.write(line)


if __name__ == "__main__":
    main()
