#!/usr/bin/env python3
# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

import subprocess
import sys


def rewrite_dep_file(path):
    with open(path) as f:
        body = f.read()

    target, rest = body.split(": ", 1)

    deps = []
    while rest:
        line, rest = rest.split("\n", 1)
        line = line.rstrip("\\").strip()
        deps.append(line)

    with open(path, "w") as f:
        for line in deps:
            f.write(line)
            f.write("\n")


def main():
    subprocess.check_call(["gcc"] + sys.argv[2:])

    for idx in range(len(sys.argv)):
        if sys.argv[idx] == "-MF":
            rewrite_dep_file(sys.argv[idx + 1])


if __name__ == "__main__":
    main()
