#!/usr/bin/env python3
# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

# Simple python script that symlinks a provided path to the only file in a directory

import os
import sys


def main():
    d = sys.argv[1]
    linkname = sys.argv[2]
    entries = os.listdir(d)
    if len(entries) != 1:
        raise ValueError("expected exactly one entry in directory")
    dest = os.path.join(d, entries[0])

    symlink = os.path.relpath(dest, start=os.path.dirname(linkname))
    os.symlink(symlink, linkname)


if __name__ == "__main__":
    main()
