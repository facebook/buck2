#!/usr/bin/env python3
# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

"""
Usage: concatenate_diagnostics.py --out path/to/output.txt [path/to/input.txt...]
"""

import argparse
from pathlib import Path
from typing import List, NamedTuple


class Args(NamedTuple):
    out: Path
    subtarget_diagnostics: List[Path]


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("--out", type=Path, required=True)
    parser.add_argument("subtarget_diagnostics", nargs="*", type=Path)
    args = Args(**vars(parser.parse_args()))

    needs_blank_line = False
    with open(args.out, "wb") as out:
        for f in args.subtarget_diagnostics:
            with open(f, "rb") as f:
                content = f.read()
            if len(content) == 0:
                continue
            if needs_blank_line:
                out.write(b"\n")
            out.write(content)
            needs_blank_line = True


if __name__ == "__main__":
    main()
