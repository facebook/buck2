#!/usr/bin/env python3
# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

"""
A simple script that accepts environment variable key-value pairs via --env flags
and writes them to an output file.

Each environment variable and its value are written on separate lines, with the
variable name followed by its value.

Examples:
    write_env_lines_action.py --env=USER=john --env=HOME=/home/john --outfile output.txt

    output.txt:
    ```
    USER
    john
    HOME
    /home/john
    
    ```
"""

import argparse
import os
import sys


def key_value_arg(s: str):
    """Parse a key=value argument."""
    key_value = s.split("=", 1)
    assert len(key_value) == 2, f"expected the form 'key=value' for '{s}'"
    return (key_value[0], key_value[1])


def parse_args():
    parser = argparse.ArgumentParser(
        fromfile_prefix_chars="@",
        description="Write environment variables to a file with each name and value on separate lines."
    )
    parser.add_argument(
        "--env",
        action="append",
        type=key_value_arg,
        metavar="NAME=VALUE",
        help="Environment variable in NAME=VALUE format",
        default=[]
    )
    parser.add_argument(
        "--outfile",
        required=True,
        help="Output file to write environment variables to"
    )

    return parser.parse_args()


def main():
    args = parse_args()

    outfile_dir = os.path.dirname(args.outfile)
    os.makedirs(outfile_dir, exist_ok=True)

    with open(args.outfile, "w") as f:
        for name, value in args.env:
            f.write(f"{name}\n{value}\n")

    return 0


if __name__ == "__main__":
    sys.exit(main())
