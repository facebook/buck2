#!/usr/bin/env python3
# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

"""
A simple script that accepts environment variable key-value pairs via --env and --path-env flags
and writes them to an output file in the original command format.

Examples:
    write_env_flags_action.py --env=USER=john --path-env=HOME=/home/john --outfile output.txt

    output.txt:
    ```
    --env=USER=john
    --path-env=HOME=/home/john
    
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
        description="Write command flags to a file in their original format."
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
        "--path-env",
        action="append",
        type=key_value_arg,
        metavar="NAME=VALUE",
        help="Path environment variable in NAME=VALUE format",
        default=[]
    )
    parser.add_argument(
        "--outfile",
        required=True,
        help="Output file to write commands to"
    )

    return parser.parse_args()


def main():
    args = parse_args()

    outfile_dir = os.path.dirname(args.outfile)
    if outfile_dir:
        os.makedirs(outfile_dir, exist_ok=True)

    with open(args.outfile, "w") as f:
        for name, value in args.path_env:
            f.write(f"--path-env={name}={value}\n")
        for name, value in args.env:
            f.write(f"--env={name}={value}\n")

    return 0


if __name__ == "__main__":
    sys.exit(main())
