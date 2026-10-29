#!/usr/bin/env fbpython
# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

from __future__ import annotations

import argparse
import sys

from .dsym_allowlist import kept_debug_file_paths, write_dsymutil_allowlist


def _parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(
        description="Writes a `dsymutil --allow` file listing the debug map entries of an executable/dylib that the scrubber would keep. The input is not modified."
    )
    parser.add_argument(
        "--input",
        required=True,
        help="Path to the input which is an executable/dylib file.",
    )
    parser.add_argument(
        "--output", required=True, help="Path to the YAML file to write."
    )
    group = parser.add_mutually_exclusive_group(required=True)
    group.add_argument(
        "--targets-file",
        help="Path to a json file which contains user-focused Buck targets",
    )
    group.add_argument(
        "--spec-file",
        help="Path to a json file which contains user-focused include/exclude specs",
    )
    parser.add_argument(
        "--persisted-targets-file",
        help="A JSON file with additional targets that must be preserved by the scrubber.",
    )
    return parser.parse_args()


def main() -> None:
    args = _parse_args()
    try:
        paths = kept_debug_file_paths(
            input_file=args.input,
            persisted_targets_file=args.persisted_targets_file,
            targets_file=args.targets_file,
            spec_file=args.spec_file,
        )
        write_dsymutil_allowlist(paths, args.output)
    except Exception as e:
        print(f"Focused debugging failed: {e}", file=sys.stderr)
        sys.exit(1)


if __name__ == "__main__":
    main()
