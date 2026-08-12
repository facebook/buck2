#!/usr/bin/env python3
# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

# pyre-strict

import argparse
import json
from pathlib import Path
from typing import Any


def convert_type_check_result(input_path: Path, output_path: Path) -> None:
    """Convert a Python type-check result to Buck2's ValidationSpec format."""
    validation: dict[str, object] = {
        "version": 1,
        "data": {"status": "success"},
    }

    try:
        with input_path.open() as input_file:
            data: Any = json.load(input_file)
        blocking_errors = [
            error
            for error in data.get("errors", [])
            if error["code"] != 0
            and error.get("name") != "unused-ignore"
            and error.get("severity") not in ("info", "ignore", "warn")
        ]
        if blocking_errors:
            validation["data"] = {
                "status": "failure",
                "message": "\n".join(
                    f"{error['path']}:{error['line']}:{error['column']} {error['description']}"
                    for error in blocking_errors
                ),
            }
    except (OSError, json.JSONDecodeError, KeyError):
        validation["data"] = {
            "status": "failure",
            "message": "Failed to read type checker output",
        }

    with output_path.open("w") as output_file:
        json.dump(validation, output_file)


def main() -> None:
    parser = argparse.ArgumentParser()
    parser.add_argument("input", type=Path, help="Path to type_check_result.json")
    parser.add_argument(
        "--output",
        type=Path,
        required=True,
        help="Path to write ValidationSpec JSON",
    )
    args = parser.parse_args()
    convert_type_check_result(args.input, args.output)


if __name__ == "__main__":
    main()
