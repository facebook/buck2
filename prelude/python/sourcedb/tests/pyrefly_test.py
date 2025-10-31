# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

import argparse
import dataclasses
import difflib
import json
import subprocess
import sys
from pathlib import Path
from typing import Any

EXPECTATIONS: Path = Path(__file__).parent / "pyrefly_expected.json"
DEFAULT_TEST_TARGET: str = "prelude//python/sourcedb/tests/test_files:main"
PATH_PREFIX: str = "prelude/python"


def strip_prefix(path: str) -> str:
    if PATH_PREFIX not in str(path):
        return path
    return PATH_PREFIX + str(path).split(PATH_PREFIX)[1]


@dataclasses.dataclass
class Alias:
    alias: str


@dataclasses.dataclass
class Manifest:
    srcs: dict[str, list[str]]
    deps: list[str]
    buildfile_path: str
    python_version: str
    python_platform: str

    def strip_root(self) -> None:
        for paths in self.srcs.values():
            old_paths = list(paths)
            paths.clear()
            for path in old_paths:
                paths.append(strip_prefix(path))

        self.buildfile_path = strip_prefix(self.buildfile_path)

    @staticmethod
    def from_json(json: Any) -> "Manifest":
        for key in (
            "srcs",
            "deps",
            "buildfile_path",
            "python_version",
            "python_platform",
        ):
            if key not in json:
                raise ValueError(
                    f"Unknown type for manifest, does not have '{key}' key. Keys are: "
                    + ", ".join(json.keys())
                )
        manifest = Manifest(**json)
        for paths in manifest.srcs.values():
            paths.sort()
        manifest.deps.sort()
        return manifest


@dataclasses.dataclass
class DB:
    db: dict[str, Manifest | Alias]
    root: str

    def strip_root(self) -> None:
        self.root = strip_prefix(self.root)

        for value in self.db.values():
            if not isinstance(value, Manifest):
                continue
            value.strip_root()

    @staticmethod
    def from_json(json: Any) -> "DB":
        for key in ("db", "root"):
            if key not in json:
                raise ValueError(
                    f"Unknown type returned from Buck, does not have '{key}' key. Keys are: "
                    + ", ".join(json.keys())
                )

        inner_db = {}

        for target, info in json["db"].items():
            if "alias" in info:
                inner_db[target] = Alias(alias=info["alias"])
            else:
                inner_db[target] = Manifest.from_json(info)

        db = DB(
            # strip root, since it's not relevant to these tests
            root="",
            db=inner_db,
        )

        db.strip_root()
        return db

    def to_json(self) -> str:
        return json.dumps(dataclasses.asdict(self), indent=2, sort_keys=True)


def query_buck(test_target: str) -> Any:
    result = subprocess.run(
        [
            "buck2",
            "bxl",
            "--reuse-current-config",
            "prelude//python/sourcedb/pyrefly.bxl:main",
            "--",
            "--target",
            test_target,
        ],
        capture_output=True,
    )

    stdout = result.stdout.decode("utf-8")
    if result.returncode != 0:
        stderr = result.stderr.decode("utf-8")
        raise Exception(
            f"Pyrefly execution with Buck2 resulted in non-zero exit: {result.returncode}\n"
            + f"STDOUT: ```\n{stdout}```\nSTDERR: ```\n{stderr}```\nCOMMAND: `{result.args}`"
        )

    return json.loads(stdout)


def diff_result(actual: DB, expected: DB, output: Path | None) -> None:
    if actual == expected:
        print("Congrats, you passed!")
        exit(0)

    actual_str = actual.to_json().split("\n")
    expected_str = expected.to_json().split("\n")

    diff = "\n".join(
        difflib.context_diff(
            actual_str, expected_str, fromfile="actual", tofile="expected"
        )
    )

    if output is None:
        print(diff)
    else:
        output.write_bytes(diff.encode("utf-8"))

    print("\nTest results differ...")
    sys.exit(1)


def get_expectations(expectations: Path) -> DB:
    text = expectations.read_bytes().decode("utf-8")
    return DB.from_json(json.loads(text))


def main() -> None:
    parser = argparse.ArgumentParser(
        description="Test `pyrefly.bxl` to ensure stability between changes.",
    )
    parser.add_argument(
        "--test-target",
        default=DEFAULT_TEST_TARGET,
        help="The target to generate output for and compare against expectations",
    )
    parser.add_argument(
        "--generate",
        action="store_true",
        help="Regenerate the expectations file and skip checking. Writes to `--expectations`",
    )
    parser.add_argument(
        "--output",
        help=(
            "Where to write the diff on failure. If no output is provided, writes to stdout"
        ),
        type=Path,
    )
    parser.add_argument(
        "--expectations",
        help="The expectations file to compare results against",
        type=Path,
        default=EXPECTATIONS,
    )
    args = parser.parse_args()

    actual = DB.from_json(query_buck(args.test_target))

    if args.generate:
        text = actual.to_json()
        args.expectations.write_bytes(text.encode("utf-8"))
        print("Generated!")
        sys.exit(0)

    expectations = get_expectations(args.expectations)

    diff_result(actual, expectations, args.output)


if __name__ == "__main__":
    main()
