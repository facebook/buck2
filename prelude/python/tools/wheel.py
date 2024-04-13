# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

# pyre-strict

import argparse
import configparser
import contextlib
import io
import json
import os
import sys
import zipfile
from types import TracebackType
from typing import cast, Dict, List, Optional, Set, Type


# pyre-fixme[24]: Generic type `AbstractContextManager` expects 1 type parameter.
class WheelBuilder(contextlib.AbstractContextManager):

    def __init__(
        self,
        *,
        name: str,
        version: str,
        output: str,
        entry_points: Optional[Dict[str, str]] = None,
        metadata: Optional[Dict[str, str]] = None,
    ) -> None:
        self._name = name
        self._version = version
        self._record: list[str] = []
        self._outf = zipfile.ZipFile(output, mode="w")
        self._entry_points: Optional[Dict[str, str]] = entry_points
        self._metadata: Dict[str, str] = {}
        self._metadata["Name"] = name
        self._metadata["Version"] = version
        if metadata is not None:
            self._metadata.update(metadata)

    def _dist_info(self, *path: str) -> str:
        return os.path.join(f"{self._name}-{self._version}.dist-info", *path)

    def write(self, dst: str, src: str) -> None:
        self._record.append(dst)
        self._outf.write(filename=src, arcname=dst)

    def writestr(self, dst: str, contents: str) -> None:
        self._record.append(dst)
        self._outf.writestr(zinfo_or_arcname=dst, data=contents)

    def _write_record(self) -> None:
        record = self._dist_info("RECORD")
        self._outf.writestr(
            record, "".join(["{},,\n".format(f) for f in (self._record + [record])])
        )

    def close(self) -> None:
        self.writestr(
            self._dist_info("METADATA"),
            "".join(
                ["{}: {}\n".format(key, val) for key, val in self._metadata.items()]
            ),
        )
        self.writestr(
            self._dist_info("WHEEL"),
            """\
Wheel-Version: 1.0
""",
        )

        # Write entry points.
        if self._entry_points is not None:
            config = configparser.ConfigParser()
            config.read_dict(cast(Dict[str, Dict[str, str]], self._entry_points))
            with io.TextIOWrapper(
                self._outf.open(self._dist_info("entry_points.txt"), mode="w"),
                encoding="utf-8",
            ) as f:
                config.write(f)

        self._write_record()
        self._outf.close()

    def __exit__(
        self,
        exc_type: Optional[Type[BaseException]],
        exc_value: Optional[BaseException],
        exc_tb: Optional[TracebackType],
    ) -> None:
        self.close()


def main(argv: List[str]) -> None:
    parser = argparse.ArgumentParser()
    parser.add_argument("--output", required=True)
    parser.add_argument("--name", required=True)
    parser.add_argument("--version", required=True)
    parser.add_argument("--entry-points", default=None)
    parser.add_argument("--srcs", action="append", default=[])
    parser.add_argument("--metadata", action="append", default=[])
    args = parser.parse_args(argv[1:])

    pkgs: Set[str] = set()
    pkgs_with_init = set()

    def _add_pkg(pkg: str) -> None:
        pkgs.add(pkg)
        parent = os.path.dirname(pkg)
        if parent:
            _add_pkg(parent)

    with WheelBuilder(
        name=args.name,
        version=args.version,
        output=args.output,
        entry_points=(
            json.loads(args.entry_points) if args.entry_points is not None else None
        ),
        metadata=dict([m.split(":", 1) for m in args.metadata]),
    ) as whl:
        for src in args.srcs:
            with open(src) as f:
                manifest = json.load(f)
            for dst, src, *_ in manifest:
                if dst.endswith((".py", ".so")):
                    pkg = os.path.dirname(dst)
                    _add_pkg(pkg)
                    if os.path.basename(dst) == "__init__.py":
                        pkgs_with_init.add(pkg)
                whl.write(dst, src)

        for pkg in pkgs - pkgs_with_init:
            whl.writestr(os.path.join(pkg, "__init__.py"), "")


sys.exit(main(sys.argv))
