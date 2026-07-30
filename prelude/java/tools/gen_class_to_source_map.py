# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

from __future__ import annotations

import argparse
import json
import os
import sys
import zipfile
from collections.abc import Iterable
from typing import TextIO


def _base_class_name_matches_base_source_path(
    base_class_name: str, base_source_path: str
) -> bool:
    return base_class_name == base_source_path or base_source_path.endswith(
        "/" + base_class_name
    )


def _source_stem(source_path: str) -> str:
    return os.path.splitext(os.path.basename(source_path))[0]


def _normalize_kotlin_file_facade(class_name: str, source_path: str | None) -> str:
    if source_path is None or not class_name.endswith("Kt"):
        return class_name

    simple_name = class_name.rsplit(".", 1)[-1]
    if simple_name[:-2] == _source_stem(source_path):
        return class_name[:-2]
    return class_name


def _legacy_source_for_class(class_name: str, sources: dict[str, str]) -> str | None:
    base_class_name = class_name.replace(".", "/")
    for source_base, source_path in sources.items():
        if _base_class_name_matches_base_source_path(base_class_name, source_base) or (
            base_class_name.endswith("Kt")
            and _base_class_name_matches_base_source_path(
                base_class_name[:-2], source_base
            )
        ):
            return source_path
    return None


def _jar_class_names(jar: str) -> list[str]:
    with zipfile.ZipFile(jar) as jar_file:
        return [
            entry[:-6].replace("/", ".")
            for entry in jar_file.namelist()
            if entry.endswith(".class")
        ]


def _build_class_entries(
    jar: str,
    source_paths: Iterable[str],
    include_classes_prefixes: Iterable[str],
) -> list[dict[str, str]]:
    legacy_sources = {os.path.splitext(source)[0]: source for source in source_paths}
    include_prefixes = tuple(include_classes_prefixes)
    entries = []

    for compiled_class in _jar_class_names(jar):
        if "$" in compiled_class:
            continue

        source_path = _legacy_source_for_class(compiled_class, legacy_sources)
        class_name = _normalize_kotlin_file_facade(compiled_class, source_path)
        if source_path is not None:
            entries.append({"className": class_name, "srcPath": source_path})
        elif compiled_class.startswith(include_prefixes):
            entries.append({"className": class_name})

    return entries


def _write_sources_jar(classes: Iterable[dict[str, str]], output: str) -> None:
    with zipfile.ZipFile(output, "w") as sources_jar:
        for entry in classes:
            source_path = entry.get("srcPath")
            if source_path is None:
                continue
            source_extension = os.path.splitext(source_path)[1]
            archive_path = entry["className"].replace(".", "/") + source_extension
            sources_jar.write(source_path, archive_path)


def generate_class_to_source_map(
    jar: str,
    source_paths: Iterable[str],
    output: TextIO,
    *,
    include_classes_prefixes: Iterable[str] = (),
    sources_jar: str | None = None,
) -> None:
    classes = _build_class_entries(jar, source_paths, include_classes_prefixes)
    if sources_jar is not None:
        _write_sources_jar(classes, sources_jar)

    json.dump({"jarPath": jar, "classes": classes}, output)
    output.write("\n")


def main(argv: list[str]) -> int:
    parser = argparse.ArgumentParser(fromfile_prefix_chars="@")
    parser.add_argument(
        "--include_classes_prefixes",
        "-i",
        default=[],
        nargs="*",
        help="Prefixes of classes to include in the output, even if their source isn't present",
    )
    parser.add_argument(
        "--output", "-o", type=argparse.FileType("w"), default=sys.stdin
    )
    parser.add_argument("--sources_jar", required=False)
    parser.add_argument("jar")
    parser.add_argument("sources", nargs="*")
    args = parser.parse_args(argv[1:])

    generate_class_to_source_map(
        args.jar,
        args.sources,
        args.output,
        include_classes_prefixes=args.include_classes_prefixes,
        sources_jar=args.sources_jar,
    )
    return 0


if __name__ == "__main__":
    sys.exit(main(sys.argv))
