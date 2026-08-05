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
from collections import defaultdict
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

    # A Kotlin file facade (`Foo.kt` -> class `FooKt`) is renamed back to the file
    # name, but a real class literally named `FooKt` (from `FooKt.kt`) is left alone.
    simple_name = class_name.rsplit(".", 1)[-1]
    if simple_name[:-2] == _source_stem(source_path):
        return class_name[:-2]
    return class_name


def _load_debug_class_to_source_map(path: str | None) -> dict[str, str]:
    if path is None:
        return {}

    with open(path) as debuginfo_file:
        source_files = json.load(debuginfo_file)

    class_to_sources: defaultdict[str, set[str]] = defaultdict(set)
    for source_file in source_files:
        for class_info in source_file["classes"]:
            class_to_sources[class_info["name"]].add(source_file["file_path"])

    unique_class_to_source: dict[str, str] = {}
    for class_name, source_paths in class_to_sources.items():
        # Never guess which source owns a class claimed by multiple files.
        if len(source_paths) != 1:
            continue
        unique_class_to_source[class_name] = next(iter(source_paths))
    return unique_class_to_source


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


def _resolve_source_path(
    compiled_class: str,
    top_level_class: str,
    debug_class_to_source: dict[str, str],
    legacy_sources: dict[str, str],
) -> str | None:
    # Debug records are authoritative. Legacy matching keeps toolchains without
    # debug support, and classes missing from otherwise valid debug data, working.
    return (
        debug_class_to_source.get(compiled_class)
        or debug_class_to_source.get(top_level_class)
        or _legacy_source_for_class(top_level_class, legacy_sources)
    )


def _source_identity(class_name: str, source_path: str) -> str:
    package, _, _ = class_name.rpartition(".")
    source_stem = _source_stem(source_path)
    return package + "." + source_stem if package else source_stem


def _jar_class_names(jar: str) -> list[str]:
    with zipfile.ZipFile(jar) as jar_file:
        return [
            entry[:-6].replace("/", ".")
            for entry in jar_file.namelist()
            if entry.endswith(".class")
        ]


def _add_source_identity_aliases(entries: dict[str, dict[str, str]]) -> None:
    # Android aggregate coverage identifies a source by package and file stem.
    # Preserve a real class mapping if it has the same name as that identity.
    for entry in list(entries.values()):
        source_path = entry.get("srcPath")
        if source_path is None:
            continue
        identity = _source_identity(entry["className"], source_path)
        entries.setdefault(identity, {"className": identity, "srcPath": source_path})


def _build_class_entries(
    jar: str,
    source_paths: Iterable[str],
    debuginfo: str | None,
    include_classes_prefixes: Iterable[str],
) -> list[dict[str, str]]:
    legacy_sources = {os.path.splitext(source)[0]: source for source in source_paths}
    debug_class_to_source = _load_debug_class_to_source_map(debuginfo)
    include_prefixes = tuple(include_classes_prefixes)
    entries: dict[str, dict[str, str]] = {}

    for compiled_class in _jar_class_names(jar):
        top_level_class = compiled_class.split("$", 1)[0]
        source_path = _resolve_source_path(
            compiled_class,
            top_level_class,
            debug_class_to_source,
            legacy_sources,
        )
        class_name = _normalize_kotlin_file_facade(top_level_class, source_path)
        existing_entry = entries.get(class_name)
        if source_path is not None:
            # Nested and synthetic classes collapse to their top-level class. Prefer
            # a sourceful mapping regardless of their order in the JAR.
            if existing_entry is None or "srcPath" not in existing_entry:
                entries[class_name] = {
                    "className": class_name,
                    "srcPath": source_path,
                }
        elif (
            existing_entry is None
            and "$" not in compiled_class
            and compiled_class.startswith(include_prefixes)
        ):
            entries[class_name] = {"className": class_name}

    _add_source_identity_aliases(entries)

    return list(entries.values())


def _write_sources_jar(classes: Iterable[dict[str, str]], output: str) -> None:
    # Several classes can share one source file, so entries are keyed by source path
    # and named after the file rather than the class to avoid source duplication.
    written_sources: set[str] = set()
    with zipfile.ZipFile(output, "w") as sources_jar:
        for entry in classes:
            source_path = entry.get("srcPath")
            if source_path is None or source_path in written_sources:
                continue
            written_sources.add(source_path)
            package, _, _ = entry["className"].rpartition(".")
            package_dir = package.replace(".", "/")
            source_name = os.path.basename(source_path)
            archive_path = (
                package_dir + "/" + source_name if package_dir else source_name
            )
            sources_jar.write(source_path, archive_path)


def generate_class_to_source_map(
    jar: str,
    source_paths: Iterable[str],
    output: TextIO,
    *,
    debuginfo: str | None = None,
    include_classes_prefixes: Iterable[str] = (),
    owner_target: str | None = None,
    sources_jar: str | None = None,
) -> None:
    classes = _build_class_entries(
        jar,
        source_paths,
        debuginfo,
        include_classes_prefixes,
    )
    if sources_jar is not None:
        _write_sources_jar(classes, sources_jar)

    result = {"jarPath": jar, "classes": classes}
    if owner_target is not None:
        result["ownerTarget"] = owner_target
    json.dump(result, output)
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
        "--output", "-o", type=argparse.FileType("w"), default=sys.stdout
    )
    parser.add_argument("--sources_jar", required=False)
    parser.add_argument("--debuginfo", required=False)
    parser.add_argument("--owner-target", required=False)
    parser.add_argument("jar")
    parser.add_argument("sources", nargs="*")
    args = parser.parse_args(argv[1:])

    generate_class_to_source_map(
        args.jar,
        args.sources,
        args.output,
        debuginfo=args.debuginfo,
        include_classes_prefixes=args.include_classes_prefixes,
        owner_target=args.owner_target,
        sources_jar=args.sources_jar,
    )
    return 0


if __name__ == "__main__":
    sys.exit(main(sys.argv))
