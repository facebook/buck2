#!/usr/bin/env python3
# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

import argparse
import os
import re
from io import TextIOWrapper
from typing import Dict, FrozenSet, Iterable, List


_RESERVED_KEYWORDS: FrozenSet[str] = frozenset(
    [
        "config_macros",
        "conflict",
        "exclude",
        "explicit",
        "extern",
        "export_as",
        "export",
        "framework",
        "header",
        "link",
        "module",
        "private",
        "requires",
        "textual",
        "umbrella",
        "use",
    ]
)


class Module:
    def __init__(
        self, name: str, mark_headers_private: bool, is_framework: bool
    ) -> None:
        self.name: str = name
        self.headers: List[str] = []
        self.mark_headers_private: bool = mark_headers_private
        self.submodules: Dict[str, Module] = {}
        self.is_framework: bool = is_framework

    def add_header(self, src: str) -> None:
        self.headers.append(src)

    def get_submodule(
        self, name: str, mark_headers_private: bool, is_framework: bool
    ) -> "Module":
        if name not in self.submodules:
            self.submodules[name] = Module(name, mark_headers_private, is_framework)

        return self.submodules[name]

    def render(self, f: TextIOWrapper, path_prefix: str, indent: int = 0) -> None:
        space = " " * indent
        name = self.name
        if name in _RESERVED_KEYWORDS:
            name = f"{name}_"
        if self.is_framework:
            f.write(f"{space}framework module {name} {{\n")
        else:
            f.write(f"{space}module {name} {{\n")

        submodule_names = set()
        for submodule_name in sorted(self.submodules.keys()):
            submodule = self.submodules[submodule_name]

            # remove any extensions for readability
            sanitized_name = os.path.splitext(submodule_name)[0]

            # module names can only be ascii or _
            sanitized_name = re.sub(r"[^A-Za-z0-9_]", "_", sanitized_name)
            if sanitized_name[0].isdigit():
                sanitized_name = "_" + sanitized_name

            # avoid any collisions with other files
            while sanitized_name in submodule_names:
                sanitized_name += "_"

            submodule_names.add(sanitized_name)
            submodule.name = sanitized_name
            submodule.render(f, path_prefix, indent + 4)

        header_space = " " * (indent + 4)
        prefix = "private " if self.mark_headers_private else ""
        for h in sorted(self.headers):
            f.write(f'{header_space}{prefix}header "{os.path.join(path_prefix, h)}"\n')

        if self.headers:
            f.write(f"{header_space}export *\n")

        f.write(f"{space}}}\n")


def _write_single_module(
    f: TextIOWrapper,
    name: str,
    headers: Iterable[str],
    path_prefix: str,
    mark_headers_private: bool,
    is_framework: bool,
) -> None:
    module = Module(name, mark_headers_private, is_framework)
    for h in headers:
        module.add_header(h)

    module.render(f, path_prefix)


def _write_submodules(
    f: TextIOWrapper,
    name: str,
    headers: Iterable[str],
    path_prefix: str,
    mark_headers_private: bool,
    is_framework: bool,
) -> None:
    # Create a tree of nested modules, one for each path component.
    root_module = Module(name, mark_headers_private, is_framework)
    for h in headers:
        module = root_module
        for i, component in enumerate(h.split(os.sep)):
            if i == 0 and component == name:
                # The common case is we have a single header path prefix that matches the module name.
                # In this case we add the headers directly to the root module.
                pass
            else:
                module = module.get_submodule(
                    component, mark_headers_private, is_framework
                )

        module.add_header(h)

    root_module.render(f, path_prefix)


def _write_swift_header(f: TextIOWrapper, name: str, swift_header_path: str) -> None:
    f.write(
        f"""
module {name}.Swift {{
    header "{swift_header_path}"
    requires objc
}}
"""
    )


def main() -> None:
    parser = argparse.ArgumentParser()
    parser.add_argument(
        "--output", required=True, help="The path to write the modulemap to"
    )
    parser.add_argument("--name", required=True, help="The name of the module")
    parser.add_argument(
        "--swift-header", help="If this is a mixed module extend with this Swift header"
    )
    parser.add_argument(
        "--use-submodules",
        action="store_true",
        help="If set produce a modulemap with per-header submodules",
    )
    parser.add_argument(
        "--symlink-tree",
        required=True,
    )
    parser.add_argument(
        "--mark-headers-private",
        help="This doesn't apply to --swift-header.",
        action="store_true",
    )
    parser.add_argument(
        "--framework",
        help="This modulemap is for embedding in a framework.",
        action="store_true",
    )
    parser.add_argument(
        "mappings", nargs="*", default=[], help="A list of import paths"
    )
    args = parser.parse_args()

    output_dir = os.path.dirname(args.output)
    if args.framework:
        path_prefix = ""
    else:
        path_prefix = os.path.relpath(args.symlink_tree, output_dir)

    with open(args.output, "w") as f:
        if args.use_submodules:
            _write_submodules(
                f,
                args.name,
                args.mappings,
                path_prefix,
                args.mark_headers_private,
                args.framework,
            )
        else:
            _write_single_module(
                f,
                args.name,
                args.mappings,
                path_prefix,
                args.mark_headers_private,
                args.framework,
            )

        if args.swift_header:
            swift_header_name = os.path.relpath(args.swift_header, output_dir)
            _write_swift_header(
                f,
                args.name,
                os.path.join(
                    "swift-extended_symlink_tree",
                    args.name,
                    str(swift_header_name),
                ),
            )


if __name__ == "__main__":
    main()
