# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

import argparse
import json
import os
import sys
import zipfile


def _base_class_name_matches_base_source_path(
    base_class_name: str, base_source_path: str
):
    return base_class_name == base_source_path or base_source_path.endswith(
        "/" + base_class_name
    )


def main(argv):
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
    parser.add_argument("jar")
    parser.add_argument("sources", nargs="*")
    args = parser.parse_args(argv[1:])

    sources = {}
    for src in args.sources:
        path = src
        base, ext = os.path.splitext(src)
        sources[base] = path

    classes = []

    with zipfile.ZipFile(args.jar) as zf:
        for ent in zf.namelist():
            base, ext = os.path.splitext(ent)

            # Ignore non-.class files.
            if ext != ".class":
                continue

            classname = base.replace("/", ".")

            # Make sure it is a .class file that corresponds to a top-level
            # .class file and not an inner class.
            if "$" in base:
                continue

            found = False
            for src_base, src_path in sources.items():
                if _base_class_name_matches_base_source_path(base, src_base):
                    classes.append(
                        {
                            "className": classname,
                            "srcPath": src_path,
                        }
                    )
                    found = True
                    break
                # Kotlin creates .class files with a "Kt" suffix when code is written outside of a class,
                # so strip that suffix and redo the comparison.
                elif base.endswith("Kt") and _base_class_name_matches_base_source_path(
                    base[:-2], src_base
                ):
                    classes.append(
                        {
                            "className": classname[:-2],
                            "srcPath": src_path,
                        }
                    )
                    found = True
                    break

            if not found:
                # If the class is not present in the sources, we still want to
                # include it if it has a prefix that we are interested in.
                # certain classes in "androidx.databinding.*" are generated and it's useful to know their presence in jars
                for prefix in args.include_classes_prefixes:
                    if classname.startswith(prefix):
                        classes.append(
                            {
                                "className": classname,
                            }
                        )
                        break

    json.dump(
        {
            "jarPath": args.jar,
            "classes": classes,
        },
        args.output,
    )
    print("", file=args.output)


sys.exit(main(sys.argv))
