#!/usr/bin/env python3
# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

# A "fake" linker command meant to be provided to rustc as `-Clinker={}`. This script will process
# the arguments passed in from rustc and export the objects, version script, and other arguments
# as outputs to later be used by an invocation of `deferred_link_action.py`.
#
# Some arguments here are stripped out e.g. -L<sysroot> in order to save work from having to persist
# an artifact between this action and the deferred link action. See the comments in
# `process_link_args()` for more details.

import argparse
import os
import shutil
import sys
from pathlib import Path
from typing import Any, IO, List, NamedTuple, Tuple


def eprint(*args: Any, **kwargs: Any) -> None:
    print(*args, end="\n", file=sys.stderr, flush=True, **kwargs)


class Args(NamedTuple):
    out_argsfile: IO[str]
    out_version_script: Path
    out_objects: Path
    linker: List[str]


def arg_parse() -> Args:
    parser = argparse.ArgumentParser()
    parser.add_argument(
        "--out_argsfile",
        type=argparse.FileType("w"),
        required=True,
    )
    parser.add_argument(
        "--out_version-script",
        type=Path,
        required=True,
    )
    parser.add_argument(
        "--out_objects",
        type=Path,
        required=True,
    )
    parser.add_argument(
        "linker",
        nargs=argparse.REMAINDER,
        type=str,
        help="Linker command line",
    )

    return Args(**vars(parser.parse_args()))


def process_link_args(args: List[str]) -> Tuple[List[str], Path | None, List[Path]]:
    new_args = []
    version_script = None
    objects = []

    i = 0
    size = len(args)
    while i < size:
        arg = args[i]
        # We want to extract the version script file as an artifact to pass along to the deferred
        # link action. rustc by default exports this file to somewhere in the TMP directory, so we
        # must persist it ourselves between actions via an artifact.
        if arg.startswith("-Wl,--version-script"):
            version_script = Path(arg.split("=")[1])
            i += 1
            continue
        # These are the artifacts that rustc generates as inputs to the linker.
        elif arg.endswith("rcgu.o") or arg.endswith("symbols.o"):
            objects.append(Path(arg))
            i += 1
            continue
        # We don't need either of these, and omitting them from the deferred link args will save
        # us from having to pass them to the deferred link action.
        # The .rlib files here are hollow rlibs, providing only metadata for each dependency. These
        # files have no impact on the link step; they're only needed by rustc.
        # The .rmeta file contains the metadata section for this crate being linked. Again, since
        # rmeta is not used at all for linking, we can omit the section entirely from our link step.
        elif arg.endswith(".rlib") or arg.endswith(".rmeta"):
            i += 1
            continue
        # The -L flag is used by rustc to pass the sysroot as a linker search path. When compiling
        # we pass a dummy empty sysroot to rustc, so this path is not needed. The real -L flags for
        # transitive deps are passed along in a separate args file.
        # The -o flag here is set by rustc to a temporary output location. In a normal rustc link,
        # rustc will eventually copy the temporary output file to the final location specified by
        # --emit=link={}. Since this path is temporary, we can simply omit it and pass the real
        # path needed by buck directly to the deferred link action.
        elif arg.startswith("-L") or arg.startswith("-o"):
            i += 2  # skip the next line
            continue

        new_args.append(arg)
        i += 1

    return (new_args, version_script, objects)


def unpack_objects(objects: Path) -> List[str]:
    return [x for x in os.listdir(objects) if x.endswith(".o") or x.endswith(".rmeta")]


def main() -> int:
    args = arg_parse()

    filtered_args, version_script, objects = process_link_args(args.linker[1:])
    args.out_argsfile.write("\n".join(filtered_args))
    args.out_argsfile.close()

    if version_script:
        shutil.copy(version_script, args.out_version_script)
    else:
        # Touch the file to make buck2 happy
        args.out_version_script.touch()

    os.mkdir(args.out_objects)
    for obj in objects:
        shutil.copy(obj, args.out_objects)

    return 0


sys.exit(main())
