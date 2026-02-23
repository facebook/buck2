#!/usr/bin/env python3
# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

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
from typing import Any, IO, NamedTuple


def eprint(*args: Any, **kwargs: Any) -> None:
    print(*args, end="\n", file=sys.stderr, flush=True, **kwargs)


class Args(NamedTuple):
    out_argsfile: IO[str]
    out_artifacts: Path
    linker: list[str]


def arg_parse() -> Args:
    parser = argparse.ArgumentParser()
    parser.add_argument(
        "--out_argsfile",
        type=argparse.FileType("w"),
        required=True,
    )
    parser.add_argument(
        "--out_artifacts",
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


def process_link_args(args: list[str], out_artifacts: Path) -> list[str]:
    new_args = []

    i = 0
    size = len(args)
    while i < size:
        arg = args[i]
        # We want to extract the version script file as an artifact to pass along to the deferred
        # link action. rustc by default exports this file to somewhere in the TMP directory, so we
        # must persist it ourselves between actions via an artifact.
        if arg.startswith("-Wl,--version-script"):
            version_script = Path(arg.split("=")[1])
            new_path = shutil.copy(version_script, out_artifacts)
            new_args.append(f"-Wl,--version-script={new_path}")
            i += 1
            continue

        # MacOS form of version script
        elif arg.startswith("-Wl,-exported_symbols_list"):
            arg = args[i + 1]
            exported_symbols_list = Path(arg.split("-Wl,")[1])
            new_path = shutil.copy(exported_symbols_list, out_artifacts)
            new_args.append(f"-Wl,-exported_symbols_list,{new_path}")
            i += 2
            continue

        # Windows form of version script
        elif arg.startswith("/DEF:"):
            def_file = Path(arg[5:])
            new_path = shutil.copy(def_file, out_artifacts)
            new_args.append(f"/DEF:{new_path}")
            i += 1
            continue

        # These are the artifacts that rustc generates as inputs to the linker.
        elif (
            arg.endswith("rcgu.o")
            or arg.endswith("rmeta.o")
            or arg.endswith("symbols.o")
            or arg.endswith("rcgu.rmeta")
            or arg.endswith("dll_imports.lib")
        ):
            new_path = shutil.copy(Path(arg), out_artifacts)
            new_args.append(new_path)
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
        elif arg.startswith("/OUT"):
            i += 1
            continue
        elif arg.startswith("-Wl,--no-undefined-version"):
            # Temporary fbcode specific hack
            i += 1
            continue
        new_args.append(arg)
        i += 1

    return new_args


def main() -> int:
    args = arg_parse()

    os.mkdir(args.out_artifacts)

    filtered_args = process_link_args(args.linker[1:], out_artifacts=args.out_artifacts)
    args.out_argsfile.write("\n".join(filtered_args))
    args.out_argsfile.close()

    return 0


sys.exit(main())
