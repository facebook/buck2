#!/usr/bin/env python3
# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

# A "fake" linker command meant to be provided to rustc as `-Clinker={}`. This script extracts
# from rustc's linker invocation the link inputs that only rustc can provide — the objects it
# synthesized for this crate — and exports them as outputs to later be consumed by the cxx-driven
# link of this crate.
#
# Everything else on the argv is discarded: the link is performed by cxx, and every flag and
# library it needs is provided by the toolchain and the dependency graph, the same way it is for
# a `cxx_binary`. See `process_link_args()` for the exact contract.

import argparse
import os
import shutil
import subprocess
import sys
from pathlib import Path
from typing import Any, IO, NamedTuple


def eprint(*args: Any, **kwargs: Any) -> None:
    print(*args, end="\n", file=sys.stderr, flush=True, **kwargs)


class Args(NamedTuple):
    out_argsfile: IO[str]
    out_artifacts: Path
    out_archive: Path | None
    archiver_argsfile: Path | None
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
    # When --out_archive and archiver_argsfile are set, the rustc objects are
    # collected into an archive rather than in the argsfile. Distributed ThinLTO needs
    # the objects to arrive as an `ArchiveLinkable` so it can plan a per-object
    # opt action for each linkable.
    parser.add_argument(
        "--out_archive",
        type=Path,
        default=None,
    )
    parser.add_argument(
        "--archiver_argsfile",
        type=Path,
        default=None,
        help="File holding the archiver command, one argument per line",
    )
    parser.add_argument(
        "linker",
        nargs=argparse.REMAINDER,
        type=str,
        help="Linker command line",
    )

    return Args(**vars(parser.parse_args()))


def process_link_args(
    args: list[str], out_artifacts: Path, collect_objects: bool
) -> tuple[list[str], list[str]]:
    """Extract rustc's synthesized objects from its linker argv, returning
    (args, objects).

    The objects are the only thing extracted:
     - the codegen unit objects of the crate itself, which includes the
       allocator shim module when rustc generates one, and
     - `symbols.o`, which carries undefined references to the exported and
       `#[used]` symbols of every linked crate so that the corresponding
       archive members survive the link.

    Nothing else is taken. In particular, the flags and libraries rustc puts
    on its link line are deliberately dropped: the toolchain and the
    dependency graph are the sole owners of the flags and libraries on the
    cxx-driven link. A rustc-synthesized file we don't recognize is an error,
    not something to pass through: it would be a dangling path by the time
    the link action runs. Version scripts are one known such case, so dylib
    and cdylib crates cannot currently be linked through this.

    With `collect_objects`, the objects are returned separately and left out
    of the returned args, for the caller to archive instead. Otherwise they
    are listed in the args and no objects are returned.
    """
    new_args = []
    objects = []
    # Original argv entries we handled, and the directories rustc placed
    # synthesized files in, used to detect files we don't know about.
    handled = set()
    temp_dirs = set()

    i = 0
    size = len(args)
    while i < size:
        arg = args[i]
        if arg.endswith("rcgu.o") or arg.endswith("symbols.o"):
            path = Path(arg)
            if path.parent.is_absolute():
                temp_dirs.add(str(path.parent))
            handled.add(arg)
            new_path = shutil.copy(path, out_artifacts)
            if collect_objects:
                objects.append(new_path)
            else:
                new_args.append(new_path)
            i += 1
            continue

        # `-o`'s value is a temporary output location that rustc would copy to
        # the `--emit=link={}` path; the link action provides its own output
        # path instead. `-L` carries only search paths (the dummy sysroot and
        # rustc's raw-dylibs temp directory), never a file. Both are marked
        # handled so their values don't trip the unrecognized-file check
        # below.
        elif arg == "-o" or arg == "-L":
            if i + 1 < size:
                handled.add(args[i + 1])
            i += 2  # also skip the flag's value
            continue

        i += 1

    # Anything left referencing a directory rustc synthesized files into is a
    # file this tool doesn't know about. It won't exist anymore when the link
    # action runs (possibly on another host), so it cannot be passed along;
    # fail loudly rather than silently dropping a link input.
    leaked = [a for a in args if a not in handled and any(d in a for d in temp_dirs)]
    if leaked:
        eprint(
            "rustc passed link inputs that extract_link_action.py does not know how to extract:"
        )
        for a in leaked:
            eprint(f"  {a}")
        eprint("Teach process_link_args() about them.")
        sys.exit(1)

    return new_args, objects


def archive_objects(
    archiver_argsfile: Path, out_archive: Path, objects: list[str]
) -> None:
    archiver = archiver_argsfile.read_text().split("\n")
    archiver = [arg for arg in archiver if arg]

    env = {**os.environ, "ZERO_AR_DATE": "1"}
    subprocess.check_call([*archiver, str(out_archive), *objects], env=env)


def main() -> int:
    args = arg_parse()

    os.mkdir(args.out_artifacts)

    archiver_argsfile = args.archiver_argsfile
    out_archive = args.out_archive
    archiving = archiver_argsfile is not None and out_archive is not None

    filtered_args, objects = process_link_args(
        args.linker[1:],
        out_artifacts=args.out_artifacts,
        collect_objects=archiving,
    )
    args.out_argsfile.write("\n".join(filtered_args))
    args.out_argsfile.close()

    if objects and archiver_argsfile is not None and out_archive is not None:
        archive_objects(archiver_argsfile, out_archive, objects)

    return 0


sys.exit(main())
