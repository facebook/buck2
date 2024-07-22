#!/usr/bin/env python3
# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

# Execute a previously deferred link action. The inputs to this script are expected to come from
# a previous invocation of `extract_link_action.py`. The main special processing here is to handle
# the optional version script argument, and pass the objects located in the provided directory
# as individual inputs to the linker command.

import argparse
import asyncio
import os
import sys
import tempfile
from pathlib import Path
from typing import Any, List, NamedTuple


def eprint(*args: Any, **kwargs: Any) -> None:
    print(*args, end="\n", file=sys.stderr, flush=True, **kwargs)


class Args(NamedTuple):
    objects: Path
    version_script: Path
    linker: List[str]


def arg_parse() -> Args:
    parser = argparse.ArgumentParser()
    parser.add_argument(
        "--objects",
        type=Path,
        required=True,
    )
    parser.add_argument(
        "--version-script",
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


def unpack_objects(objects: Path) -> List[str]:
    return [os.path.join(objects, x) for x in os.listdir(objects) if x.endswith(".o")]


async def main() -> int:
    args = arg_parse()

    linker_cmd = args.linker[:1]

    objects = unpack_objects(args.objects)

    with tempfile.NamedTemporaryFile(
        mode="wb",
        prefix="real-linker-args-",
        suffix=".txt",
        delete=False,
    ) as args_file:
        # Some platforms do not use version-scripts. For those platforms we simply
        # do not pass the version-script to the linker.
        if os.path.getsize(args.version_script) > 0:
            args_file.write(
                b"-Wl,--version-script=" + str(args.version_script).encode() + b"\n"
            )

        args_file.write("\n".join(objects).encode() + b"\n")
        args_file.write("\n".join(args.linker[1:]).encode() + b"\n")
        args_file.flush()

        proc = await asyncio.create_subprocess_exec(
            *linker_cmd,
            "@" + args_file.name,
            env=os.environ,
            limit=1_000_000,
        )
        res = await proc.wait()

    return res


sys.exit(asyncio.run(main()))
