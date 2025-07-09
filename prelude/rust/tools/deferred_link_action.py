#!/usr/bin/env python3
# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

# Execute a previously deferred link action. The inputs to this script are expected to come from
# a previous invocation of `extract_link_action.py`.

import argparse
import asyncio
import os
import sys
import tempfile
from typing import Any, List, NamedTuple


def eprint(*args: Any, **kwargs: Any) -> None:
    print(*args, end="\n", file=sys.stderr, flush=True, **kwargs)


class Args(NamedTuple):
    linker: List[str]


def arg_parse() -> Args:
    parser = argparse.ArgumentParser()
    parser.add_argument(
        "linker",
        nargs=argparse.REMAINDER,
        type=str,
        help="Linker command line",
    )

    return Args(**vars(parser.parse_args()))


async def main() -> int:
    args = arg_parse()

    linker_cmd = args.linker[:1]

    with tempfile.NamedTemporaryFile(
        mode="wb",
        prefix="real-linker-args-",
        suffix=".txt",
        delete=False,
    ) as args_file:
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
