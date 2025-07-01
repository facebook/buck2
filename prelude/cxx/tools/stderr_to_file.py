#!/usr/bin/env python3
# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

"""
Usage: stderr_to_file.py --out=path/to/output path/to/clang++ [args...]
"""

import argparse
import asyncio
import signal
import subprocess
import sys
from pathlib import Path
from typing import List, NamedTuple


# Exit code of `bash -c 'sleep 100'` <CTRL+C>
_INTERRUPTED = 128 + signal.SIGINT.value


class Args(NamedTuple):
    out: Path
    command: List[str]


class SubprocessProtocol(asyncio.SubprocessProtocol):
    """Write subprocess stderr to both self.out and sys.stderr"""

    def __init__(self, out, exit_future):
        self.out = out
        self.exit_future = exit_future
        self.pipe_closed = False
        self.exited = False

    def pipe_data_received(self, fd, data):
        if fd == sys.stderr.fileno():
            # Blocking write to file. This is buffered in a Python
            # io.BufferedRandom.
            self.out.write(data)
            # Blocking unbuffered write to stderr. Our writes will be exactly as
            # buffered as the subprocess's writes.
            sys.stderr.buffer.write(data)
            sys.stderr.flush()

    def pipe_connection_lost(self, fd, exc):
        if fd == sys.stderr.fileno():
            self.pipe_closed = True
            # Either of pipe_connection_lost() or process_exited() can be called
            # before the other. Wait until both methods are called.
            self._check_for_exit()

    def process_exited(self):
        self.exited = True
        # Either of pipe_connection_lost() or process_exited() can be called
        # before the other. Wait until both methods are called.
        self._check_for_exit()

    def _check_for_exit(self):
        if self.pipe_closed and self.exited:
            try:
                self.exit_future.set_result(True)
            except asyncio.InvalidStateError:
                # Event loop has shut down.
                pass


async def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("--out", type=Path, required=True)
    parser.add_argument("command", nargs=argparse.REMAINDER)
    args = Args(**vars(parser.parse_args()))

    loop = asyncio.get_running_loop()
    exit_future = asyncio.Future(loop=loop)

    with open(args.out, "wb+") as out:
        transport, protocol = await loop.subprocess_exec(
            lambda: SubprocessProtocol(out, exit_future),
            *args.command,
            stdin=None,  # inherit
            stdout=None,  # inherit
            stderr=subprocess.PIPE,
        )
        await exit_future
        transport.close()

    returncode = transport.get_returncode()
    if returncode is None:
        return _INTERRUPTED
    else:
        return returncode


try:
    sys.exit(asyncio.run(main()))
except KeyboardInterrupt:
    sys.exit(_INTERRUPTED)
