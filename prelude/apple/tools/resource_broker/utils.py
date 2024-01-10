# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

import asyncio
import json
import shlex
from dataclasses import dataclass
from io import TextIOWrapper
from pathlib import Path
from typing import Any, List, Tuple

from dataclasses_json import dataclass_json

from .idb_companion import IdbCompanion
from .timeouts import DEFAULT_OPERATION_TIMEOUT


@dataclass_json
@dataclass
class _IdbStdout:
    grpc_path: str


@dataclass
class IdbCompanionProcess:
    process: asyncio.subprocess.Process
    stderr: TextIOWrapper
    stderr_path: Path

    def cleanup(self) -> None:
        self.process.terminate()
        self.stderr.close()


async def _read_until_valid_json(stream: asyncio.StreamReader) -> Any:
    buffer = b""
    while True:
        data = await stream.readuntil(b"}")
        buffer += data
        try:
            return json.loads(buffer.decode())
        except json.JSONDecodeError:
            pass
    raise RuntimeError(
        "Should not be reachable since either the valid JSON is there or `asyncio.IncompleteReadError` is raised."
    )


async def _read_stdout(p: IdbCompanionProcess) -> Tuple[int, TextIOWrapper, Any]:
    if not p.process.stdout:
        raise ValueError("Expected stdout to be set for idb companion launch process.")
    try:
        json = await _read_until_valid_json(p.process.stdout)
    except asyncio.IncompleteReadError as e:
        if not e.partial:
            with open(p.stderr_path) as f:
                lines = f.readlines()
                raise RuntimeError(
                    f"idb companion terminated unexpectedly with the following stderr:\n{lines}"
                ) from e
        else:
            raise
    return p.process.pid, p.stderr, json


async def wait_for_idb_companions(
    processes: List[IdbCompanionProcess],
    timeout: float = DEFAULT_OPERATION_TIMEOUT,
) -> List[IdbCompanion]:
    reads = [asyncio.Task(_read_stdout(p)) for p in processes]
    done, pending = await asyncio.wait(
        reads,
        timeout=timeout,
    )
    if not pending:
        results = [task.result() for task in done]
        return [
            IdbCompanion(
                # pyre-ignore[16]: `from_dict` is dynamically provided by `dataclass_json`
                socket_address=_IdbStdout.from_dict(json_dict).grpc_path,
                pid=pid,
                stderr=stderr,
            )
            for pid, stderr, json_dict in results
        ]

    process_index = {reads[i]: processes[i] for i in range(len(processes))}

    stderr_paths = []

    for task in pending:
        task.cancel()
        process_info = process_index[task]
        stderr_paths.append(str(process_info.stderr_path))
        process_info.process.terminate()

    raise RuntimeError(
        f"Timeout when trying to launch idb companions. List of files with stderr for pending companions: {stderr_paths}"
    )


async def execute_generic_text_producing_command(
    name: str, cmd: List[str], timeout: float = DEFAULT_OPERATION_TIMEOUT
) -> str:
    process = await asyncio.create_subprocess_exec(
        *cmd,
        stdin=asyncio.subprocess.DEVNULL,
        stdout=asyncio.subprocess.PIPE,
        stderr=asyncio.subprocess.PIPE,
    )
    stdout, stderr = await asyncio.wait_for(process.communicate(), timeout=timeout)
    if process.returncode != 0:
        raise RuntimeError(
            f"Failed to {name} with command:\n```\n{shlex.join(cmd)}\n```\nstdout:\n```\n{stdout.decode(errors='ignore')}\n```\nstdout:\n```\n{stderr.decode(errors='ignore')}\n```\n"
        )
    return stdout.decode()


async def spawn_companion(
    command: List[str],
    log_file_suffix: str,
) -> IdbCompanionProcess:
    stderr_path = Path("/tmp/buck2_idb_companion_logs") / f"stderr-{log_file_suffix}"
    stderr_path.parent.mkdir(parents=True, exist_ok=True)
    stderr = stderr_path.open(mode="w")
    process = await asyncio.create_subprocess_exec(
        *command,
        stdin=asyncio.subprocess.DEVNULL,
        stdout=asyncio.subprocess.PIPE,
        stderr=stderr,
    )
    return IdbCompanionProcess(
        process=process,
        stderr=stderr,
        stderr_path=stderr_path,
    )
