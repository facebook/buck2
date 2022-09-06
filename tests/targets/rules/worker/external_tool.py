#!/usr/bin/env fbpython
# (c) Facebook, Inc. and its affiliates. Confidential and proprietary.

import argparse
import json
import os
import pathlib
import subprocess
import sys

TYPE_HANDSHAKE = "handshake"
TYPE_COMMAND = "command"
TYPE_RESULT = "result"
TYPE_ERROR = "error"
PROTOCOL_VERSION = "0"

START_HANDSHAKE_PREFIX = "["
START_MESSAGE_PREFIX = ","
EXIT_DATA = "]"


def _parse_args():
    parser = argparse.ArgumentParser(
        description="External tool used for worker tool testing."
    )

    parser.add_argument(
        "--num-jobs",
        type=int,
        required=True,
        help="number of jobs this tool should expect",
    )

    parser.add_argument(
        "--hello-phrase",
        required=True,
        help="Hello phrase that should be printed before execution starts",
    )

    parser.add_argument(
        "--out",
        required=True,
        type=pathlib.Path,
        help="Path to the output file",
    )

    return parser.parse_args()


def _write_to_stream(data, stream=sys.stdout, append_with_eol=False):
    stream.write(data)
    if append_with_eol:
        stream.write(os.linesep)
    stream.flush()


def _writeln(line, stream):
    _write_to_stream(data=line, stream=stream, append_with_eol=True)


def _read_from_stream(delimiter="}") -> str:
    buffer = ""
    while True:  # until EOF
        chunk = sys.stdin.read(1)
        if not chunk:  # EOF?
            return buffer

        buffer += chunk
        if delimiter == chunk:
            return buffer


def _perform_handshake():
    handshake_data = _read_from_stream()
    assert len(handshake_data) > 0
    assert START_HANDSHAKE_PREFIX == handshake_data[0]
    _write_to_stream(handshake_data)


def _read_command():
    command = _read_from_stream()
    assert len(command) > 0
    assert START_MESSAGE_PREFIX == command[0]
    return json.loads(command[1:])


def _send_command_result(command_result):
    command_result_string = json.dumps(command_result)
    _write_to_stream("{}{}".format(START_MESSAGE_PREFIX, command_result_string))


def _perform_termination():
    exit_value = _read_from_stream(delimiter=EXIT_DATA)
    assert exit_value == EXIT_DATA
    _write_to_stream(exit_value)


def main():
    args = _parse_args()
    hello_phrase = args.hello_phrase.replace(
        "$WORKER_TOOL_NAME", os.environ["WORKER_TOOL_NAME"]
    )

    _perform_handshake()

    for _ in range(args.num_jobs):
        command_data = _read_command()

        with open(command_data["args_path"], "r") as args_path:
            start_command_args = [line.rstrip() for line in args_path.readlines()]

        return_code = -1
        with open(args.out, "w") as out_file:
            with open(command_data["stdout_path"], "w") as std_out_file:
                with open(command_data["stderr_path"], "w") as std_err_file:
                    _writeln(hello_phrase, std_out_file)
                    job_process = subprocess.run(
                        start_command_args,
                        check=True,
                        stdout=out_file,
                        stderr=out_file,
                    )
                    return_code = job_process.returncode
                    _writeln(
                        "Process finished with exit code: {}".format(return_code),
                        std_err_file,
                    )
                    _writeln("Result is stored in: {}".format(args.out), std_out_file)

        _send_command_result(
            {
                "id": command_data["id"],
                "type": "result",
                "exit_code": return_code,
            }
        )

    _perform_termination()


if __name__ == "__main__":
    main()
