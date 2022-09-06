#!/usr/bin/env fbpython
# (c) Facebook, Inc. and its affiliates. Confidential and proprietary.

import json
import sys
from pathlib import Path

_POSSIBLE_OUTPUT_PATHS = [
    "assetsDirPath",
    "bundlePath",
    "miscDirPath",
    "outputFilePath",
    "outputPath",
    "sourceMapPath",
]

START_HANDSHAKE_PREFIX = "["
START_MESSAGE_PREFIX = ","
EXIT_DATA = "]"


def _write_to_stream(data, stream=sys.stdout):
    stream.write(data)
    stream.flush()


def _read_from_stream(delimiter="}") -> str:
    buffer = ""
    while True:  # until EOF
        chunk = sys.stdin.read(1)
        if not chunk:  # EOF?
            return buffer

        buffer += chunk
        if delimiter == chunk:
            return buffer.strip()


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


"""
Worker tool implementation for use in JS testing - it just prints the given JSON command
to any output file that it finds.
"""


def main():
    _perform_handshake()

    job_data = _read_command()

    with open(job_data["args_path"], "r") as args_path:
        job_args = "".join([line.rstrip() for line in args_path.readlines()])
        job_args_json = json.loads(job_args)

        for possible_output_path in _POSSIBLE_OUTPUT_PATHS:
            if possible_output_path in job_args_json.keys():
                output_path = job_args_json[possible_output_path]
                Path(output_path).parent.mkdir(exist_ok=True)
                with open(output_path, "w+") as output:
                    output.write(json.dumps(job_args_json, indent=4))

    job_result = {
        "id": job_data["id"],
        "type": "result",
        "exit_code": "0",
    }
    _send_command_result(job_result)

    _perform_termination()


if __name__ == "__main__":
    main()
