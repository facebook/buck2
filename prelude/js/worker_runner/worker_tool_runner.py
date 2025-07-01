# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

import argparse
import json
import os
import pathlib
import subprocess
import sys
import tempfile
import traceback
from typing import Any, Dict

ENCODING = "utf-8"

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
        description="Worker tool runner.",
        fromfile_prefix_chars="@",
    )

    parser.add_argument(
        "--worker-tool",
        type=str,
        required=True,
        metavar="worker_tool",
        help="Worker tool",
    )

    parser.add_argument(
        "--worker-args-file",
        type=pathlib.Path,
        metavar="worker_args_file",
        help="Path to the file with stored worker tool arguments",
    )

    parser.add_argument(
        "--worker-env-file",
        type=pathlib.Path,
        metavar="worker_env_file",
        help="Path to the file with stored environment variables",
    )

    parser.add_argument(
        "--command-args-file",
        type=str,
        nargs="+",
        metavar="command_args_file",
        help="Path to the file with command arguments",
    )

    # TODO(T121096376) remove this hack
    parser.add_argument(
        "--command-args-file-extra-data-fixup-hack",
        type=bool,
        metavar="command_args_file_extra_data_fixup_hack",
        help="Whether to change the 'extraData' in the command args file to be a JSON blob",
    )

    return parser.parse_known_args()


def _write_to_stream(stream, data):
    stream.write(data.encode(ENCODING))
    stream.flush()


def read_till_separator(stream, separator):
    buffer = ""
    while True:  # until EOF
        chunk = stream.read(1)
        if not chunk:  # EOF?
            return buffer

        decoded_chunk = chunk.decode(ENCODING)
        buffer += decoded_chunk
        if separator == decoded_chunk:
            return buffer.strip()


def _read_from_stream(stream, delimiter="}") -> str:
    return read_till_separator(stream, separator=delimiter)


def _create_handshake_data(message_id) -> Dict[str, Any]:
    return {
        "id": message_id,
        "type": TYPE_HANDSHAKE,
        "protocol_version": PROTOCOL_VERSION,
        "capabilities": [],
    }


def _perform_handshake(worker, message_id):
    handshake_data = _create_handshake_data(message_id)
    handshake_data_string = json.dumps(handshake_data).rstrip()
    _write_to_stream(
        worker.stdin, "{}{}".format(START_HANDSHAKE_PREFIX, handshake_data_string)
    )
    handshake_reply = _read_from_stream(worker.stdout)
    if len(handshake_reply) == 0:
        raise RuntimeError(
            "Handshake '"
            + handshake_data_string
            + "' failed for worker '"
            + json.dumps(worker.args).rstrip()
            + "' with stderr: '"
            + _read_from_stream(worker.stderr)
            + "'"
        )
    assert START_HANDSHAKE_PREFIX == handshake_reply[0]
    handshake_reply_data = json.loads(handshake_reply[1:])
    assert handshake_data == handshake_reply_data


def _create_args_file(args, tmp_dir) -> pathlib.Path:
    args_path = os.path.join(tmp_dir, "command.args")
    with open(args_path, "x") as args_path_writer:
        for arg in args:
            args_path_writer.write(arg)
            args_path_writer.write(os.linesep)
    return args_path


def _create_job_command(args_path, message_id, tmp_dir) -> Dict[str, Any]:
    job_stdout = os.path.join(tmp_dir, "{}.out".format(message_id))
    job_stderr = os.path.join(tmp_dir, "{}.err".format(message_id))

    return {
        "id": message_id,
        "type": TYPE_COMMAND,
        "args_path": args_path,
        "stdout_path": job_stdout,
        "stderr_path": job_stderr,
    }


def _send_command(command, worker):
    _write_to_stream(
        worker.stdin, "{}{}".format(START_MESSAGE_PREFIX, json.dumps(command).rstrip())
    )


def _receive_command_reply(worker, message_id) -> int:
    reply = _read_from_stream(worker.stdout)
    if not reply:
        raise RuntimeError(
            f"Failed to receive reply from worker, stderr:\n\n{worker.stderr.read()}"
        )
    assert START_MESSAGE_PREFIX == reply[0]
    reply_data = json.loads(reply[1:])

    assert reply_data["id"] == message_id

    type = reply_data["type"]
    if type == TYPE_ERROR:
        raise AssertionError("Worker had an error: {}".format(reply_data))

    assert type == TYPE_RESULT

    exit_code = reply_data["exit_code"]
    if not isinstance(exit_code, int):
        raise AssertionError(
            "Worker returned non-integer exit code: {}".format(reply_data)
        )
    return exit_code


def _perform_termination(worker):
    _write_to_stream(worker.stdin, EXIT_DATA)
    exit_reply = _read_from_stream(worker.stdout, delimiter=EXIT_DATA)
    assert exit_reply == EXIT_DATA


def _maybe_expand_worker_arg(arg, envs) -> str:
    expanded_arg = arg
    if "$" in arg:
        for k, v in os.environ.items():
            expanded_arg = expanded_arg.replace("${}".format(k), v)
            expanded_arg = expanded_arg.replace("${}{}{}".format("{", k, "}"), v)
    return expanded_arg


def _println(line):
    if line:
        print("{}{}".format(line.rstrip(), os.linesep))


def _fixup_command_args_file(command_args_file, index, tmp_dir) -> str:
    with open(command_args_file, "r") as input_f:
        try:
            content = json.load(input_f)
        except json.JSONDecodeError:
            raise Exception(
                f"Unable to decode {command_args_file}, it should be a JSON file.",
                file=sys.stderr,
                flush=True,
            )

    if "extraData" in content:
        try:
            content["extraData"] = json.loads(content["extraData"])
        except json.JSONDecodeError:
            raise Exception(
                f"Unable to decode 'extraData' in {command_args_file}, it should be a JSON string.",
                file=sys.stderr,
                flush=True,
            )

    args_path = os.path.join(
        tmp_dir,
        "fixup_command_args_{}".format(index),
    )
    with open(args_path, "x") as args_path_writer:
        args_path_writer.write(json.dumps(content, sort_keys=True))

    return args_path


def _prepare_command_args_files(parsed_args, unparsed_args, tmp_dir):
    command_args_files = parsed_args.command_args_file
    command_args_file_extra_data_fixup_hack = (
        parsed_args.command_args_file_extra_data_fixup_hack
    )
    command_args = unparsed_args

    if command_args_files and len(command_args) > 0:
        raise RuntimeError("Use either command line arguments or a file, not both!")

    if command_args_files:
        if command_args_file_extra_data_fixup_hack:
            command_args_files = [
                _fixup_command_args_file(command_args_file, i, tmp_dir)
                for i, command_args_file in enumerate(command_args_files)
            ]
    elif command_args:
        if command_args_file_extra_data_fixup_hack:
            raise RuntimeError(
                "Only request command args file fixup if a command args file is specified!"
            )
        command_args_file = _create_args_file(command_args, tmp_dir)
        command_args_files = [command_args_file]
    else:
        raise RuntimeError("Must provide a command for the worker to run!")

    return command_args_files


def _prepare_environment(worker_env_file):
    envs = dict(os.environ)
    if worker_env_file:
        with open(worker_env_file) as we_file:
            lines = we_file.readlines()
            for i in range(0, len(lines), 2):
                key = lines[i].rstrip()
                value = lines[i + 1].rstrip()
                envs[key] = value
    return envs


def _handle_error(exit_code, job_command):
    if exit_code:
        _println("Worker tool command finished with exit code: {}".format(exit_code))

    if job_command is None:
        _println("Handshake failed")
    else:
        stdout_path = job_command["stdout_path"]
        if os.path.exists(stdout_path):
            _println("{}std out: ".format(os.linesep))
            with open(job_command["stdout_path"], "r") as std_out_reader:
                for line in std_out_reader.readlines():
                    _println(line)

        stderr_path = job_command["stderr_path"]
        if os.path.exists(stderr_path):
            _println("{}std err: ".format(os.linesep))
            with open(job_command["stderr_path"], "r") as std_err_reader:
                for line in std_err_reader.readlines():
                    _println(line)

    if exit_code is not None:
        sys.exit(exit_code)


def _get_worker_tool_args(worker_args_file, envs):
    worker_tool_args = []
    if worker_args_file:
        with open(worker_args_file) as wa_file:
            for line in wa_file.readlines():
                worker_tool_args.append(_maybe_expand_worker_arg(line.rstrip(), envs))
    return worker_tool_args


def main():
    parsed_args, unparsed_args = _parse_args()

    envs = _prepare_environment(parsed_args.worker_env_file)
    worker_tool_args = _get_worker_tool_args(parsed_args.worker_args_file, envs)

    with tempfile.TemporaryDirectory() as tmp_dir:
        command_args_files = _prepare_command_args_files(
            parsed_args, unparsed_args, tmp_dir
        )

        worker_tool = parsed_args.worker_tool
        # It's important to use os.path.normpath() instead of pathlib.Path.resolve()
        # because we don't want to resolve symlinks.
        worker_tool = os.path.normpath(os.path.join(os.getcwd(), worker_tool))
        with subprocess.Popen(
            [worker_tool] + worker_tool_args,
            env=envs,
            stdout=subprocess.PIPE,
            stdin=subprocess.PIPE,
            stderr=subprocess.PIPE,
        ) as worker:
            exit_code = None
            try:
                job_command = None
                message_id = 0
                _perform_handshake(worker, message_id)

                for command_args_file in command_args_files:
                    message_id += 1
                    job_command = _create_job_command(
                        command_args_file, message_id, tmp_dir
                    )
                    _send_command(job_command, worker)
                    exit_code = _receive_command_reply(worker, message_id)

                _perform_termination(worker)

            except Exception:
                traceback.print_exc()
                exit_code = 1
            finally:
                _handle_error(exit_code, job_command)


if __name__ == "__main__":
    main()
