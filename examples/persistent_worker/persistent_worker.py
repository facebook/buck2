#!/usr/bin/env python3
# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

"""Buck2 local and remote persistent worker and action runner.

This script can:
- Execute build requests as a Buck2 local persistent worker.
- Execute build requests as a remote persistent worker through Bazel protocol.
"""

import argparse
import os
import shlex
import socket
import sys
import time
from concurrent import futures
from dataclasses import dataclass

import google.protobuf.proto as proto
import grpc
import proto.bazel.worker_protocol_pb2 as bazel_pb2
import proto.buck2.worker_pb2 as buck2_pb2
import proto.buck2.worker_pb2_grpc as buck2_pb2_grpc


@dataclass
class Request:
    """Universal worker request, independent of Buck2 or Bazel protocol."""

    argv: list[str]


@dataclass
class Response:
    """Universal worker response, independent of Buck2 or Bazel protocol."""

    exit_code: int
    stderr: str


class ArgumentParserError(Exception):
    pass


class RecoverableArgumentParser(argparse.ArgumentParser):
    def error(self, message):
        raise ArgumentParserError(f"{self.prog}: error: {message}\n")


class Implementation:
    """Universal worker implementation, independent of Buck2 or Bazel protocol."""

    def __init__(self):
        self.parser = RecoverableArgumentParser(
            fromfile_prefix_chars="@",
            prog="worker_py_handler",
            description="Persistent Worker Request Handler",
        )
        self.parser.add_argument(
            "outfile", type=argparse.FileType("w"), help="Output file."
        )

    def execute(self, request: Request) -> Response:
        try:
            print(
                "WORKER",
                socket.gethostname(),
                os.getpid(),
                os.getcwd(),
                file=sys.stderr,
            )
            print("REQUEST", request.argv, file=sys.stderr)
            args = self.parser.parse_args(request.argv)
            print("ARGS", args, file=sys.stderr)
            output = args.outfile
            name = os.path.basename(output.name)
            print("WRITE", name, file=sys.stderr)
            output.write(name + "\n")
            print("SLEEP", name, file=sys.stderr)
            time.sleep(1)
            print("COMPLETED", name, file=sys.stderr)
            output.close()
            return Response(exit_code=0, stderr=f"wrote to {output.name}")
        except ArgumentParserError as e:
            return Response(exit_code=2, stderr=str(e))


class Buck2Servicer(buck2_pb2_grpc.WorkerServicer):
    """Buck2 remote persistent worker implementation."""

    def __init__(self):
        self.impl = Implementation()

    def Execute(self, request, context):
        _ = context
        print("BUCK2", request, file=sys.stderr)
        # Decode arguments as UTF-8 strings.
        argv = [arg.decode("utf-8") for arg in request.argv]
        response = self.impl.execute(Request(argv=argv))
        host = socket.gethostname()
        pid = os.getpid()
        cwd = os.getcwd()
        return buck2_pb2.ExecuteResponse(
            exit_code=response.exit_code,
            stderr=f"Buck2 persistent worker {host} {pid} {cwd}\n" + response.stderr,
        )


class BazelServicer:
    def __init__(self):
        self.impl = Implementation()

    def Execute(self, request: bazel_pb2.WorkRequest) -> bazel_pb2.WorkResponse:
        print("BAZEL", request, file=sys.stderr)
        response = self.impl.execute(Request(argv=request.arguments))
        host = socket.gethostname()
        pid = os.getpid()
        cwd = os.getcwd()
        return bazel_pb2.WorkResponse(
            exit_code=response.exit_code,
            output=f"Bazel persistent worker {host} {pid} {cwd} {request.request_id}\n"
            + response.stderr,
            request_id=request.request_id,
        )


def main():
    print("MAIN", socket.gethostname(), os.getpid(), os.getcwd(), file=sys.stderr)
    parser = argparse.ArgumentParser(
        fromfile_prefix_chars="@",
        prog="worker",
        description="Buck2/Bazel Local/Remote Persistent Worker",
    )
    parser.add_argument(
        "--persistent_worker",
        action="store_true",
        help="Enable persistent worker (Bazel protocol).",
    )

    (args, rest) = parser.parse_known_args()

    if socket_path := os.getenv("WORKER_SOCKET"):
        # Buck2 persistent worker mode
        print("BUCK2 WORKER START", file=sys.stderr)
        if rest:
            rest_joined = " ".join(map(shlex.quote, rest))
            print(f"Unexpected arguments: {rest_joined}\n", file=sys.stderr)
            parser.print_usage()
            sys.exit(2)

        server = grpc.server(
            futures.ThreadPoolExecutor(max_workers=os.cpu_count() or 1)
        )
        buck2_pb2_grpc.add_WorkerServicer_to_server(Buck2Servicer(), server)
        server.add_insecure_port(f"unix://{socket_path}")
        server.start()
        server.wait_for_termination()
    elif args.persistent_worker:
        # Bazel persistent worker mode
        print("BAZEL WORKER START", file=sys.stderr)
        if rest:
            rest_joined = " ".join(map(shlex.quote, rest))
            print(f"Unexpected arguments: {rest_joined}\n", file=sys.stderr)
            parser.print_usage()
            sys.exit(2)

        servicer = BazelServicer()
        # uses length prefixed serialization features added in proto version 5.28.0.
        # https://github.com/protocolbuffers/protobuf/pull/16965
        while request := proto.parse_length_prefixed(
            bazel_pb2.WorkRequest, sys.stdin.buffer
        ):
            response = servicer.Execute(request)
            proto.serialize_length_prefixed(response, sys.stdout.buffer)
            sys.stdout.flush()
    else:
        print(
            "Expected either 'WORKER_SOCKET' environment variable or '--persistent_worker' argument.",
            file=sys.stderr,
        )
        sys.exit(2)


if __name__ == "__main__":
    main()
