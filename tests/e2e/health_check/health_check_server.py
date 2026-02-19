# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.


import argparse
import logging
import os
import signal
import sys
import threading
import time
from concurrent import futures

import grpc
from buck2.app.buck2_health_check_proto import health_check_pb2, health_check_pb2_grpc


class HealthChecker(health_check_pb2_grpc.HealthCheckServicer):
    def __init__(
        self,
        stop_event: threading.Event,
        argsparse: argparse.Namespace,
        *args,
        **kwargs,
    ) -> None:
        self.args = argsparse
        self.delay = argsparse.delay
        self.with_request_hang = argsparse.with_request_hang
        self.stats_file_handle = open(
            os.environ["HEALTH_CHECK_SERVER_STATS_OUTPUT"], "w+"
        )

    def UpdateContext(
        self,
        request: health_check_pb2.HealthCheckContextEvent,
        context: grpc.ServicerContext,
    ) -> health_check_pb2.Empty:
        self.stats_file_handle.write("UpdateContext Requested\n")
        self.stats_file_handle.flush()
        self.handle_request_delays()
        self.stats_file_handle.write("UpdateContext Completed\n")
        self.stats_file_handle.flush()
        return health_check_pb2.Empty()

    def RunChecks(
        self,
        request: health_check_pb2.HealthCheckSnapshotData,
        context: grpc.ServicerContext,
    ) -> health_check_pb2.HealthCheckResult:
        self.stats_file_handle.write("RunChecks Requested\n")
        self.stats_file_handle.flush()
        self.handle_request_delays()
        self.stats_file_handle.write("RunChecks Completed\n")
        self.stats_file_handle.flush()
        return health_check_pb2.HealthCheckResult(
            reports=[health_check_pb2.Report(tag="test_tag")]
        )

    def handle_request_delays(self) -> None:
        if self.with_request_hang:
            while True:
                time.sleep(1)
        if self.delay:
            time.sleep(int(self.delay))


def shutdown(stop_event: threading.Event) -> None:
    stop_event.set()


def serve(args: argparse.Namespace) -> None:
    server = grpc.server(futures.ThreadPoolExecutor(max_workers=10))
    stop_event = threading.Event()
    health_checker = HealthChecker(stop_event, args)
    health_check_pb2_grpc.add_HealthCheckServicer_to_server(health_checker, server)
    listen_addr = server.add_insecure_port("127.0.0.1:0")

    with open(args.state_info_file, "w+") as f:
        f.write(str(listen_addr))

    # This server is started by a shell scrip from the buck2 binary.
    # Since we don't have the process reference in tests, we need to send the SIGTERM instead of SIGINT.
    signal.signal(signal.SIGTERM, lambda x, y: shutdown(stop_event))
    signal.signal(signal.SIGINT, lambda x, y: shutdown(stop_event))

    server.start()
    try:
        stop_event.wait()
        print("Stopped RPC server, Waiting for RPCs to complete...")
        server.stop(1).wait()
    finally:
        print("Exiting health check server")


def parse_args(args: list[str] | None = None) -> argparse.Namespace:
    parser = argparse.ArgumentParser(description="Parse args for health check server")
    parser.add_argument(
        "--state-info-file",
        help="File name to which the server port is written",
        default="",
    )
    parser.add_argument(
        "--with-init-hang",
        required=False,
        action="store_true",
        help="flag that if set will hang on init",
    )
    parser.add_argument(
        "--with-request-hang",
        required=False,
        action="store_true",
        help="flag that if set will hang the request",
    )
    parser.add_argument(
        "--delay",
        type=int,
        help="response delay in seconds",
        default=0,
        required=False,
    )
    # no need to parse --tcp-port and other not related params
    parsed_args, _ = parser.parse_known_args(args)
    return parsed_args


def main() -> None:
    logging.basicConfig(level=logging.INFO)
    args = parse_args(sys.argv[1:])
    serve(args)


if __name__ == "__main__":
    # Do not add code here, it won't be run. Add them to the function called below.
    main()  # pragma: no cover
