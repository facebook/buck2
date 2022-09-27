#!/usr/bin/env python3
# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

"""
Runs a simple buckd for testing some parts of buck2 daemon operation/lifecycle
"""

import argparse
import fcntl
import json
import logging
import os
import pathlib
import sys
import threading
import time
from concurrent.futures import ThreadPoolExecutor

import daemon
import grpc

try:
    # Import using buck test
    from buck2.cli.py import daemon_pb2, daemon_pb2_grpc
except ImportError:
    # Import using generic python
    sys.path.insert(0, "/tmp/daemon_pb2_out/")
    import daemon_pb2
    import daemon_pb2_grpc

# The buck2 daemon lifecycle is documented in buck2/cli/daemon_lifecycle.md
#
# The important bits that we replicate are:
#   1. create and lock the pid file
#   2. write our pid to the pid file
#   3. write our pid and server port in json to the .info file
#   4. write an empty .stdout and .stderr file
# TODO(cjhopman): remove 4 once buck has real events flowing from the daemon
#
# We implement just a few small parts of the daemon api: kill and status endpoints
# We define a handful of delays that are used for testing buck2 daemon client
# (particular testing killing a non-responsive daemon).


class HangingServicer(daemon_pb2_grpc.DaemonApiServicer):
    def __init__(self, kill_sem, kill_delay, pid):
        self.kill_sem = kill_sem
        self.kill_delay = kill_delay
        self.pid = pid
        pass

    def Kill(self, request, context):
        if self.kill_delay > 0:
            time.sleep(self.kill_delay)
        self.kill_sem.release()
        return daemon_pb2.CommandResult(kill_response=daemon_pb2.KillResponse())

    def Status(self, request, context):
        return daemon_pb2.CommandResult(
            status_response=daemon_pb2.StatusResponse(
                process_info=self.process_info,
                start_time=None,
                uptime=None,
            )
        )


def serve(args):
    pid = os.getpid()
    daemon_dir = pathlib.Path(args.daemon_dir)

    with open(daemon_dir / "buckd.pid", "w") as pidfile:
        (daemon_dir / "buckd.stdout").touch()
        (daemon_dir / "buckd.stderr").touch()
        pidfile.write(str(pid))
        fcntl.flock(pidfile, fcntl.LOCK_EX)

        server = grpc.server(thread_pool=ThreadPoolExecutor(max_workers=1))
        kill_sem = threading.Semaphore(0)

        servicer = HangingServicer(kill_sem, args.kill_delay, pid)

        daemon_pb2_grpc.add_DaemonApiServicer_to_server(servicer, server)
        listen_addr = server.add_insecure_port("localhost:0")
        logging.info("Starting server on %s w/ pid %s", listen_addr, pid)
        endpoint = "{}:{}".format("tcp", listen_addr)

        with open(daemon_dir / "buckd.info", "w") as infofile:
            infofile.write(
                json.dumps(
                    {
                        "pid": pid,
                        "endpoint": endpoint,
                        "version": args.version,
                    }
                )
            )

        servicer.listen_addr = listen_addr
        servicer.process_info = daemon_pb2.DaemonProcessInfo(
            pid=pid,
            endpoint=endpoint,
            version=args.version,
        )

        if not args.no_server:
            server.start()
            kill_sem.acquire()
            server.stop(1)
            server.wait_for_termination()

        if args.shutdown_delay > 0:
            time.sleep(args.shutdown_delay)


if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument("--no-server", action="store_true")
    parser.add_argument("--kill-delay", type=float, default=0)
    parser.add_argument("--shutdown-delay", type=float, default=0)
    parser.add_argument("--daemon-dir", required=True)
    parser.add_argument("--version", required=True)
    args = parser.parse_args()

    with daemon.DaemonContext():
        logging.basicConfig(level=logging.INFO)
        serve(args)
