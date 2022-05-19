#!/usr/bin/env python3

import argparse
import os
import signal
import sys
import threading
from concurrent.futures import ThreadPoolExecutor

import grpc
from buck2.install_proto import install_pb2, install_pb2_grpc


class BadInstallerService(install_pb2_grpc.InstallerServicer):
    def __init__(self, stop_event, *args, **kwargs):
        self.stop_event = stop_event

    def FileReadyRequest(self, request, context):
        err_msg = f"Mocking failing to install '{request.path}'"
        response = {
            "name": request.name,
            "err_msg": err_msg,
            "err": True,
            "path": request.path,
        }
        return install_pb2.FileResponse(**response)

    def ShutdownServer(self, request, context):
        shutdown(self.stop_event)
        response = {}
        return install_pb2.ShutdownResponse(**response)


def shutdown(stop_event):
    stop_event.set()


def serve(args):
    server = grpc.server(thread_pool=ThreadPoolExecutor(max_workers=1))
    stop_event = threading.Event()
    install_pb2_grpc.add_InstallerServicer_to_server(
        BadInstallerService(stop_event), server
    )
    ## https://grpc.github.io/grpc/python/grpc.html
    listen_addr = server.add_insecure_port(f"unix://{args.named_pipe}")
    print(f"Starting server on {listen_addr} w/ pid {os.getpid()}")
    server.start()
    signal.signal(signal.SIGINT, lambda x, y: shutdown(stop_event))
    stop_event.wait()
    print("Stopped RPC server, Waiting for RPCs to complete...")
    server.stop(1).wait()
    print("Done stopping server")


def parse_args(args=None):
    parser = argparse.ArgumentParser(description="Parse args for installer")
    parser.add_argument(
        "--named-pipe",
        type=str,
        help="named pipe for installer to connect to",
        required=True,
    )
    return parser.parse_args(args)


if __name__ == "__main__":
    args = parse_args(sys.argv[1:])
    serve(args)
