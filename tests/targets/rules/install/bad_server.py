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
    def __init__(self, stop_event, with_wrong_install_id, *args, **kwargs):
        self.stop_event = stop_event
        self.with_wrong_install_id = with_wrong_install_id

    def Install(self, request, _context):
        install_id = request.install_id
        files = request.files

        print(
            f"Received request with install info: install_id= {install_id} and files= {files}"
        )

        install_response = install_pb2.InstallResponse()
        install_response.install_id = (
            "mock_install_id" if self.with_wrong_install_id else install_id
        )
        return install_response

    def FileReadyRequest(self, request, context):
        error_detail = install_pb2.ErrorDetail()
        error_detail.message = f"Mocking failing to install '{request.path}'"

        file_response = install_pb2.FileResponse()
        file_response.name = request.name
        file_response.path = request.path
        file_response.install_id = request.install_id
        file_response.error_details = error_detail
        return file_response

    def ShutdownServer(self, request, context):
        shutdown(self.stop_event)
        response = install_pb2.ShutdownResponse()
        response.install_id = request.install_id
        return response


def shutdown(stop_event):
    stop_event.set()


def serve(args):
    server = grpc.server(thread_pool=ThreadPoolExecutor(max_workers=1))
    stop_event = threading.Event()
    install_pb2_grpc.add_InstallerServicer_to_server(
        BadInstallerService(stop_event, args.with_wrong_install_id), server
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
    parser.add_argument(
        "--with_wrong_install_id",
        required=False,
        action="store_true",
        help="flag that if set then need to produce a wrong install id response",
    )
    return parser.parse_args(args)


if __name__ == "__main__":
    args = parse_args(sys.argv[1:])
    serve(args)
