#!/usr/bin/env python3

import argparse
import os
import signal
import subprocess
import sys
import threading
from concurrent.futures import ThreadPoolExecutor
from typing import Optional

import grpc
from buck2.install_proto import install_pb2, install_pb2_grpc


class RsyncInstallerService(install_pb2_grpc.InstallerServicer):
    def __init__(self, stop_event, argsparse, *args, **kwargs):
        self.args = argsparse
        self.stop_event = stop_event
        if argsparse.install_location == "":
            self.dst = argsparse.dst
        else:
            self.dst = f"{argsparse.install_location}:{argsparse.dst}"

    def FileReadyRequest(self, request, context):
        (_out, stderr, code) = self.rsync_install(request.path, self.dst)
        response = {
            "name": f"{request.name}",
            "path": request.path,
        }
        file_response = install_pb2.FileResponse(**response)

        if code != 0:
            error_detail = install_pb2.ErrorDetail()
            error_detail.message = stderr
            file_response.error_detail = error_detail

        return file_response

    def ShutdownServer(self, request, context):
        shutdown(self.stop_event)
        response = {}
        return install_pb2.ShutdownResponse(**response)

    def rsync_install(self, src, dst):
        cmd = [
            "rsync",
            "-a",
            str(src),
            str(dst),
        ]
        cp = subprocess.Popen(
            cmd, stdout=subprocess.PIPE, stderr=subprocess.PIPE, encoding="utf8"
        )
        stdout, stderr = cp.communicate()
        code = cp.returncode
        return (stdout, stderr, code)


def try_command(
    cmd: [str],
    err_msg: str,
    cwd: Optional = None,
    env: Optional = None,
    shell: bool = False,
):
    try:
        output = subprocess.check_output(cmd, cwd=cwd, env=env, shell=shell)
        return output
    except Exception as e:
        print(f"Failed step {err_msg} with {str(e)}")
        raise e


def shutdown(stop_event):
    stop_event.set()


def serve(args):
    server = grpc.server(thread_pool=ThreadPoolExecutor(max_workers=1))
    stop_event = threading.Event()
    install_pb2_grpc.add_InstallerServicer_to_server(
        RsyncInstallerService(stop_event, args), server
    )
    ## https://grpc.github.io/grpc/python/grpc.html
    listen_addr = server.add_insecure_port(f"unix://{args.named_pipe}")
    print(f"{args.named_pipe}")
    print(f"Starting server on {listen_addr} w/ pid {os.getpid()}")
    server.start()
    signal.signal(signal.SIGINT, lambda x, y: shutdown(stop_event))
    stop_event.wait()
    print("Stopped RPC server, Waiting for RPCs to complete...")
    server.stop(1).wait()
    print("Done stopping server")


def parse_args(args=None):
    parser = argparse.ArgumentParser(description="Parse args for install location")
    parser.add_argument(
        "--install-location",
        help="Defines install hostname (I.E. devserver)",
        default="",
    )
    parser.add_argument(
        "--dst",
        type=str,
        help="destination rsync target folder",
        default="/tmp/",
    )
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
