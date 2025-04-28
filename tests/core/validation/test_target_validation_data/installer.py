# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

import argparse
import socket
import sys


def parse_args(args):
    parser = argparse.ArgumentParser()
    parser.add_argument(
        "--tcp-port",
        type=int,
        help="tcp port for installer to connect to",
        required=True,
    )
    args, _ = parser.parse_known_args(args)
    return args


def main() -> None:
    args = parse_args(sys.argv[1:])
    sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
    address = ("localhost", args.tcp_port)
    print(f"Installer: Binding to port {address[1]}")
    sock.bind(address)
    sock.listen(1)
    connection, _ = sock.accept()
    print("Installer: Incoming connection accepted, now closing it")
    connection.close()


if __name__ == "__main__":
    main()
