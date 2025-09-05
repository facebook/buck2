# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

import threading

import grpc
from buck2.app.buck2_forkserver_proto import forkserver_pb2, forkserver_pb2_grpc


class Buck2ForkserverClient:
    def __init__(self, socket_path, cmd_cwd: str = "/tmp"):
        self.socket_path = socket_path
        self.cmd_cwd = cmd_cwd
        self.channel = None
        self.stub = None

    def connect(self):
        try:
            print(f"Connecting to forkserver at {self.socket_path}")
            self.channel = grpc.insecure_channel(
                self.socket_path,
            )

            self.stub = forkserver_pb2_grpc.ForkserverStub(self.channel)

        except Exception as e:
            print(f"✗ Failed to connect: {e}")
            print(e)
            if self.channel:
                self.channel.close()
                self.channel = None
            raise RuntimeError(f"Failed to connect to forkserver: {e}")

    def disconnect(self):
        if self.channel:
            self.channel.close()
            self.channel = None
            self.stub = None

    def set_log_filter(self, log_filter):
        if not self.stub:
            raise RuntimeError("Not connected to forkserver. Call connect() first.")

        request = forkserver_pb2.SetLogFilterRequest(log_filter=log_filter)

        try:
            response = self.stub.SetLogFilter(request)
            res = response
            return res
        except grpc.RpcError as e:
            print(f"Failed to set log filter: {e}")
            raise e

    def run_command(
        self,
        exe,
        argv=None,
        cwd=None,
        timeout_seconds=None,
        env_directives=None,
        enable_miniperf=False,
    ):
        """Execute a command via forkserver"""
        if not self.stub:
            raise RuntimeError("Not connected to forkserver. Call connect() first.")

        print(f"Sending command: {exe} {' '.join(argv)} forkserver")

        # Create CommandRequest
        command_request = forkserver_pb2.CommandRequest(
            exe=exe.encode("utf-8"),
            argv=[arg.encode("utf-8") for arg in (argv or [])],
            enable_miniperf=enable_miniperf,
            action_digest="fake_digest",
        )

        # Set working directory if provided
        if cwd:
            command_request.cwd.path = cwd.encode("utf-8")
        else:
            command_request.cwd.path = self.cmd_cwd.encode("utf-8")

        # Set timeout if provided
        if timeout_seconds:
            command_request.timeout.seconds = int(timeout_seconds)
            command_request.timeout.nanos = int((timeout_seconds % 1) * 1e9)

        # Set environment directives if provided
        if env_directives:
            for directive in env_directives:
                env_directive = forkserver_pb2.EnvDirective()
                if directive["type"] == "clear":
                    env_directive.clear.CopyFrom(forkserver_pb2.EnvClear())
                elif directive["type"] == "remove":
                    env_directive.remove.key = directive["key"].encode("utf-8")
                elif directive["type"] == "set":
                    env_directive.set.key = directive["key"].encode("utf-8")
                    env_directive.set.value = directive["value"].encode("utf-8")
                command_request.env.append(env_directive)

        should_cancel = threading.Event()

        def request_generator():
            # First, send the command request
            yield forkserver_pb2.RequestEvent(command_request=command_request)

            while not should_cancel.wait():
                try:
                    if should_cancel.is_set():
                        # yield a cancel request
                        yield forkserver_pb2.RequestEvent(
                            cancel_request=forkserver_pb2.CancelRequest()
                        )
                        break
                except BaseException:
                    raise

        try:
            # Send request stream and get response stream
            response_stream = self.stub.Run(request_generator())

            # Accumulate results
            stdout_data = []
            stderr_data = []
            exit_code = None
            execution_stats = None
            error_reason = None

            # Process response events
            for event in response_stream:
                if event.HasField("exit"):
                    exit_code = event.exit.exit_code
                    if event.exit.execution_stats:
                        execution_stats = event.exit.execution_stats
                    # Signal generator to stop
                    break
                elif event.HasField("timeout"):
                    error_reason = f"Command timed out after: {event.timeout.duration}"
                    break
                elif event.HasField("stdout"):
                    stdout_chunk = event.stdout.data.decode("utf-8", errors="replace")
                    stdout_data.append(stdout_chunk)
                elif event.HasField("stderr"):
                    stderr_chunk = event.stderr.data.decode("utf-8", errors="replace")
                    stderr_data.append(stderr_chunk)
                elif event.HasField("cancel"):
                    error_reason = "Command was cancelled"
                    break
                elif event.HasField("spawn_failed"):
                    error_reason = (
                        f"Failed to spawn command: {event.spawn_failed.reason}"
                    )
                    break

            # Return accumulated results
            result = {
                "exit_code": exit_code,
                "stdout": "".join(stdout_data),
                "stderr": "".join(stderr_data),
                "execution_stats": execution_stats,
                "error": error_reason,
            }
            return result

        except grpc.RpcError as e:
            print(f"Failed to run command: {e}")
            raise

    def __enter__(self):
        self.connect()
        return self

    def __exit__(self, exc_type, exc_val, exc_tb):
        self.disconnect()
