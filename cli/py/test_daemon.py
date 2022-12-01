# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

"""some tests for the buck daemon.

To run these, first generate python code from the daemon protobuf definition.
See https://grpc.io/docs/languages/python/quickstart/

It also needs python-daemon available: https://pypi.org/project/python-daemon/

Then from the buck2 root run:
```
cargo build --bin=buck2 --release
python3 -m grpc_tools.protoc -Icli_proto --python_out=/tmp/daemon_pb2_out --grpc_python_out=/tmp/daemon_pb2_out cli_proto/daemon.proto
pytest cli/py/test_daemon.py

```

"""


import contextlib
import json
import os
import pathlib
import random
import shutil
import subprocess
import sys
import time
from datetime import datetime

import pytest

# These are unused, but we want to catch the error in the tests rather than when running the server
proto_out = "/tmp/daemon_pb2_out"
try:
    # Imports using buck test
    from buck2.cli.py import daemon_pb2, daemon_pb2_grpc  # noqa F401  # noqa F401
except ImportError:
    # Imports using generic python
    sys.path.insert(0, proto_out)
    import daemon_pb2  # noqa F401
    import daemon_pb2_grpc  # noqa F401
except Exception:
    raise AssertionError(f"Expected daemon protobuf/grpc output in {proto_out}")


def _env(env, timeout):
    return {
        **os.environ,
        **(env or {}),
        "BUCKD_STARTUP_TIMEOUT": str(timeout),
        "BUCK2_TEST_DISABLE_LOG_UPLOAD": "true",
    }


def check_output(*args, timeout=60, env=None, **kwargs):
    return (
        subprocess.check_output(
            *args, timeout=timeout, env=_env(env, timeout), **kwargs
        )
        .decode()
        .strip()
    )


def check_call(*args, timeout=60, env=None, **kwargs):
    subprocess.check_call(*args, timeout=timeout, env=_env(env, timeout), **kwargs)


@pytest.fixture
def buck():
    isolation_prefix = ".buckd.test.%06d" % random.randint(0, 999999)
    isolation_arg = "--isolation-dir=%s" % isolation_prefix
    exe_path = pathlib.Path(os.environ.get("TEST_EXECUTABLE") or "target/release/buck2")
    exe_prefix = [exe_path, isolation_arg]

    isolation_dir = check_output(exe_prefix + ["root", "-k", "daemon"])
    yield (exe_prefix, isolation_dir)
    if os.path.exists(isolation_dir):
        shutil.rmtree(isolation_dir)


class TestingDaemon(object):
    def __init__(self, pid):
        self.pid = pid

    def is_alive(self):
        try:
            os.kill(self.pid, 0)
            return True
        except Exception:
            # not quite right, we should look for a specific error, but good enough
            return False

    def kill(self):
        os.kill(self.pid, 9)


@contextlib.contextmanager
def start_testing_daemon(
    isolation_dir, kill_delay=0, shutdown_delay=0, version="mismatched_by_default"
):
    isolation_dir = pathlib.Path(isolation_dir)
    try:
        if os.path.exists(isolation_dir):
            shutil.rmtree(isolation_dir)
        os.makedirs(isolation_dir)
        subprocess.check_call(
            [
                os.environ.get("SCRIPT") or "cli/py/server.py",
                "--daemon-dir=%s" % isolation_dir,
                "--kill-delay=%d" % kill_delay,
                "--shutdown-delay=%d" % shutdown_delay,
                "--version=%s" % version,
            ]
        )
        info_path = isolation_dir / "buckd.info"
        start = datetime.now()
        daemon = None
        last_exception = None
        while daemon is None and (datetime.now() - start).total_seconds() < 3:
            try:
                with open(info_path) as infofile:
                    info = json.load(infofile)
                    daemon = TestingDaemon(info["pid"])
            except Exception as e:
                last_exception = e
                pass
            time.sleep(0.1)
        assert daemon is not None, "couldn't start testing daemon (%s)" % last_exception

        yield daemon
    finally:
        try:
            daemon.kill()
        except Exception:
            pass


def get_status(buck_exe):
    status = check_output(buck_exe + ["status"])
    return json.loads(status)


def wait_for_exit(pid, timeout=5):
    start = datetime.now()
    print(start)
    while (datetime.now() - start).total_seconds() < timeout:
        try:
            os.kill(pid, 0)
        except ProcessLookupError:
            return
    raise AssertionError()


def test_server(buck):
    buck_exe, isolation_dir = buck
    port = check_output(buck_exe + ["server"])
    print(port)


def test_status(buck):
    buck_exe, isolation_dir = buck
    # run `buck2 server` to start daemon
    check_call(buck_exe + ["server"])
    status = check_output(buck_exe + ["status"])
    print(status)


def test_kill(buck):
    buck_exe, isolation_dir = buck
    check_call(buck_exe + ["server"])
    pid = get_status(buck_exe)["process_info"]["pid"]
    check_call(buck_exe + ["kill"])
    wait_for_exit(pid)


# tests we can kill the testing daemon
def test_testing_server_kill(buck):
    buck_exe, isolation_dir = buck
    with start_testing_daemon(isolation_dir) as daemon:
        assert daemon.is_alive(), "daemon should be alive"
        check_call(buck_exe + ["kill"])
        # server should be killed before "buck kill" returns
        assert not daemon.is_alive()


def test_version_mismatch(buck):
    buck_exe, isolation_dir = buck
    # --version outputs "buck <version> <local>"
    expected_version = check_output(buck_exe + ["--version"]).split(" ", 2)
    expected_version = expected_version[1]
    print(expected_version)
    with start_testing_daemon(isolation_dir, version="notbuck") as daemon:
        # current version should be from testing daemon
        assert get_status(buck_exe)["process_info"]["version"] == "notbuck"

        # `buck server` should enforce that there's a running buckd with matching version
        output = check_output(buck_exe + ["server"])
        print(output)

        # so should kill our sever
        assert not daemon.is_alive(), "daemon should be dead"

        # and have matching version
        assert expected_version != "notbuck"
        assert get_status(buck_exe)["process_info"]["version"] == expected_version


def test_version_match(buck):
    buck_exe, isolation_dir = buck
    # --version outputs "buck <version> <local>"
    expected_version = check_output(buck_exe + ["--version"]).split(" ", 2)
    expected_version = expected_version[1]
    print(expected_version)
    with start_testing_daemon(isolation_dir, version=expected_version) as daemon:
        # current version should be from testing daemon
        status = get_status(buck_exe)
        print(status)
        assert status["process_info"]["version"] == expected_version
        status = get_status(buck_exe)
        print(status)
        status = get_status(buck_exe)
        print(status)
        # `buck server` should enforce that there's a running buckd with matching version
        output = subprocess.check_output(buck_exe + ["status"])
        output = subprocess.check_output(buck_exe + ["server"])
        print(output)
        # so should not kill our sever and should be able to get status
        assert get_status(buck_exe)["process_info"] == status["process_info"]
        assert daemon.is_alive(), "daemon should be alive"


def test_kill_hanging_on_kill(buck):
    buck_exe, isolation_dir = buck
    # kill_delay forever here
    with start_testing_daemon(isolation_dir, kill_delay=1e8) as daemon:
        assert daemon.is_alive(), "daemon should be alive"
        check_call(buck_exe + ["kill"], timeout=30)
        # server should be killed before "buck kill" returns
        assert not daemon.is_alive()


def test_kill_delay_on_shutdown(buck):
    buck_exe, isolation_dir = buck
    with start_testing_daemon(isolation_dir, shutdown_delay=300) as daemon:
        assert daemon.is_alive(), "daemon should be alive"
        check_call(buck_exe + ["kill"], timeout=30)
        # server should be killed before "buck kill" returns
        assert not daemon.is_alive()


def test_kill_hanging_on_shutdown(buck):
    buck_exe, isolation_dir = buck
    with start_testing_daemon(isolation_dir, shutdown_delay=1e30) as daemon:
        assert daemon.is_alive(), "daemon should be alive"
        check_call(buck_exe + ["kill"], timeout=30)
        # server should be killed before "buck kill" returns
        assert not daemon.is_alive()
