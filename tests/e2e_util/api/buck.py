#!/usr/bin/env fbpython
# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

# pyre-unsafe

import os
import uuid
from asyncio import subprocess
from pathlib import Path
from typing import Dict, Iterable, Optional, Tuple

from buck2.tests.e2e_util.api.buck_result import (
    AuditConfigResult,
    BuckException,
    BuckExceptionType,
    BuckResult,
    BuckResultType,
    BuildResult,
    BxlResult,
    TargetsResult,
    TestResult,
)
from buck2.tests.e2e_util.api.executable import Executable
from buck2.tests.e2e_util.api.executable_type import ExecutableType
from buck2.tests.e2e_util.api.lsp import LspClient
from buck2.tests.e2e_util.api.process import Process
from buck2.tests.e2e_util.api.result import E, R, Result
from buck2.tests.e2e_util.api.subscribe import SubscribeClient


class Buck(Executable):
    """Instantiates a Buck object with a executable path"""

    def __init__(
        self,
        executable_type: ExecutableType,
        path_to_executable: Path,
        encoding: str,
        env: Dict[str, str],
        cwd: Optional[Path] = None,
        isolation_prefix: Optional[str] = None,
    ) -> None:
        super().__init__(executable_type, path_to_executable, encoding, env, cwd)
        self.set_buckd(False)
        self.isolation_prefix = isolation_prefix

    def set_buckd(self, toggle: bool) -> None:
        """
        Setting buckd env to value of toggle.
        toggle can be 0 for enabled and 1 for disabled
        """
        self._env["NO_BUCKD"] = str(int(toggle))

    def set_isolation_prefix(self, isolation_prefix: str) -> None:
        self.isolation_prefix = isolation_prefix

    def _get_cwd(self, rel_cwd: Optional[Path]) -> Path:
        if rel_cwd is None:
            return self.cwd
        abs_cwd = self.cwd / rel_cwd
        assert abs_cwd.exists()
        return abs_cwd

    def build(
        self,
        *argv: str,
        input: Optional[bytes] = None,
        rel_cwd: Optional[Path] = None,
        env: Optional[Dict[str, str]] = None,
    ) -> Process[BuildResult, BuckException]:
        """
        Returns a Process with BuildResult type using a process
        created with the build command and any
        additional arguments.

        rel_cwd: Optional Path specifying the workding directive to run
        the command relative to the root.
        env: Optional dictionary for environment variables to run command with.
        """
        args = list(argv)
        if (
            self.executable_type
            in (ExecutableType.buck2, ExecutableType.buck2_build_api_binary)
            and "--print-build-report" not in args
        ):
            args.append("--print-build-report")

        return self._run_buck_command(
            "build",
            *args,
            input=input,
            rel_cwd=rel_cwd,
            env=env,
            result_type=lambda proc, stdout, stderr, buck_build_id: BuildResult(
                proc, stdout, stderr, buck_build_id, self.executable_type, *args
            ),
            exception_type=BuckException,
        )

    def build_without_report(
        self,
        *argv: str,
        input: Optional[bytes] = None,
        rel_cwd: Optional[Path] = None,
        env: Optional[Dict[str, str]] = None,
    ) -> Process[BuckResult, BuckException]:
        """
        Returns a Process with BuckResult type using a process
        created with the build command and any
        additional arguments.

        rel_cwd: Optional Path specifying the workding directive to run
        the command relative to the root.
        env: Optional dictionary for environment variables to run command with.
        """

        return self._run_buck_command(
            "build",
            *argv,
            input=input,
            rel_cwd=rel_cwd,
            env=env,
            result_type=BuckResult,
            exception_type=BuckException,
        )

    def help(
        self,
        *argv: str,
        input: Optional[bytes] = None,
        rel_cwd: Optional[Path] = None,
        env: Optional[Dict[str, str]] = None,
    ) -> Process[BuckResult, BuckException]:
        return self._run_buck_command(
            "help",
            *argv,
            input=input,
            rel_cwd=rel_cwd,
            env=env,
            result_type=BuckResult,
            exception_type=BuckException,
        )

    def help_env(
        self,
        *argv: str,
        input: Optional[bytes] = None,
        rel_cwd: Optional[Path] = None,
        env: Optional[Dict[str, str]] = None,
    ) -> Process[BuckResult, BuckException]:
        return self._run_buck_command(
            "help-env",
            *argv,
            input=input,
            rel_cwd=rel_cwd,
            env=env,
            result_type=BuckResult,
            exception_type=BuckException,
        )

    def run(
        self,
        *argv: str,
        input: Optional[bytes] = None,
        rel_cwd: Optional[Path] = None,
        env: Optional[Dict[str, str]] = None,
    ) -> Process[BuckResult, BuckException]:
        """
        Returns a Process with BuckResult type using a process
        created with the build command and any
        additional arguments

        rel_cwd: Optional Path specifying the workding directive to run
        the command relative to the root.
        env: Optional dictionary for environment variables to run command with.
        """
        return self._run_buck_command(
            "run",
            *argv,
            input=input,
            rel_cwd=rel_cwd,
            env=env,
            result_type=BuckResult,
            exception_type=BuckException,
        )

    def clean(
        self,
        *argv: str,
        input: Optional[bytes] = None,
        rel_cwd: Optional[Path] = None,
        env: Optional[Dict[str, str]] = None,
    ) -> Process[BuckResult, BuckException]:
        """
        Returns a Process with BuckResult type using a process
        created with the clean command and any
        additional arguments

        rel_cwd: Optional Path specifying the workding directive to run
        the command relative to the root.
        env: Optional dictionary for environment variables to run command with.
        """
        return self._run_buck_command(
            "clean",
            *argv,
            input=input,
            rel_cwd=rel_cwd,
            env=env,
            result_type=BuckResult,
            exception_type=BuckException,
        )

    def root(
        self,
        *argv: str,
        input: Optional[bytes] = None,
        rel_cwd: Optional[Path] = None,
        env: Optional[Dict[str, str]] = None,
    ) -> Process[BuckResult, BuckException]:
        """
        Returns a Process with BuckResult type using a process
        created with the root command

        kind: --kind argument to the root command
        rel_cwd: Optional Path specifying the workding directory to run
        the command relative to the root.
        env: Optional dictionary for environment variables to run command with.
        """
        return self._run_buck_command(
            "root",
            *argv,
            input=input,
            rel_cwd=rel_cwd,
            env=env,
            result_type=BuckResult,
            exception_type=BuckException,
        )

    def kill(
        self,
        input: Optional[bytes] = None,
        rel_cwd: Optional[Path] = None,
        env: Optional[Dict[str, str]] = None,
    ) -> Process[BuckResult, BuckException]:
        """
        Returns a Process with BuckResult type using a process
        created with the kill command

        rel_cwd: Optional Path specifying the workding directive to run
        the command relative to the root.
        env: Optional dictionary for environment variables to run command with.
        """
        return self._run_buck_command(
            "kill",
            input=input,
            rel_cwd=rel_cwd,
            env=env,
            result_type=BuckResult,
            exception_type=BuckException,
        )

    def test(
        self,
        *argv: str,
        input: Optional[bytes] = None,
        rel_cwd: Optional[Path] = None,
        env: Optional[Dict[str, str]] = None,
    ) -> Process[TestResult, BuckException]:
        """
        Returns a Process with TestResult type using a process
        created with the test command and any
        additional arguments

        rel_cwd: Optional Path specifying the workding directive to run
        the command relative to the root.
        env: Optional dictionary for environment variables to run command with.
        """
        xml_flag, test_output_file = self._create_xml_file()

        buck2_tpx = os.environ.get("BUCK2_TPX")
        if buck2_tpx is not None:
            patched_argv = [
                "--config",
                "test.v2_test_executor={}".format(buck2_tpx),
                *argv,
            ]
        else:
            patched_argv = list(argv)

        return self._run_buck_command(
            "test",
            *xml_flag,
            *patched_argv,
            input=input,
            rel_cwd=rel_cwd,
            env=env,
            result_type=lambda proc, stdout, stderr, buck_build_id: TestResult(
                proc, stdout, stderr, buck_build_id, self.cwd / test_output_file
            ),
            exception_type=BuckException,
        )

    def targets(
        self,
        *argv: str,
        input: Optional[bytes] = None,
        rel_cwd: Optional[Path] = None,
        env: Optional[Dict[str, str]] = None,
    ) -> Process[TargetsResult, BuckException]:
        """
        Returns a Process with BuckResult type using a process
        created with the targets command and any
        additional arguments

        rel_cwd: Optional Path specifying the workding directive to run
        the command relative to the root.
        env: Optional dictionary for environment variables to run command with.

        TODO: Add a TargetsResult with structured output.
        """

        args = list(argv)

        return self._run_buck_command(
            "targets",
            *argv,
            input=input,
            rel_cwd=rel_cwd,
            env=env,
            result_type=lambda proc, stdout, stderr, buck_build_id: TargetsResult(
                proc, stdout, stderr, buck_build_id, *args
            ),
            exception_type=BuckException,
        )

    def ctargets(
        self,
        *argv,
        input: Optional[bytes] = None,
        rel_cwd: Optional[Path] = None,
        env: Optional[Dict[str, str]] = None,
    ) -> Process[BuckResult, BuckException]:
        return self._run_buck_command(
            "ctargets",
            *argv,
            input=input,
            rel_cwd=rel_cwd,
            env=env,
            result_type=BuckResult,
            exception_type=BuckException,
        )

    def audit_config(
        self,
        *argv: str,
        input: Optional[bytes] = None,
        rel_cwd: Optional[Path] = None,
        env: Optional[Dict[str, str]] = None,
    ) -> Process[AuditConfigResult, BuckException]:
        """
        Returns a Process with AuditConfigResult type using a process
        created with the audit_config command

        rel_cwd: Optional Path specifying the workding directive to run
        the command relative to the root.
        env: Optional dictionary for environment variables to run command with.
        """
        args = list(argv)
        return self._run_buck_command(
            "audit",
            "config",
            *args,
            input=input,
            rel_cwd=rel_cwd,
            env=env,
            result_type=lambda proc, stdout, stderr, buck_build_id: AuditConfigResult(
                proc, stdout, stderr, buck_build_id, *args
            ),
            exception_type=BuckException,
        )

    def audit_configurations(
        self,
        *argv: str,
        input: Optional[bytes] = None,
        rel_cwd: Optional[Path] = None,
        env: Optional[Dict[str, str]] = None,
    ) -> Process[BuckResult, BuckException]:
        args = list(argv)
        return self._run_buck_command(
            "audit",
            "configurations",
            *args,
            input=input,
            rel_cwd=rel_cwd,
            env=env,
            result_type=BuckResult,
            exception_type=BuckException,
        )

    def audit_dep_files(
        self,
        *argv: str,
        input: Optional[bytes] = None,
        rel_cwd: Optional[Path] = None,
        env: Optional[Dict[str, str]] = None,
    ) -> Process[BuckResult, BuckException]:
        args = list(argv)
        return self._run_buck_command(
            "audit",
            "dep-files",
            *args,
            input=input,
            rel_cwd=rel_cwd,
            env=env,
            result_type=BuckResult,
            exception_type=BuckException,
        )

    def audit_visibility(
        self,
        *argv: str,
        input: Optional[bytes] = None,
        rel_cwd: Optional[Path] = None,
        env: Optional[Dict[str, str]] = None,
    ) -> Process[BuckResult, BuckException]:
        args = list(argv)
        return self._run_buck_command(
            "audit",
            "visibility",
            *args,
            input=input,
            rel_cwd=rel_cwd,
            env=env,
            result_type=BuckResult,
            exception_type=BuckException,
        )

    def audit(
        self,
        *argv: str,
        input: Optional[bytes] = None,
        rel_cwd: Optional[Path] = None,
        env: Optional[Dict[str, str]] = None,
    ) -> Process[BuckResult, BuckException]:
        args = list(argv)
        return self._run_buck_command(
            "audit",
            *args,
            input=input,
            rel_cwd=rel_cwd,
            env=env,
            result_type=BuckResult,
            exception_type=BuckException,
        )

    def audit_output(
        self,
        *argv: str,
        input: Optional[bytes] = None,
        rel_cwd: Optional[Path] = None,
        env: Optional[Dict[str, str]] = None,
    ) -> Process[AuditConfigResult, BuckException]:
        args = list(argv)
        return self._run_buck_command(
            "audit",
            "output",
            *args,
            input=input,
            rel_cwd=rel_cwd,
            env=env,
            result_type=BuckResult,
            exception_type=BuckException,
        )

    def query(
        self,
        *argv: str,
        input: Optional[bytes] = None,
        rel_cwd: Optional[Path] = None,
        env: Optional[Dict[str, str]] = None,
    ) -> Process[BuckResult, BuckException]:
        return self._query("query", *argv, rel_cwd=rel_cwd, env=env)

    def cquery(
        self,
        *argv: str,
        input: Optional[bytes] = None,
        rel_cwd: Optional[Path] = None,
        env: Optional[Dict[str, str]] = None,
    ) -> Process[BuckResult, BuckException]:
        return self._query("cquery", *argv, rel_cwd=rel_cwd, env=env)

    def uquery(
        self,
        *argv: str,
        input: Optional[bytes] = None,
        rel_cwd: Optional[Path] = None,
        env: Optional[Dict[str, str]] = None,
    ) -> Process[BuckResult, BuckException]:
        return self._query("uquery", *argv, rel_cwd=rel_cwd, env=env)

    def aquery(
        self,
        *argv: str,
        input: Optional[bytes] = None,
        rel_cwd: Optional[Path] = None,
        env: Optional[Dict[str, str]] = None,
    ) -> Process[BuckResult, BuckException]:
        return self._query("aquery", *argv, rel_cwd=rel_cwd, env=env)

    def _query(
        self,
        query_command: str,
        *argv: str,
        input: Optional[bytes] = None,
        rel_cwd: Optional[Path] = None,
        env: Optional[Dict[str, str]] = None,
    ) -> Process[BuckResult, BuckException]:
        """
        Returns a Process with BuckResult type using a process
        created with the query command and any
        additional arguments

        rel_cwd: Optional Path specifying the workding directive to run
        the command relative to the root.
        env: Optional dictionary for environment variables to run command with.

        TODO: Add a QueryResult with structured output.
        """
        return self._run_buck_command(
            query_command,
            *argv,
            input=input,
            rel_cwd=rel_cwd,
            env=env,
            result_type=BuckResult,
            exception_type=BuckException,
        )

    def bxl(
        self,
        *argv: str,
        input: Optional[bytes] = None,
        rel_cwd: Optional[Path] = None,
        env: Optional[Dict[str, str]] = None,
    ) -> Process[BxlResult, BuckException]:
        args = list(argv)
        return self._run_buck_command(
            "bxl",
            *args,
            input=input,
            rel_cwd=rel_cwd,
            env=env,
            result_type=lambda proc, stdout, stderr, buck_build_id: BxlResult(
                proc, stdout, stderr, buck_build_id, *args
            ),
            exception_type=BuckException,
        )

    def docs_starlark(
        self,
        query_patterns: Iterable[str],
        builtins: bool = True,
        prelude: bool = False,
        output_format: str = "json",
        input: Optional[bytes] = None,
        rel_cwd: Optional[Path] = None,
        env: Optional[Dict[str, str]] = None,
    ) -> Process[BuckResult, BuckException]:
        """
        Returns a Process with BuckResult type using a process
        created with the docs command and any
        additional arguments

        query_patterns: The patterns, if any, to ask for docs for
        builtins: Whether to pass the --builtins flag
        prelude: Whether to pass the --prelude flag
        output_format: How to get documentation back
        rel_cwd: Optional Path specifying the workding directive to run
        the command relative to the root.
        env: Optional dictionary for environment variables to run command with.
        """
        args = ["--format", output_format]
        if builtins:
            args.append("--builtins")
        if prelude:
            args.append("--prelude")
        args.extend(query_patterns)
        return self._run_buck_command(
            "docs",
            "starlark",
            *args,
            input=input,
            rel_cwd=rel_cwd,
            env=env,
            result_type=BuckResult,
            exception_type=BuckException,
        )

    def profile(
        self,
        *args: str,
        input: Optional[bytes] = None,
        rel_cwd: Optional[Path] = None,
        env: Optional[Dict[str, str]] = None,
    ) -> Process[BuckResult, BuckException]:
        """
        Returns a Process with BuckResult type using a process created with the
        profile command and any additional arguments

        args: Arguments to pass to buck2 profile.
        rel_cwd: Optional Path specifying the workding directive to run
        the command relative to the root.
        env: Optional dictionary for environment variables to run command with.
        """
        return self._run_buck_command(
            "profile",
            *args,
            input=input,
            rel_cwd=rel_cwd,
            env=env,
            result_type=BuckResult,
            exception_type=BuckException,
        )

    def debug(
        self,
        *args: str,
        input: Optional[bytes] = None,
        rel_cwd: Optional[Path] = None,
        env: Optional[Dict[str, str]] = None,
    ) -> Process[BuckResult, BuckException]:
        """
        Returns a Process with BuckResult type using a process created with the
        debug command and any additional arguments
        """
        return self._run_buck_command(
            "debug",
            *args,
            input=input,
            rel_cwd=rel_cwd,
            env=env,
            result_type=BuckResult,
            exception_type=BuckException,
        )

    def starlark(
        self,
        *args: str,
        input: Optional[bytes] = None,
        rel_cwd: Optional[Path] = None,
        env: Optional[Dict[str, str]] = None,
    ) -> Process[BuckResult, BuckException]:
        """
        Returns a Process with BuckResult type using a process created with the
        debug command and any additional arguments
        """
        return self._run_buck_command(
            "starlark",
            *args,
            input=input,
            rel_cwd=rel_cwd,
            env=env,
            result_type=BuckResult,
            exception_type=BuckException,
        )

    def install(
        self,
        *argv: str,
        input: Optional[bytes] = None,
        rel_cwd: Optional[Path] = None,
        env: Optional[Dict[str, str]] = None,
    ) -> Process[BuckResult, BuckException]:
        return self._run_buck_command(
            "install",
            *argv,
            input=input,
            rel_cwd=rel_cwd,
            env=env,
            result_type=BuckResult,
            exception_type=BuckException,
        )

    def log(
        self,
        *args: str,
        input: Optional[bytes] = None,
        rel_cwd: Optional[Path] = None,
        env: Optional[Dict[str, str]] = None,
    ) -> Process[BuckResult, BuckException]:
        return self._run_buck_command(
            "log",
            *args,
            input=input,
            rel_cwd=rel_cwd,
            env=env,
            result_type=BuckResult,
            exception_type=BuckException,
        )

    def status(
        self,
        *args: str,
        input: Optional[bytes] = None,
        rel_cwd: Optional[Path] = None,
        env: Optional[Dict[str, str]] = None,
    ) -> Process[BuckResult, BuckException]:
        return self._run_buck_command(
            "status",
            *args,
            input=input,
            rel_cwd=rel_cwd,
            env=env,
            result_type=BuckResult,
            exception_type=BuckException,
        )

    def server(
        self,
        *args: str,
        input: Optional[bytes] = None,
        rel_cwd: Optional[Path] = None,
        env: Optional[Dict[str, str]] = None,
    ) -> Process[BuckResult, BuckException]:
        return self._run_buck_command(
            "server",
            *args,
            input=input,
            rel_cwd=rel_cwd,
            env=env,
            result_type=BuckResult,
            exception_type=BuckException,
        )

    async def lsp(
        self,
        *args: str,
        rel_cwd: Optional[Path] = None,
        env: Optional[Dict[str, str]] = None,
    ) -> LspClient:
        process = await self._run_buck_command(
            "lsp",
            *args,
            input=None,
            stdin=subprocess.PIPE,
            rel_cwd=rel_cwd,
            env=env,
            result_type=BuckResult,
            exception_type=BuckException,
            intercept_stderr=False,
        ).start()
        cwd = self._get_cwd(rel_cwd)
        return LspClient(process, cwd)

    async def subscribe(
        self,
        *args: str,
        rel_cwd: Optional[Path] = None,
        env: Optional[Dict[str, str]] = None,
    ) -> SubscribeClient:
        process = self._run_buck_command(
            "subscribe",
            "--unstable-json",
            *args,
            input=None,
            stdin=subprocess.PIPE,
            rel_cwd=rel_cwd,
            env=env,
            result_type=BuckResult,
            exception_type=BuckException,
            intercept_stderr=False,
        )
        client = await SubscribeClient.create(process)
        return client

    def _run_buck_command(
        self,
        cmd: str,
        *argv: str,
        input: Optional[bytes],
        rel_cwd: Optional[Path],
        env: Optional[Dict[str, str]],
        result_type: BuckResultType,
        exception_type: BuckExceptionType,
        stdin: Optional[int] = None,
        intercept_stderr: bool = True,
        # pyre-fixme[34]: `Variable[E (bound to Exception)]` isn't present in the
        #  function's parameters.
        # pyre-fixme[34]: `Variable[R (bound to
        #  buck2.tests.e2e_util.api.result.Result)]` isn't present in the function's
        #  parameters.
    ) -> Process[R, E]:
        """
        Returns a process created from the execuable path,
        command and any additional arguments
        """
        buck_build_id = str(uuid.uuid1())
        command_env = self._get_command_env(env)
        if (
            self.executable_type == ExecutableType.buck1
            and "BUCK_BUILD_ID" not in command_env
        ):
            command_env["BUCK_BUILD_ID"] = buck_build_id
        if (
            self.executable_type == ExecutableType.buck2
            and "BUCK_WRAPPER_UUID" not in command_env
        ):
            command_env["BUCK_WRAPPER_UUID"] = buck_build_id

        cmd_to_run = (
            [
                str(self.path_to_executable),
                "--dir",
                str(self.cwd),
            ]
            if self.executable_type == ExecutableType.buck2_build_api_binary
            else [str(self.path_to_executable), cmd]
        )
        if self.isolation_prefix:
            if self.executable_type == ExecutableType.buck1:
                cmd_to_run.extend(
                    ["--isolation_prefix", "buck-out/" + str(self.isolation_prefix)]
                )
            elif self.executable_type == ExecutableType.buck2:
                cmd_to_run = [
                    cmd_to_run[0],
                    "--isolation-dir",
                    str(self.isolation_prefix),
                    *cmd_to_run[1:],
                ]
            elif self.executable_type == ExecutableType.buck2_build_api_binary:
                cmd_to_run.extend(
                    ["--buck-out", "buck-out/" + str(self.isolation_prefix)]
                )
        cmd_to_run.extend(argv)
        cmd_to_run = self._get_windows_cmd_options() + cmd_to_run
        stderr = subprocess.PIPE if intercept_stderr else None
        return Process(
            cmd_to_run=cmd_to_run,
            working_dir=self._get_cwd(rel_cwd),
            env=command_env,
            input=input,
            stdin=stdin,
            stdout=subprocess.PIPE,
            stderr=stderr,
            result_type=lambda proc, stdout, stderr: result_type(
                proc, stdout, stderr, buck_build_id
            ),
            exception_type=lambda cmd_to_run, working_dir, env, proc, stdout, stderr: exception_type(
                cmd_to_run, working_dir, env, proc, stdout, stderr, buck_build_id
            ),
            encoding=self.encoding,
        )

    def _create_xml_file(self, *argv: str) -> Tuple[Iterable[str], str]:
        """
        Creates a xml file used for the test output. Ensures an xml file
        is created if not specified.
        """
        xml_flag = [""]
        test_output_file = "testOutput.xml"
        # ensures xml file is always generated
        if "--xml" not in argv:
            xml_flag = ["--xml", "testOutput.xml"]
        else:
            test_output_file = argv[argv.index("--xml") + 1]
        return xml_flag, test_output_file

    def execute(
        self,
        *argv: str,
        env: Optional[Dict[str, str]] = None,
        input: Optional[bytes] = None,
        stdin: Optional[int] = None,
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
    ) -> Process[Result, Exception]:
        raise NotImplementedError("Buck does not use execute.")

    def rage(
        self,
        *argv: str,
        input: Optional[bytes] = None,
        rel_cwd: Optional[Path] = None,
        env: Optional[Dict[str, str]] = None,
    ) -> Process[BuckResult, BuckException]:
        return self._run_buck_command(
            "rage",
            *argv,
            input=input,
            rel_cwd=rel_cwd,
            env=env,
            result_type=BuckResult,
            exception_type=BuckException,
        )
