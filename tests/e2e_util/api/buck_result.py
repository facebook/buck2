# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

# pyre-unsafe

import json
import textwrap
import xml.etree.ElementTree as ET
from asyncio import subprocess
from collections import defaultdict
from enum import auto, Enum
from pathlib import Path
from typing import Callable, Dict, Iterable, List, Tuple

from buck2.tests.e2e_util.api.executable_type import ExecutableType
from buck2.tests.e2e_util.api.result import E, R, Result


BuckResultType = Callable[[subprocess.Process, str, str, str], R]
BuckExceptionType = Callable[
    [Iterable[str], Path, Dict[str, str], subprocess.Process, str, str, str], E
]


class ExitCode(Enum):
    """Enum for exit codes of Buck1"""

    SUCCESS = 0
    BUILD_ERROR = 1
    BUSY = 2
    COMMANDLINE_ERROR = 3
    NOTHING_TO_DO = 4
    PARSE_ERROR = 5
    RUN_ERROR = 6
    FATAL_GENERIC = 10
    FATAL_BOOTSTRAP = 11
    FATAL_OOM = 12
    FATAL_IO = 13
    FATAL_DISK_FULL = 14
    FIX_FAILED = 16
    TEST_ERROR = 32
    TEST_NOTHING = 64
    SIGNAL_INTERRUPT = 130


class ExitCodeV2(Enum):
    """Enum for exit codes of Buck2"""

    SUCCESS = 0
    UNKNOWN_ERROR = 1
    INFRA_ERROR = 2
    USER_ERROR = 3
    DAEMON_CONNECTION_FAILURE = 11


class AutoName(Enum):
    """Makes the value of the Enum its name"""

    @staticmethod
    def _generate_next_value_(name, start, count, last_values):
        return name


class ResultType(AutoName):
    """Enum for result types of buck test"""

    DRY_RUN = auto()
    EXCLUDED = auto()
    DISABLED = auto()
    ASSUMPTION_VIOLATION = auto()
    FAILURE = auto()
    SUCCESS = auto()


class BuckResult(Result):
    """
    Represents a buck process that has finished running and succeeded.
    If the buck process failed, proceeds to raise BuckException
    """

    def __init__(
        self, process: subprocess.Process, stdout: str, stderr: str, buck_build_id: str
    ) -> None:
        super().__init__(process, stdout, stderr)
        self.buck_build_id = buck_build_id


class BuckException(Exception, BuckResult):
    """Represents a Buck process that has finished running and failed."""

    def __init__(
        self,
        cmd_to_run: Iterable[str],
        working_dir: Path,
        env: Dict[str, str],
        process: subprocess.Process,
        stdout: str,
        stderr: str,
        buck_build_id: str,
    ) -> None:
        cmd = " ".join(str(e) for e in cmd_to_run)
        indented_stderr = textwrap.indent(stderr, " " * 8)
        error_msg = (
            textwrap.dedent(
                f"""
            <cmd> {cmd}
            <working_dir> {working_dir}
            <stderr>
            """
            )
            + indented_stderr
            + "\n</stderr>"
        )
        Exception.__init__(
            self,
            error_msg,
        )
        BuckResult.__init__(self, process, stdout, stderr, buck_build_id)

    def check_returncode(self) -> None:
        assert self.process.returncode != 0

    def get_exit_code(self) -> ExitCode:
        """Returns the exit code of a Buck Result when it exits"""
        # See https://docs.python.org/3/library/subprocess.html#subprocess.Popen.returncode
        # for negative return code.
        assert self.process.returncode is not None
        if self.process.returncode < 0:  # type: ignore
            return ExitCode(128 - self.process.returncode)  # type: ignore
        return ExitCode(self.process.returncode)

    def get_exit_code_v2(self) -> ExitCodeV2:
        """Returns the exit code of a Buck Result when it exits"""
        # See https://docs.python.org/3/library/subprocess.html#subprocess.Popen.returncode
        # for negative return code.
        assert self.process.returncode is not None
        if self.process.returncode < 0:  # type: ignore
            return ExitCodeV2(128 - self.process.returncode)  # type: ignore
        return ExitCodeV2(self.process.returncode)


class BuildReport:
    """
    A parsed JSON representation of buck v2 build output on stdout.
    Build report is invoked on v2 builds by passing --print-build-report flag.
    Does not support buck v1.

    Attributes:
        build_report: A JSON dictionary parsed representation of stdout build report.
        root: A Path to the project root. Parsed from build report.
        results: A dictionary mapping targets to a tuple of output paths.
            Parsed from build report.
    """

    def __init__(self, parsed) -> None:
        assert isinstance(parsed, Dict)
        self.build_report: Dict[str, ...] = parsed  # type: ignore
        self.root = Path(self.build_report["project_root"])  # type: ignore
        self.results: Dict[str, Dict[str, ...]] = self.build_report["results"]

    def _to_abs_paths(self, paths: Tuple[Path, ...]) -> Tuple[Path, ...]:
        return tuple(self.root / path for path in paths)

    def outputs_for_target(
        self, target: str, sub_target: str = "DEFAULT", rel_path: bool = False
    ) -> Tuple[Path, ...]:
        assert "//" in target
        paths: Tuple[Path, ...]
        if target.startswith("//"):
            # Get the full target "cell//target" that matches this target.
            matched_outputs = [
                entry["outputs"][sub_target]
                for t, entry in self.results.items()
                if t.endswith(target)
            ]
            assert (
                len(matched_outputs) > 0
            ), f"Found no match for target {target} in {self.build_report}"
            assert (
                len(matched_outputs) == 1
            ), f"Found different cells for target {target} in {self.results}"
            paths = matched_outputs[0]
        else:
            paths = self.results[target]["outputs"][sub_target]
        if rel_path:
            return tuple(Path(p) for p in paths)
        return self._to_abs_paths(paths)

    def output_for_target(
        self, target: str, sub_target: str = "DEFAULT", rel_path: bool = False
    ) -> Path:
        paths = self.outputs_for_target(target, sub_target, rel_path)
        assert len(paths) == 1, f"Found more than 1 output for target {target}: {paths}"
        return paths[0]


LOG_COMPUTE_KEY = "build_api::actions::calculation: compute"


class TargetsResult(BuckResult):
    """Represents a Buck process  of a targets command that has finished running"""

    def __init__(
        self,
        process: subprocess.Process,
        stdout: str,
        stderr: str,
        buck_build_id: str,
        *argv: str,
    ) -> None:
        self.args = " ".join(argv)
        super().__init__(process, stdout, stderr, buck_build_id)

    def get_target_to_build_output(self) -> Dict[str, str]:
        """
        Returns a dict of the target and its output file in buck-out
        """
        target_to_output = {}
        assert (
            "--show-output" in self.args or "--show-full-output" in self.args
        ), "Must add --show-output or --show-full-output arg to get targets output"
        show_output = self.stdout.strip().splitlines()
        for line in show_output:
            output_mapping = line.split()
            assert len(output_mapping) <= 2, "Output mapping should be less than 2"
            target = output_mapping[0]
            if len(output_mapping) == 1:
                target_to_output[target] = ""
            else:
                target_to_output[target] = output_mapping[1]
        return target_to_output


class BuildResult(BuckResult):
    """Represents a Buck process  of a build command that has finished running"""

    def __init__(
        self,
        process: subprocess.Process,
        stdout: str,
        stderr: str,
        buck_build_id: str,
        executable_type: ExecutableType,
        *argv: str,
    ) -> None:
        self.args = " ".join(argv)
        self.executable_type = executable_type
        super().__init__(process, stdout, stderr, buck_build_id)

    def get_target_to_build_output(self) -> Dict[str, str]:
        """
        Returns a dict of the build target and file created in buck-out
        Prints to build target followed by path to buck-out file to stdout
        """
        target_to_output = {}
        assert (
            "--show-output" in self.args or "--show-full-output" in self.args
        ), "Must add --show-output or --show-full-output arg to get build output"
        show_output = self.stdout.strip().splitlines()
        if "--print-build-report" in self.args:
            # When mixing --show-output with --print-build-report, the first line is
            # the build report, and the remaining ones are the results
            show_output = show_output[1:]
        for line in show_output:
            output_mapping = line.split()
            assert len(output_mapping) <= 2, "Output mapping should be less than 2"
            target = output_mapping[0]
            if len(output_mapping) == 1:
                target_to_output[target] = ""
            else:
                target_to_output[target] = output_mapping[1]
        return target_to_output

    def get_build_report(self) -> BuildReport:
        """
        Returns a BuildReport object for a buck v2 build invoked with --print-build-report.
        Looks for a '{' and parses the build stdout starting from '{' as a json.
        """
        try:
            assert self.executable_type in (
                ExecutableType.buck2,
                ExecutableType.buck2_build_api_binary,
            ), "--print-build-report only works in v2"
            start = self.stdout.index("{")
            end = self.stdout.index("\n", start)
            parsed = json.loads(self.stdout[start:end])
            return BuildReport(parsed)
        except Exception as e:
            print(f"stdout: {self.stdout}\nstderr: {self.stderr}")
            raise e

    def get_action_to_cache_miss_count(self) -> Dict[str, int]:
        """
        Returns a dictionary of action key to number of cache misses.
        Populates this dictionary by going through stdout looking for logs of compute calls.

        Currently, there is no unique identifier for the action in buck2, so the action key
        is just a tuple of configured target name and analysis id.
        """
        action_to_cache_miss_count: Dict[str, int] = defaultdict(int)
        for line in self.stdout.splitlines():
            if LOG_COMPUTE_KEY in line:
                target = line.split(LOG_COMPUTE_KEY)[-1].strip()
                action_to_cache_miss_count[target] += 1
        return dict(action_to_cache_miss_count)


class TestResultSummary:
    """Represents a summary of a test result"""

    def __init__(self, name: str, status: str, result_type: ResultType) -> None:
        self.name: str = name
        self.status: str = status
        self.result_type: ResultType = ResultType(result_type)

    def get_name(self) -> str:
        """Returns the name of the test"""
        return self.name

    def get_status(self) -> str:
        """Returns the status of the test"""
        return self.status

    def get_result_type(self) -> ResultType:
        """Returns the result type of the test"""
        return self.result_type


class TestResult(BuckResult):
    """Represents a Buck process  of a test command that has finished running"""

    def __init__(
        self,
        process: subprocess.Process,
        stdout: str,
        stderr: str,
        buck_build_id: str,
        test_output_file: Path,
    ) -> None:
        super().__init__(process, stdout, stderr, buck_build_id)
        self.test_root = (
            ET.parse(str(test_output_file)).getroot()
            if test_output_file.exists()
            else None
        )

    def get_tests(self) -> List[TestResultSummary]:
        """Returns a list of test result summaries"""
        if not self.test_root:
            return []
        test_list = []
        for tests in self.test_root:
            for testresult in tests.iter("testresult"):
                name = testresult.get("name")
                status = testresult.get("status")
                testresult_type = testresult.get("type")
                assert testresult_type in (
                    e.value for e in ResultType
                ), f"Type {testresult_type} is not a ResultType Enum"
                result_type = ResultType(testresult_type)
                test_result_summary = TestResultSummary(name, status, result_type)
                test_list.append(test_result_summary)
        return test_list

    def get_success_count(self) -> int:
        """Returns the number of successful tests"""
        return self._get_count(ResultType.SUCCESS)

    def get_failure_count(self) -> int:
        """Returns the number of failed tests"""
        return self._get_count(ResultType.FAILURE)

    def get_skipped_count(self) -> int:
        """Returns the number of tests skipped"""
        return self._get_count(ResultType.EXCLUDED)

    def _get_count(self, result_type: ResultType) -> int:
        """Returns the number of tests with the given status"""
        return sum(
            1 for test in self.get_tests() if test.get_result_type() == result_type
        )


class AuditConfigResult(BuckResult):
    """Represents a Buck process of an audit config command that has finished running"""

    def __init__(
        self,
        process: subprocess.Process,
        stdout: str,
        stderr: str,
        buck_build_id: str,
        *argv: str,
    ) -> None:
        self.args = " ".join(argv)
        super().__init__(process, stdout, stderr, buck_build_id)

    def get_json(self) -> Dict[str, str]:
        """Returns a dict of the json sent back by buck"""
        assert (
            "--style=json" in self.args or "--style json" in self.args
        ), "Must add --style=json or `--style json` arg to get json output"
        try:
            start = self.stdout.index("{")
            audit_json = self.stdout[start:].strip()
            parsed = json.loads(audit_json)
            return parsed
        except Exception as e:
            print(f"stdout: {self.stdout}\nstderr: {self.stderr}")
            raise e


class BxlResult(BuckResult):
    """Represents a Buck process  of a bxl command that has finished running"""

    def __init__(
        self,
        process: subprocess.Process,
        stdout: str,
        stderr: str,
        buck_build_id: str,
        *argv: str,
    ) -> None:
        self.args = " ".join(argv)
        super().__init__(process, stdout, stderr, buck_build_id)
