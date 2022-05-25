#!/usr/bin/env python3
# (c) Facebook, Inc. and its affiliates. Confidential and proprietary.

"""
Fake script that acts as a test
"""

import argparse
import json
import os
import platform
import signal
import subprocess
import sys
import tempfile
import time
from contextlib import contextmanager
from enum import Enum
from pathlib import Path
from typing import Dict, Iterable, List, Optional


class Colors(Enum):
    # Copied from https://stackoverflow.com/questions/287871/how-to-print-colored-text-to-the-terminal
    HEADER = "\033[95m"
    OKBLUE = "\033[94m"
    OKCYAN = "\033[96m"
    OKGREEN = "\033[92m"
    WARNING = "\033[93m"
    FAIL = "\033[91m"
    ENDC = "\033[0m"
    BOLD = "\033[1m"
    UNDERLINE = "\033[4m"


def print_running(msg: str) -> None:
    print(Colors.OKGREEN.value + "Running " + msg + Colors.ENDC.value)


def print_error(msg: str) -> None:
    print(
        Colors.FAIL.value + Colors.BOLD.value + "ERROR: " + msg + Colors.ENDC.value,
        file=sys.stderr,
    )


@contextmanager
def timing() -> None:
    start = time.time()
    yield
    duration = time.time() - start
    print(f"Finished in {duration:.2f} seconds.")


def run(
    args: Iterable[str],
    capture_output: bool = False,
    env: Optional[Dict[str, str]] = None,
    error: Optional[str] = None,
) -> subprocess.CompletedProcess:
    """
    Runs a command (args) in a new process.
    If the command fails, raise CalledProcessError.
    If the command passes, return CompletedProcess.
    If capture_output is False, print to the console, otherwise record it as CompletedProcess.stdout/stderr.
    If error is specified, print error on stderr when there is a CalledProcessError.
    """
    # On Ci stderr gets out of order with stdout. To avoid this, we need to flush stdout/stderr first.
    sys.stdout.flush()
    sys.stderr.flush()
    try:
        result = subprocess.run(
            tuple(args),
            # We'd like to use the capture_output argument,
            # but that isn't available in Python 3.6 which we use on Windows
            stdout=subprocess.PIPE if capture_output else sys.stdout,
            stderr=subprocess.PIPE if capture_output else sys.stderr,
            check=True,
            encoding="utf-8",
            env=env or os.environ.copy(),
        )
        return result
    except subprocess.CalledProcessError as e:
        # Print the console info if we were capturing it
        if capture_output:
            print(e.stdout, file=sys.stdout)
            print(e.stderr, file=sys.stderr)
        if error:
            print_error(error)
        sys.exit(1)


def check_no_changes():
    hg_status = run(["hg", "status", "-mard"], capture_output=True)
    # If hg status -mard has nonempty stdout, then there's uncommitted changes.
    if hg_status.stdout.strip():
        run(["hg", "diff", "--pager=none"])
        print_error(
            "File changed from commit. This means you need to run cargo-fmt locally and amend this commit."
        )
        sys.exit(1)


def rustfmt(buck2_dir: Path, ci: bool) -> None:
    """
    Make the formatting consistent, using the custom rustfmt,
    which is a pre-release of rustfmt 2.0.
    We do that by putting rustfmt on the PATH, but that PATH
    also has a copy of rustup tools, so use our rustup captured before.
    Mixing and matching cargo-fmt and rust-fmt doesn't work on Windows,
    so skip formatting for now.
    """
    print_running("rustfmt")
    cargo_fmt = run(
        ["rustup", "which", "cargo-fmt"], capture_output=True
    ).stdout.strip()
    env = os.environ.copy()
    env["RUSTFMT"] = str(
        buck2_dir.parent.parent / "tools" / "third-party" / "rustfmt" / "rustfmt"
    )
    output = run([cargo_fmt, "--"], capture_output=True, env=env).stdout
    for line in output.splitlines():
        # The fbsource formatter spews out:
        #   Warning: the `merge_imports` option is deprecated. Use `imports_granularity=Crate` instead
        # So we just ignore its outputs, until Cargo moves to rustfmt 2.0.
        if "merge_imports" not in line.strip():
            print(output, file=sys.stderr)
            sys.exit(1)
    # On CI, fail if any committed files have changed,
    # mainly because of cargo fmt changing a source file
    if ci:
        check_no_changes()


CLIPPY_ALLOW = [
    "clippy::blacklisted-name",  # Not using foo, bar, baz in test data is silly
    "clippy::cognitive_complexity",  # This is an arbitrary linter
    "clippy::comparison_chain",  # Generates worse code and harder to read
    "clippy::comparison_to_empty",  # x == "" is clearer than x.is_empty()
    "clippy::implicit-hasher",  # Makes code more complex for little benefit
    "clippy::len-without-is-empty",  # len() == 0 is perfectly clear
    "clippy::manual-range-contains",  # a <= b && b <= c is way clearer than (a..=c).contains(&b)
    "clippy::many_single_char_names",  # match(a,b,c,d,e) sometimes makes sense
    "clippy::match_single_binding",  # Triggered by derive(Derivative)
    "clippy::match-like-matches-macro",  # Using matches! is sometimes clearer, sometimes not
    "clippy::match-wild-err-arm",  # Seems reasonable to panic on Err(_)
    "clippy::missing-safety-doc",  # Documentation should be tailored to the reader, not the linter
    "clippy::mut_from_ref",  # Tries to check soundness, which Rust already does
    "clippy::naive-bytecount",  # Requires an extra dependency for marginal gains.
    "clippy::needless_collect",  # False positives: doesn't understand lifetimes, or e.g. DoubleEndedIterator.
    "clippy::needless_lifetimes",  # This is throwing false positives
    "clippy::new_without_default",  # Default is not always useful
    "clippy::single_match",  # Sometimes a single match looks good
    "clippy::too_many_arguments",  # This is an arbitrary limit set on number of arguments and not always useful
    "clippy::type_complexity",  # This is an arbitrary limit set on number of type parameterizations and not always useful
    "clippy::unnecessary-wraps",  # Sometimes unnecessary wraps provide the right API
    "clippy::wrong_self_convention",  # These rules are useless pedantry
    "clippy::bool-assert-comparison",  # from rust version 1.53.0
    "clippy::non-send-fields-in-send-ty",  # This got unlanded in 1.58.1 (https://github.com/rust-lang/rust-clippy/issues/8045)
    "clippy::unwrap-or-else-default",  # Defaults aren't always more clear as it removes the type information when reading code
    "clippy::enum-variant-names",  # Sometimes you do want the same prefixes
]

CLIPPY_DENY = [
    "warnings",  # Turn on -Wall
    "bare_trait_objects",
    "clippy::await_holding_lock",
    "clippy::await_holding_refcell_ref",
    "clippy::cloned_instead_of_copied",
    "clippy::dbg_macro",
    "clippy::debug_assert_with_mut_call",
    "clippy::empty_enum",
    "clippy::filter_map_next",
    "clippy::flat_map_option",
    "clippy::from_iter_instead_of_collect",
    "clippy::inconsistent_struct_constructor",
    "clippy::inefficient_to_string",
    "clippy::large_stack_arrays",
    "clippy::let_underscore_drop",
    "clippy::let_unit_value",
    "clippy::linkedlist",
    "clippy::macro_use_imports",
    "clippy::map_flatten",
    "clippy::map_unwrap_or",
    "clippy::maybe_infinite_iter",
    "clippy::mem_forget",
    "clippy::mut_mut",
    "clippy::needless_bitwise_bool",
    "clippy::needless_borrow",
    "clippy::needless_continue",
    "clippy::nonstandard_macro_braces",
    "clippy::range_minus_one",
    "clippy::rc_mutex",
    "clippy::ref_option_ref",
    "clippy::rest_pat_in_fully_bound_structs",
    "clippy::same_functions_in_if_condition",
    "clippy::str_to_string",
    "clippy::string_to_string",
    "clippy::todo",
    "clippy::trivially_copy_pass_by_ref",
    "clippy::unwrap_or_else_default",
    "clippy::useless_transmute",
    "clippy::verbose_file_reads",
    "clippy::wildcard_dependencies",
    "ellipsis_inclusive_range_patterns",
    "non_fmt_panics",
    "temporary_cstring_as_ptr",
    "unconditional_recursion",
    "unused_extern_crates",
]


def clippy(package_args: List[str]) -> None:
    """
    Run cargo clippy.
    Also fails on any rustc warnings or build errors.
    We'd really like a quiet option (at least for CI), but it doesn't exist
    """
    print_running("clippy")
    clippy_allow_args = [f"--allow={c}" for c in CLIPPY_ALLOW]
    clippy_deny_args = [f"--deny={c}" for c in CLIPPY_DENY]
    run(
        [
            "cargo",
            "clippy",
            *package_args,
            "-Z=unstable-options",
            "--profile=test",
            "--tests",
            "--benches",
            "--",
            *clippy_allow_args,
            *clippy_deny_args,
        ],
    )


def starlark_linter() -> None:
    print_running("starlark linter")
    starlark_files = (
        run(
            [
                "hg",
                "files",
                ".",
                "--include=**.bxl",
                "--include=**.bzl",
                "--include=**/TARGETS",
                "--include=**/TARGETS.v2",
                "--exclude=starlark-rust/starlark/testcases",
            ],
            capture_output=True,
        )
        .stdout.strip()
        .splitlines()
    )
    with tempfile.NamedTemporaryFile(mode="w+t") as fp:
        fp.writelines([x + "\n" for x in starlark_files])
        fp.flush()
        output = run(
            ["cargo", "run", "--bin=starlark", "--", "--check", "@" + fp.name],
            capture_output=True,
        ).stdout

    # Starlark linter prints each lint on a different line, then 1 summary line,
    # so 1 line long means no lint warnings
    if len(output.splitlines()) != 1:
        print(output, file=sys.stderr)
        print_error("Starlark lint errors were detected")
        sys.exit(1)


def _lookup(d, *keys):
    """Nested lookup in a dict"""
    for k in keys:
        if d is None:
            return None
        d = d.get(k)
    return d


def rustdoc(package_args: List[str]) -> None:
    print_running("cargo doc")
    # We have to chose between showing the output, or capturing it.
    # We have to capture it to figure out if there were warnings.
    # We would strongly like to show it, because it might take a while.
    # Cheat and do it twice, as we know Rust caches it, so the second time is quick.
    run(["cargo", "doc", "--no-deps", *package_args])
    output = run(
        ["cargo", "doc", "--message-format=json", "--no-deps", *package_args],
        capture_output=True,
    )

    has_warnings = False

    # We'd really like to turn on warnings-as-errors, but we can't
    # We'd really like to get this information from the exit code, but we can't
    # Therefore, look for output that suggests there was a warning produced.
    # Alas, that's the substring 'warning', since given console output, even 'warning:'
    # might get an escape code within it.
    for line in output.stdout.split("\n"):
        line = line.strip()
        if not line:
            continue

        line = json.loads(line)

        # If it's not a compiler message then ignore it.
        if line.get("reason") != "compiler-message":
            continue

        # If it's not from buck2 itself (e.g. a dep), ignore.
        target = line.get("target", {}).get("src_path", "")
        if "/buck2/" not in target:
            continue

        # If it's not a doc warning, ignore it. The `message` field will
        # contain a `code` field that itself has a `code` field that is machine
        # readable for we look for this.
        code = _lookup(line, "message", "code", "code")
        if code is None or "rustdoc::" not in code:
            continue

        has_warnings = True

        print_error("Documentation warning:")
        print(line.get("message", {}).get("rendered", ""))

    if has_warnings:
        sys.exit(1)


def test(package_args: List[str]) -> None:
    print_running("cargo test")
    run(["cargo", "test", *package_args])


def main() -> None:
    parser = argparse.ArgumentParser()
    parser.add_argument(
        "--ci",
        action="store_true",
        default=False,
        help="Whether to run CI workflow",
    )
    parser.add_argument(
        "--lint-only",
        action="store_true",
        default=False,
        help="Perform formatting and lints only. Do not run tests.",
    )
    parser.add_argument(
        "--lint-rust-only",
        action="store_true",
        default=False,
        help="Perform rust formatting and lints only. Do not run tests.",
    )
    parser.add_argument(
        "--lint-starlark-only",
        action="store_true",
        default=False,
        help="Perform starlark formatting and lints only. Do not run tests.",
    )
    parser.add_argument(
        "--rustfmt-only",
        action="store_true",
        default=False,
        help="Perform formatting only. Do not run lints or tests.",
    )
    parser.add_argument(
        "--exclude",
        action="append",
        help="Packages excluded from linting.",
    )
    parser.add_argument(
        "packages",
        nargs="*",
        type=str,
        help="The packages to run lint on. If not specified, all packages",
    )
    args = parser.parse_args()

    # Change to buck2 directory
    buck2_dir = Path(__file__).parent.absolute()
    os.chdir(str(buck2_dir))

    package_args = [f"--package={p.rstrip('/')}" for p in args.packages]
    if args.exclude:
        package_args.append("--workspace")
        package_args.extend([f"--exclude={p.rstrip('/')}" for p in args.exclude])

    if not args.lint_starlark_only:
        with timing():
            rustfmt(buck2_dir, args.ci)

    if not (args.rustfmt_only or args.lint_starlark_only):
        with timing():
            clippy(package_args)

    if package_args == [] and not (args.lint_rust_only or args.rustfmt_only):
        with timing():
            starlark_linter()

    if not (
        args.lint_only
        or args.lint_rust_only
        or args.lint_starlark_only
        or args.rustfmt_only
    ):
        with timing():
            rustdoc(package_args)

        with timing():
            test(package_args)

    # On CI, check to make sure our test doesn't overwrite existing files
    if args.ci:
        check_no_changes()


if __name__ == "__main__":
    try:
        main()
    except KeyboardInterrupt:
        # no stack trace on interrupt
        sys.exit(signal.SIGINT)
