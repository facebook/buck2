#!/usr/bin/env python3
# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

"""
Fake script that acts as a test
"""

import argparse
import json
import os
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


def check_no_changes(git: bool):
    status_cmd = []
    diff_cmd = []
    if git:
        status_cmd = ["git", "status", "--porcelain"]
        diff_cmd = ["git", "diff"]

    else:
        status_cmd = ["hg", "status", "-mard"]
        diff_cmd = ["hg", "diff", "--pager=none"]

    status = run(status_cmd, capture_output=True)
    if status.stdout.strip():
        run(diff_cmd)
        print_error(
            "File changed from commit. This means you need to run cargo-fmt locally and amend this commit."
        )
        sys.exit(1)


def list_starlark_files(git: bool):
    cmd = None
    includes = [
        "**.bxl",
        "**.bzl",
        "**/TARGETS",
        "**/TARGETS.v2",
    ]
    excludes = [
        "starlark-rust/starlark/testcases",
        "tests/targets/lsp/bad_syntax.bzl",
        "tests/targets/lsp/query.bxl",
        "tests/targets/lsp/globals.bzl",
        "tests/targets/lsp/cell/sub/defs.bzl",
    ]

    if git:
        excludes = [f":!:{s}" for s in excludes]
        cmd = ["git", "ls-files", "--"] + includes + excludes
    else:
        includes = [f"--include={s}" for s in includes]
        excludes = [f"--exclude={s}" for s in excludes]
        cmd = (
            [
                "hg",
                "files",
                ".",
            ]
            + includes
            + excludes
        )

    starlark_files = (
        run(
            cmd,
            capture_output=True,
        )
        .stdout.strip()
        .splitlines()
    )
    return starlark_files


def rustfmt(buck2_dir: Path, ci: bool, git: bool) -> None:
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

    if run([cargo_fmt, "--"], env=env).returncode != 0:
        sys.exit(1)

    # On CI, fail if any committed files have changed,
    # mainly because of cargo fmt changing a source file
    if ci:
        check_no_changes(git)


CLIPPY_ALLOW = [
    # Lints that are temporarily broken in our version of the Rust toolchain
    "clippy::needless-match",
    "clippy::only-used-in-recursion",
    # Other
    "clippy::disallowed_names",  # Not using foo, bar, baz in test data is silly
    "clippy::bool-to-int-with-if",  # Using if branches to return 1 or 0 is valid, but this complains that we should use `int::from`, which is arguably less clear
    "clippy::cognitive_complexity",  # This is an arbitrary linter
    "clippy::collapsible-if",  # Sometimes nesting better expresses intent
    "clippy::collapsible-else-if",  # Sometimes nesting better expresses intent
    "clippy::comparison_chain",  # Generates worse code and harder to read
    "clippy::comparison_to_empty",  # x == "" is clearer than x.is_empty()
    "clippy::derive_partial_eq_without_eq",  # In generated protobuf code
    "clippy::implicit-hasher",  # Makes code more complex for little benefit
    "clippy::len-without-is-empty",  # len() == 0 is perfectly clear
    "clippy::manual-range-contains",  # a <= b && b <= c is way clearer than (a..=c).contains(&b)
    "clippy::many_single_char_names",  # match(a,b,c,d,e) sometimes makes sense
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
    "clippy::uninlined_format_args",  # From Rust 1.67.0 (made pedantic in 1.67.1 https://github.com/rust-lang/rust-clippy/pull/10265)
    "clippy::needless_update",  # Our RE structs have slightly different definitions in internal and OSS.
    "clippy::almost-swapped",  # Triggered by Clap v3, perhaps remove when we move to v4
]

CLIPPY_DENY = [
    "clippy::all",
    "clippy::await_holding_lock",
    "clippy::await_holding_refcell_ref",
    "clippy::dbg_macro",
    "clippy::debug_assert_with_mut_call",
    "clippy::empty_enum",
    "clippy::filter_map_next",
    "clippy::flat_map_option",
    "clippy::from_iter_instead_of_collect",
    "clippy::large_stack_arrays",
    "clippy::linkedlist",
    "clippy::macro_use_imports",
    "clippy::maybe_infinite_iter",
    "clippy::mem_forget",
    "clippy::mut_mut",
    "clippy::needless_continue",
    "clippy::needless_range_loop",
    "clippy::nonstandard_macro_braces",
    "clippy::rc_mutex",
    "clippy::ref_option_ref",
    "clippy::rest_pat_in_fully_bound_structs",
    "clippy::same_functions_in_if_condition",
    "clippy::str_to_string",
    "clippy::string_to_string",
    "clippy::todo",
    "clippy::trivially_copy_pass_by_ref",
    "clippy::useless_transmute",
    "clippy::verbose_file_reads",
    "clippy::wildcard_dependencies",
    "let_underscore_drop",
    "unused_extern_crates",
]


CLIPPY_AUTOFIX = [
    # Only add machine-fixable warnings in this list, or we'll see them all
    # the time in CI.
    "clippy::cloned_instead_of_copied",
    "clippy::inconsistent_struct_constructor",
    "clippy::inefficient_to_string",
    "clippy::let_unit_value",
    "clippy::map_flatten",
    "clippy::map_unwrap_or",
    "clippy::needless_bitwise_bool",
    "clippy::needless_borrow",
    "clippy::range_minus_one",
    "clippy::unwrap_or_else_default",
    "clippy::useless-conversion",
]

RUSTC_ALLOW = {
    # This needs a feature
    "unfulfilled-lint-expectations",
    # This is not *actually* a  warning but rather a warning level.
    "warnings",
}


def _get_default_rustc_warnings() -> List[str]:
    """
    We want to error on all Rustc default warnings. The very natural way to do
    this would be to simply enable -Dwarnings, which would enable the
    `warnings` lint group from rustc like `clippy::all` does.

    Unfortunately, that's not at all what -Dwarnings does! `warnings` is not a
    lint group, it's a special magical keyword that turns all `warnings` into
    errors and is completely incompatible with tweaking lint levels (so e.g. if
    you `--allow` a warning, but have `-Dwarnings` , that will error out).

    So, we have to ask rustc to list all the default warnings for us, and error
    out on them here.
    """
    rustc = run(["rustup", "which", "rustc"], capture_output=True).stdout.strip()
    out = run([rustc, "-Whelp"], capture_output=True).stdout.strip()

    # This is some parsing that wants to be a little robust to changes in the
    # output we're reading we're parsing help here.
    lints = []

    for line in out.split("\n"):
        maybe_a_lint = line.split(None, 2)
        if len(maybe_a_lint) == 3 and maybe_a_lint[1] == "warn":
            lint = maybe_a_lint[0]
            if lint not in RUSTC_ALLOW:
                lints.append(lint)

    return lints


def clippy(package_args: List[str], fix: bool) -> None:
    """
    Run cargo clippy.
    Also fails on any rustc warnings or build errors.
    We'd really like a quiet option (at least for CI), but it doesn't exist
    """

    print_running("clippy")

    rustc_default_warnings = _get_default_rustc_warnings()

    clippy_fix_args = ["--fix"] if fix else []
    clippy_deny_args = [f"--deny={c}" for c in [*CLIPPY_DENY, *rustc_default_warnings]]
    clippy_allow_args = [f"--allow={c}" for c in [*CLIPPY_ALLOW, *CLIPPY_AUTOFIX]]

    run(
        [
            "cargo",
            "clippy",
            *package_args,
            *clippy_fix_args,
            "-Z=unstable-options",
            "--profile=test",
            "--tests",
            "--benches",
            "--",
            *clippy_deny_args,
            *clippy_allow_args,
        ]
    )


def starlark_linter(git: bool) -> None:
    print_running("starlark linter")
    starlark_files = list_starlark_files(git)
    with tempfile.NamedTemporaryFile(mode="w+t") as fp:
        fp.writelines([x + "\n" for x in starlark_files])
        fp.flush()
        run(
            [
                "buck2",
                "--isolation-dir=starlark-linter",
                "starlark",
                "lint",
                "--no-buckd",
                "@" + fp.name,
            ]
        )


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
        "--git",
        action="store_true",
        default=False,
        help="Use `git` to check repo state, the script defaults to `hg`",
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
        "--clippy-fix",
        action="store_true",
        default=False,
        help="Apply Clippy suggestions",
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

    if package_args == [] and not (args.lint_rust_only or args.rustfmt_only):
        with timing():
            starlark_linter(args.git)

    if not (args.rustfmt_only or args.lint_starlark_only):
        with timing():
            clippy(package_args, args.clippy_fix)

    if not args.lint_starlark_only:
        with timing():
            rustfmt(buck2_dir, args.ci, args.git)

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
        check_no_changes(args.git)


if __name__ == "__main__":
    try:
        main()
    except KeyboardInterrupt:
        # no stack trace on interrupt
        sys.exit(signal.SIGINT)
