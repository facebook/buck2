# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

# pyre-strict

import argparse
import enum
import re
import shlex
import subprocess
import sys
import tempfile

from typing import List, Optional, Tuple


def _eprintln(msg: str) -> None:
    print(msg, flush=True, file=sys.stderr)


def _is_argfile(index: int, args: List[str]) -> bool:
    previous_argument = args[index - 1 : index]
    previous_two_arguments = args[index - 2 : index]
    if (
        previous_argument == ["-rpath"]
        or previous_argument == ["-install_name"]
        or previous_two_arguments == ["-rpath", "-Xlinker"]
        or previous_two_arguments == ["-install_name", "-Xlinker"]
    ):
        return False

    return args[index].startswith("@")


def _expand_arg_files(args: List[str]) -> List[str]:
    expanded_args = []
    for index, arg in enumerate(args):
        if _is_argfile(index, args):
            with open(arg[1:]) as argfile:
                expanded_args.extend(
                    [line.strip('"') for line in argfile.read().splitlines()]
                )
        else:
            expanded_args.append(arg)
    return expanded_args


def _seperate_wrapper_args_from_linker_args(
    args: List[str],
) -> Tuple[List[str], List[str]]:
    wrapper_args = []
    linker_args = []
    expanded_args = _expand_arg_files(args)

    i = 0
    while i < len(expanded_args):
        if expanded_args[i] == "-Xwrapper":
            wrapper_args.append(expanded_args[i + 1])
            i += 1
        else:
            linker_args.append(expanded_args[i])
        i += 1

    return wrapper_args, linker_args


def _diagnose_potential_unexported_symbol_issue(
    unexported_symbol_lists: List[str], stderr: str
) -> Optional[str]:
    stderr_lines = stderr.splitlines()
    undefined_symbol_re = re.compile(r"undefined symbol:.*\(mangled: (\S+)\)")
    undefined_symbols = set()
    for stderr_line in stderr_lines:
        match = re.search(undefined_symbol_re, stderr_line)
        if match:
            undefined_symbols.add(match.group(1))

    if not undefined_symbols:
        return None

    unexported_symbols = set()
    incorrectly_unexported_symbols = set()
    incorrect_unexported_symbol_lists = []
    for unexported_symbol_list in unexported_symbol_lists:
        target_name, file_path = unexported_symbol_list.split(",")
        with open(file_path, "r") as unexported_symbol_list_file:
            unexported_symbols = set(unexported_symbol_list_file.read().splitlines())
            intersection = undefined_symbols & unexported_symbols
            if intersection:
                incorrectly_unexported_symbols.update(intersection)
                incorrect_unexported_symbol_lists.append(target_name)

    if not incorrect_unexported_symbol_lists:
        return None

    return f"""
UNEXPORTED SYMBOLS ERROR:

At least one symbol is included in an unexported symbol list, but referenced across dylib boundaries. Please
run the following command to fix the unexported symbol lists:

arc fix-unexported-symbol-lists {"".join(["--target " + target for target in incorrect_unexported_symbol_lists])} {" ".join(["--symbol " + symbol for symbol in sorted(incorrectly_unexported_symbols)])}

Here is the linker failure message:
"""


class Linker(enum.Enum):
    LLD = "lld"
    LD64 = "ld64"


def _discover_linker(args: List[str]) -> Optional[Linker]:
    for arg in args:
        if arg.startswith("-fuse-ld="):
            linker_name = arg.split("=")[-1]
            if linker_name == Linker.LLD.value:
                return Linker.LLD
            elif linker_name == Linker.LD64.value:
                return Linker.LD64
            else:
                raise Exception(f"Unknown linker: {linker_name}")


def main(argv: List[str]) -> int:
    wrapper_args, linker_args = _seperate_wrapper_args_from_linker_args(argv[1:])

    parser = argparse.ArgumentParser()
    parser.add_argument("-linker")
    parser.add_argument("-unexported_symbol_list", action="append")
    args = parser.parse_args(wrapper_args)

    linker = _discover_linker(linker_args)
    if linker == Linker.LLD:
        linker_args.extend(
            ["-Xlinker", "-pika_include_mangled_names_in_undefined_symbol_errors"]
        )

    with tempfile.NamedTemporaryFile(mode="w+t") as linker_argsfile:
        linker_argsfile.write("\n".join([shlex.quote(a) for a in linker_args]))
        linker_argsfile.flush()
        result = subprocess.run(
            [args.linker, f"@{linker_argsfile.name}"], capture_output=True, text=True
        )

    if result.returncode != 0:
        if args.unexported_symbol_list and linker == Linker.LLD:
            diagnosis = _diagnose_potential_unexported_symbol_issue(
                args.unexported_symbol_list, result.stderr
            )
            if diagnosis:
                _eprintln(diagnosis)

    if result.stdout:
        print(result.stdout)
    if result.stderr:
        print(result.stderr, file=sys.stderr)

    return result.returncode


if __name__ == "__main__":
    sys.exit(main(sys.argv))
