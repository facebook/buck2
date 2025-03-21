# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

# pyre-strict

"""
Example usage (internal):
$ cat inputs.manifest
[["foo.py", "input/foo.py", "//my_rule:foo"]]
$ buck build //fbcode//python/build/compile:compile --show-full-output
$ python <fulloutput/__main__.py> --output=out-dir --bytecode-manifest=output.manifest inputs.manifest
$ find out-dir -type f
out-dir/foo.pyc

Or (external):
compile.py --output=out-dir --bytecode-manifest=output.manifest --ignore-errors inputs.manifest
"""

import argparse
import errno
import json
import os
import re
import sys
import traceback
from functools import partial
from py_compile import compile, PycInvalidationMode, PyCompileError
from types import TracebackType
from typing import List, Type


if sys.version_info[0] == 3:
    import importlib
    import importlib.util

    DEFAULT_FORMAT: str = importlib.util.cache_from_source("{pkg}/{name}.py")
else:
    DEFAULT_FORMAT: str = "{pkg}/{name}.pyc"


def get_py_path(module: str) -> str:
    return module.replace(".", os.sep) + ".py"


def get_pyc_path(module: str, fmt: str) -> str:
    try:
        package, name = module.rsplit(".", 1)
    except ValueError:
        package, name = "", module
    parts = fmt.split(os.sep)
    for idx in range(len(parts)):
        if parts[idx] == "{pkg}":
            parts[idx] = package.replace(".", os.sep)
        elif parts[idx].startswith("{name}"):
            parts[idx] = parts[idx].format(name=name)
    return os.path.join(*parts)


def _mkdirs(dirpath: str) -> None:
    try:
        os.makedirs(dirpath)
    except OSError as e:
        if e.errno != errno.EEXIST:
            raise


def _stderr_print(msg: str) -> None:
    print(msg, file=sys.stderr, end="")


def _hyperlink(file: str, line: int, text: str) -> str:
    from urllib.parse import urlencode

    OSC = "\033]"
    ST = "\033\\"
    params = urlencode({"project": "fbsource", "paths[0]": file, "lines[0]": line})
    uri = f"https://www.internalfb.com/intern/nuclide/open/arc/?{params}"
    return f"{OSC}8;;{uri}{ST}{text}{OSC}8;;{ST}"


def pretty_exception(
    typ: Type[BaseException], exc: BaseException, tb: TracebackType, src: str
) -> None:
    try:
        from colorama import Fore, just_fix_windows_console, Style

        just_fix_windows_console()

        trace = traceback.format_exception(typ, exc, tb)
        line_number = None
        if isinstance(exc, PyCompileError) and isinstance(exc.exc_value, SyntaxError):
            line_number = exc.exc_value.lineno
        if line_number is None:
            line_number = 1
        prev_line = ""
        for line in trace:
            if line.startswith(
                "\nDuring handling of the above exception, another exception occurred:"
            ):
                # Drop everything beyond the original exception
                return

            if line.startswith("  File"):
                prev_line = line
            else:
                if prev_line.startswith("  File"):
                    # Magenta for last listed filename and line number
                    s = re.sub(
                        r"\b\d+$",
                        lambda match: f"{Fore.MAGENTA}{match.group()}{Fore.RESET}",
                        prev_line,
                    )
                    s = re.sub(
                        r'"(.*?)"',
                        lambda match: _hyperlink(
                            src,
                            line_number,
                            f'{Fore.MAGENTA}"{match.group(1)}"{Fore.RESET}',
                        ),
                        s,
                    )
                    _stderr_print(s)
                    prev_line = ""
            if line.startswith("    "):
                line = Fore.CYAN + line + Fore.RESET  # Cyan for code lines
                _stderr_print(line)
            elif line.startswith("SyntaxError") or line.startswith("ValueError"):
                # Bold magenta for exception
                line = Style.BRIGHT + Fore.MAGENTA + line + Style.RESET_ALL
                _stderr_print(line)
    except Exception:
        # If anything goes wrong, fall back to the default exception printing
        sys.excepthook = sys.__excepthook__
        traceback.print_exception(typ, exc, tb)


def main(argv: List[str]) -> None:
    parser = argparse.ArgumentParser(fromfile_prefix_chars="@")
    parser.add_argument("-o", "--output", required=True)
    parser.add_argument(
        "--bytecode-manifest", required=True, type=argparse.FileType("w")
    )
    parser.add_argument("-f", "--format", default=DEFAULT_FORMAT)
    parser.add_argument(
        "--invalidation-mode",
        type=str,
        default=PycInvalidationMode.UNCHECKED_HASH.name,
        choices=[m.name for m in PycInvalidationMode],
    )
    parser.add_argument(
        "--debug",
        action="store_true",
        default=False,
    )
    parser.add_argument("manifests", nargs="*")
    args = parser.parse_args(argv[1:])
    invalidation_mode = PycInvalidationMode.__members__[args.invalidation_mode]
    bytecode_manifest = []

    _mkdirs(args.output)

    for manifest_path in args.manifests:
        with open(manifest_path) as mf:
            manifest = json.load(mf)
        for dst, src, _ in manifest:
            # This is going to try to turn a path into a Python module, so
            # reduce the scope for bugs in get_pyc_path by normalizing first.
            dst = os.path.normpath(dst)
            # We only care about python sources.
            base, ext = os.path.splitext(dst)
            if ext != ".py":
                continue
            module = base.replace(os.sep, ".")
            dest_pyc = get_pyc_path(module, args.format)
            pyc = os.path.join(args.output, dest_pyc)
            _mkdirs(os.path.dirname(pyc))
            try:
                compile(
                    src,
                    cfile=pyc,
                    dfile=get_py_path(module),
                    doraise=True,
                    invalidation_mode=invalidation_mode,
                )
            except PyCompileError:
                if not args.debug:
                    sys.excepthook = partial(pretty_exception, src=src)
                raise
            bytecode_manifest.append((dest_pyc, pyc, src))
    json.dump(bytecode_manifest, args.bytecode_manifest, indent=2)


sys.exit(main(sys.argv))
