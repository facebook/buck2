# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

"""Build package trees for the prelude's Node.js rules.

package_tree.py flat-package --emit DIR [--package-json FILE] --out DIR
package_tree.py merge --deps DIR --prebuilt DIR --out DIR
package_tree.py launchers --spec FILE --out DIR
package_tree.py archive --src DIR --format FORMAT --out FILE [--root ROOT] [--executables FILE]
    [--sha256 FILE]
package_tree.py compress --src FILE --out OUT [--sha256 FILE] --zstd ZSTD...
package_tree.py strip-types --src DIR --out DIR
"""

from __future__ import annotations

import argparse
import contextlib
import gzip
import hashlib
import itertools
import json
import os
import posixpath
import re
import shutil
import stat
import subprocess
import sys
import tarfile
import zipfile
from collections.abc import Collection, Iterator, Sequence
from pathlib import Path
from typing import BinaryIO, NamedTuple


def _copy_entry(
    entry: os.DirEntry[str],
    target: Path,
    keep_symlinks: bool,
    skip_suffixes: tuple[str, ...],
    ancestors: set[tuple[int, int]],
) -> None:
    if keep_symlinks and entry.is_symlink():
        os.symlink(os.readlink(entry.path), target, target_is_directory=entry.is_dir())
    elif entry.is_dir():
        _copy_tree(Path(entry.path), target, keep_symlinks, skip_suffixes, ancestors)
    elif not entry.name.endswith(skip_suffixes) and not (
        entry.is_symlink() and os.path.realpath(entry.path).endswith(skip_suffixes)
    ):
        shutil.copy(entry.path, target)


def _copy_tree(
    src: Path,
    dst: Path,
    keep_symlinks: bool,
    skip_suffixes: tuple[str, ...],
    ancestors: set[tuple[int, int]],
) -> None:
    st = os.stat(src)
    key = (st.st_dev, st.st_ino)
    if key in ancestors:
        raise ValueError(
            f"symlink cycle: {src} resolves to a directory that contains it"
        )
    ancestors.add(key)
    dst.mkdir(parents=True, exist_ok=True)
    with os.scandir(src) as entries:
        for entry in entries:
            _copy_entry(
                entry, dst / entry.name, keep_symlinks, skip_suffixes, ancestors
            )
    ancestors.remove(key)


def copy_tree(
    src: Path,
    dst: Path,
    keep_symlinks: bool = True,
    skip_suffixes: tuple[str, ...] = (),
) -> None:
    """Copy the contents of `src` into `dst`, keeping symlinks as symlinks unless `keep_symlinks` is false.

    Files whose names, or whose symlink targets' names, end with one of
    `skip_suffixes` are left out, except symlinks kept as symlinks. Raises `ValueError` on a symlink cycle, which only
    matters when symlinks are copied as their targets.
    """
    _copy_tree(src, dst, keep_symlinks, skip_suffixes, set())


def flat_package(emit: Path, package_json: Path | None, out: Path) -> None:
    """Lay out `emit` at the root of `out`, with `package_json` as its `package.json`."""
    copy_tree(emit, out)
    if package_json is not None:
        shutil.copy(package_json, out / "package.json")


def merge(
    deps: Path,
    prebuilt: Path,
    out: Path,
    keep_symlinks: bool = sys.platform != "win32",
) -> list[str]:
    """Merge two `node_modules` trees into `out`, or return the entries both define.

    A scope directory present in both trees is merged into one real directory,
    even when one tree links it, and its packages are compared instead. Nothing is written when any entry collides. Symlinks are
    copied as their targets on Windows, where a re-created link does not resolve
    in the consuming action; there a symlink cycle raises `ValueError`.
    """
    clashes = []
    scopes = set()
    for entry in sorted(os.scandir(prebuilt), key=lambda e: e.name):
        other = deps / entry.name
        if entry.name.startswith("@") and entry.is_dir() and other.is_dir():
            scopes.add(entry.name)
            clashes += sorted(
                entry.name + "/" + child.name
                for child in os.scandir(entry.path)
                if os.path.lexists(other / child.name)
            )
        elif os.path.lexists(other):
            clashes.append(entry.name)
    if not clashes:
        out.mkdir(parents=True, exist_ok=True)
        for tree in (deps, prebuilt):
            for entry in os.scandir(tree):
                if entry.name in scopes:
                    copy_tree(Path(entry.path), out / entry.name, keep_symlinks)
                else:
                    _copy_entry(entry, out / entry.name, keep_symlinks, (), set())
    return clashes


LAUNCHERS_HELP = """\
Write a POSIX sh launcher DIR/bin/NAME and a Windows launcher DIR/bin/NAME.cmd for each entry of
an app whose root is the parent of bin.

FILE is a JSON object:
  launchers  [{"name": NAME, "entry": PATH}], PATH being relative to the root, `/`-separated
  runtime    {"kind": "native" | "pointer" | "host",
              "posix_exe": PATH, "windows_exe": PATH,  (required unless kind is "host")
              "node_args": [str], "env": {str: str}}
  node_args  [str]
  args       [str]
  env        {str: str}

A launcher runs NODE, then the runtime's node_args, node_args, the entry, args and the
launcher's own arguments, with the runtime's env and then env set; an empty value unsets the
variable, and no two names may differ only in case. NODE is $NODEJS_LAUNCHER_NODE when set and non-empty, else ROOT/posix_exe (sh) or ROOT\\windows_exe
(cmd), or `node` from PATH for a host runtime, whose cmd launcher then drops .JS from PATHEXT.
`{app_root}` expands to ROOT when the launcher runs; values are otherwise literal.
"""

_APP_ROOT = "{app_root}"
_HOST_NODE = "node"
_MACRO = re.compile(r"\$\(@?[A-Za-z0-9_-]+[\s)]")
_ENV_NAME = re.compile(r"[A-Za-z_][A-Za-z0-9_]*")
_EXE = re.compile(r"[\w+-][\w.+-]*(/[\w+-][\w.+-]*)*", re.ASCII)
_SH_SAFE = re.compile(r"[\w@%+=:,./-]+", re.ASCII)
_SH_ESCAPED = re.compile(r'[\\"$`]')
_WINDOWS_DEVICE = re.compile(
    r"(CON|PRN|AUX|NUL|COM[0-9]|LPT[0-9])(\..*)?", re.IGNORECASE
)
# Variables the launchers set for themselves, and ERRORLEVEL: a variable of that name would hide
# node's exit code from `exit /b %ERRORLEVEL%`.
_RESERVED_ENV = frozenset(["__ROOT", "__NODE", "__CARET", "ERRORLEVEL"])
# The default PATHEXT.
_WINDOWS_EXE_EXTENSIONS = frozenset(
    [
        ".com",
        ".exe",
        ".bat",
        ".cmd",
        ".vbs",
        ".vbe",
        ".js",
        ".jse",
        ".wsf",
        ".wsh",
        ".msc",
    ]
)

# A sh launcher resolves its own path through symlinks without `readlink -f`, which older macOS
# and BSDs lack. The variables it unsets would make `ls` quote or color names, or `cd` search.
# bash 3.2, macOS's sh, ends a `$(` at the `)` that balances it, so case patterns open with `(`.
_SH_ROOT = """\
__ROOT=$(
  unset CDPATH CLICOLOR_FORCE QUOTING_STYLE
  self=$0
  while
    dir=${self%"${self##*/}"}
    [ -h "$self" ]
  do
    link=$(ls -ld -- "$self")
    link=${link#*"$self -> "}
    case $link in
      (/*) self=$link ;;
      (*) self=$dir$link ;;
    esac
  done
  cd -P -- "${dir:-./}.." && pwd -P
) || exit"""

# Arguments with any of these are quoted: whitespace and `,;=` separate arguments for the C
# runtime or for batch files, and cmd.exe acts on the rest.
_CMD_SPECIAL = frozenset(' \t&|<>^()%!",;=')
# `call` expands `%` twice and doubles each caret within quotes, so a caret comes from a variable
# that only the second expansion reads, as does the app root.
_CMD_CALL_ESCAPES = {"%": "%%%%", "^": "%%__CARET%%"}
# FOR variables that are not also `%~` modifiers.
_CMD_FOR_VARIABLES = "RBCEGHIJKLMOQUVWYbceghijklmoquvwy"


def _check_object(value: object, keys: set[str], what: str) -> dict[str, object]:
    if not isinstance(value, dict):
        raise ValueError(f"{what} must be an object")
    unknown = sorted(set(value) - keys)
    if unknown:
        raise ValueError(f"{what} has unknown keys {unknown}")
    return value


def _check_value(value: object, what: str) -> str:
    if not isinstance(value, str):
        raise ValueError(f"{what} must be a string, not {value!r}")
    bad = next((c for c in value if c != "\t" and not " " <= c <= "~"), None)
    if bad is not None:
        # cmd.exe decodes batch files in the console code page.
        raise ValueError(
            f"{what} {value!r} has {bad!r}; launchers hold printable ASCII only"
        )
    if _MACRO.search(value):
        raise ValueError(
            f"{what} {value!r} has a Buck macro, which a launcher cannot expand; "
            f"refer to files in the app with `{_APP_ROOT}`"
        )
    return value


def _is_portable_name(name: str) -> bool:
    return not (
        name in ("", ".", "..")
        or name[-1] in ". "
        or any(c in '\t<>:"\\|?*' for c in name)
        or _WINDOWS_DEVICE.fullmatch(name)
    )


def _check_path(value: object, what: str) -> str:
    path = _check_value(value, what)
    if not all(map(_is_portable_name, path.split("/"))):
        raise ValueError(f"{what} {path!r} is not a portable relative path")
    return path


def _strings(obj: dict[str, object], key: str, what: str) -> list[str]:
    values = obj.get(key)
    if values is None:
        return []
    if not isinstance(values, list):
        raise ValueError(f"{what} must be a list of strings")
    return [_check_value(v, f"{what}[{i}]") for i, v in enumerate(values)]


def _env(obj: dict[str, object], what: str) -> dict[str, str]:
    env = obj.get("env")
    if env is None:
        return {}
    if not isinstance(env, dict):
        raise ValueError(f"{what} must be an object")
    checked = {}
    for name, value in env.items():
        if not _ENV_NAME.fullmatch(name) or name.upper() in _RESERVED_ENV:
            raise ValueError(f"{what} cannot set {name!r}")
        checked[name] = _check_value(value, f"{what}[{name!r}]")
    return checked


def _runtime_nodes(runtime: dict[str, object]) -> tuple[str, str]:
    """The node that sh and cmd launchers run when `NODEJS_LAUNCHER_NODE` is unset or empty."""
    kind = runtime.get("kind")
    if kind == "host":
        if (
            runtime.get("posix_exe") is not None
            or runtime.get("windows_exe") is not None
        ):
            raise ValueError("a host runtime has no posix_exe or windows_exe")
        return _HOST_NODE, _HOST_NODE
    if kind not in ("native", "pointer"):
        raise ValueError(f"runtime.kind must be native, pointer or host, not {kind!r}")
    exes = []
    for key in ("posix_exe", "windows_exe"):
        exe = _check_path(runtime.get(key), f"runtime.{key}")
        if not _EXE.fullmatch(exe):
            raise ValueError(
                f"runtime.{key} {exe!r} has characters launchers do not quote"
            )
        exes.append(exe)
    return "$__ROOT/" + exes[0], "%__ROOT%\\" + exes[1].replace("/", "\\")


def _sh_word(parts: list[str]) -> str:
    """Quotes `parts`, joined by the app root, as one sh word."""
    if len(parts) == 1 and _SH_SAFE.fullmatch(parts[0]):
        return parts[0]
    # Double quotes, since shellcheck flags `$` or a final backslash within single quotes.
    return (
        '"'
        + "${__ROOT}".join(_SH_ESCAPED.sub(r"\\\g<0>", part) for part in parts)
        + '"'
    )


def _sh_launcher(
    node: str, node_args: list[str], entry: str, args: list[str], env: dict[str, str]
) -> str:
    lines = ["#!/bin/sh", _SH_ROOT, f"__NODE=${{NODEJS_LAUNCHER_NODE:-{node}}}"]
    for name, value in env.items():
        parts = value.split(_APP_ROOT)
        lines.append(f"export {name}={_sh_word(parts)}" if value else f"unset {name}")
    words = [_sh_word(arg.split(_APP_ROOT)) for arg in node_args]
    words.append(_sh_word(["", "/" + entry]))
    words += [_sh_word(arg.split(_APP_ROOT)) for arg in args]
    lines.append(" ".join(['exec "$__NODE"', *words, '"$@"']))
    return "\n".join(lines) + "\n"


def _cmd_call_arg(parts: list[str]) -> str:
    """Quotes `parts`, joined by the app root, as one argument on a launcher's `call` line.

    The program gets the argument if it parses its command line like the Microsoft C runtime,
    or if it is a batch file that passes `%*` on to such a program, since the result has cmd.exe
    metacharacters only within quotes.
    """
    # `call` prints its help instead of running the program if an unquoted argument has `/?`.
    if (
        len(parts) == 1
        and parts[0]
        and not _CMD_SPECIAL.intersection(parts[0])
        and "/?" not in parts[0]
    ):
        return parts[0]
    out = ['"']
    for i, part in enumerate(parts):
        if i:
            out.append("%%__ROOT%%")
        backslashes = 0
        for c in part:
            if c == "\\":
                backslashes += 1
            else:
                if c == '"':
                    # `\"` would end the quotes for cmd.exe, so `"` is written `""`.
                    out.append("\\" * backslashes + '"')
                backslashes = 0
            out.append(_CMD_CALL_ESCAPES.get(c, c))
        if i == len(parts) - 1:
            out.append("\\" * backslashes + '"')
    return "".join(out)


def _cmd_set(name: str, value: str) -> str:
    """A batch line that sets `name` to `value`, or unsets it when `value` is empty.

    `set "NAME=VALUE"` keeps the quotes within VALUE, and after an odd number of them the line
    is unquoted, so metacharacters there get carets. The app root comes from a FOR variable,
    which cmd.exe substitutes after parsing the line.
    """
    parts = value.split(_APP_ROOT)
    var = None
    if len(parts) > 1:
        var = next((v for v in _CMD_FOR_VARIABLES if v not in value), None)
        if var is None:
            raise ValueError(
                f"env value {value!r} leaves no FOR variable for `{_APP_ROOT}`"
            )
    out = []
    quoted = True
    for i, part in enumerate(parts):
        if i:
            out.append(f"%%~{var}")
        for c in part:
            if c == '"':
                quoted = not quoted
            elif c == "%":
                c = "%%"
            elif not quoted and c in "^&|<>()":
                c = "^" + c
            out.append(c)
    line = f'set "{name}={"".join(out)}"'
    return f'for %%{var} in ("%__ROOT%") do {line}' if var else line


def _cmd_launcher(
    node: str, node_args: list[str], entry: str, args: list[str], env: dict[str, str]
) -> str:
    call_args = [arg.split(_APP_ROOT) for arg in node_args]
    call_args.append(["", "\\" + entry.replace("/", "\\")])
    call_args += [arg.split(_APP_ROOT) for arg in args]
    lines = [
        "@echo off",
        "setlocal EnableExtensions DisableDelayedExpansion",
        # `%~dp0` names the wrong directory at the top level of a script run as `"NAME"` from
        # PATH, but not in a subroutine.
        "call :root",
        'set "__NODE=%NODEJS_LAUNCHER_NODE%"',
        f'if not defined __NODE set "__NODE={node}"',
    ]
    if any("^" in part for parts in call_args for part in parts):
        lines.append('set "__CARET=^"')
    lines += [_cmd_set(name, value) for name, value in env.items()]
    if node == _HOST_NODE:
        # cmd.exe looks for `node` in the current directory first, trying each PATHEXT extension,
        # so it would hand a `node.js` there to Windows Script Host.
        lines += [
            'if defined PATHEXT set "PATHEXT=;%PATHEXT:;=;;%;"',
            'if defined PATHEXT set "PATHEXT=%PATHEXT:;.JS;=%"',
            'if defined PATHEXT set "PATHEXT=%PATHEXT:;;=;%"',
            'if defined PATHEXT set "PATHEXT=%PATHEXT:~1,-1%"',
        ]
    lines += [
        # Node inherits `__ROOT`, `__NODE` and `__CARET`: `call` expands them as it runs.
        " ".join(['call "%%__NODE%%"', *map(_cmd_call_arg, call_args), "%%*"]),
        "exit /b %ERRORLEVEL%",
        ":root",
        'set "__ROOT=%~dp0.."',
        "exit /b",
    ]
    return "\r\n".join(lines) + "\r\n"


def _check_launcher(i: int, launcher: object) -> tuple[str, str]:
    fields = _check_object(launcher, {"name", "entry"}, f"launchers[{i}]")
    name = _check_path(fields.get("name"), f"launchers[{i}].name")
    if "/" in name:
        raise ValueError(f"launchers[{i}].name {name!r} has a `/`")
    # Also `node.exe` and the like: cmd.exe finds their sh launchers for `node`.
    if name.lower().split(".")[0] == _HOST_NODE:
        raise ValueError(
            f"launchers[{i}].name {name!r} would shadow node with bin on PATH"
        )
    # cmd.exe would run the sh launcher itself for NAME.
    _, dot, extension = name.lower().rpartition(".")
    if dot and "." + extension in _WINDOWS_EXE_EXTENSIONS:
        raise ValueError(
            f"launchers[{i}].name {name!r} ends in a Windows executable extension"
        )
    return name, _check_path(fields.get("entry"), f"launchers[{i}].entry")


def write_launchers(spec: object, out: Path) -> None:
    """Write the launchers that `spec` describes into `out`; see `LAUNCHERS_HELP`."""
    checked = _check_object(
        spec, {"launchers", "runtime", "node_args", "args", "env"}, "spec"
    )
    runtime = _check_object(
        checked.get("runtime"),
        {"kind", "posix_exe", "windows_exe", "node_args", "env"},
        "runtime",
    )
    posix_node, windows_node = _runtime_nodes(runtime)
    node_args = _strings(runtime, "node_args", "runtime.node_args")
    node_args += _strings(checked, "node_args", "node_args")
    args = _strings(checked, "args", "args")
    env = {**_env(runtime, "runtime.env"), **_env(checked, "env")}
    names = {}
    for name in env:
        other = names.setdefault(name.upper(), name)
        if other != name:
            raise ValueError(f"env sets both {other!r} and {name!r}, one cmd variable")

    launchers = checked.get("launchers")
    if not isinstance(launchers, list) or not launchers:
        raise ValueError("launchers must be a non-empty list")
    files = {}
    for i, launcher in enumerate(launchers):
        name, entry = _check_launcher(i, launcher)
        sh = _sh_launcher(posix_node, node_args, entry, args, env)
        cmd = _cmd_launcher(windows_node, node_args, entry, args, env)
        for file, text, mode in ((name, sh, 0o755), (name + ".cmd", cmd, 0o644)):
            if file.lower() in files:
                raise ValueError(f"launchers write bin/{file} twice")
            files[file.lower()] = (file, text, mode)

    bin_dir = out / "bin"
    bin_dir.mkdir(parents=True, exist_ok=True)
    for file, text, mode in files.values():
        path = bin_dir / file
        path.write_bytes(text.encode("ascii"))
        path.chmod(mode)


ARCHIVE_FORMATS = ("zip", "tar", "tar.gz")

ARCHIVE_HELP = """\
Write the contents of DIR to FILE as a FORMAT archive, under ROOT in the archive (by default, at
its top), and FILE's SHA-256 in hex, with a newline, to the --sha256 file.

Entries are in depth-first order, sorted by name within each directory. Directories are 0755, tar
symlinks 0777, and files 0755 if their owner can execute them or the --executables file lists them,
else 0644. Windows has no execute bits, so there only listed files are 0755. Each nonblank line of
the --executables file, less surrounding whitespace, is the `/`-separated path of a file in DIR that
neither is nor goes through a symlink.

zip entries are Unix entries dated 1985-02-01 00:00:00, with files deflated at level 9. tar entries
are PAX, dated 1985-10-26T08:15:00Z, with uid and gid 0 and no owner names. tar.gz is gzip at level
9 with no name or mtime. So the archive depends only on ROOT, the --executables file, the names,
contents, execute bits and symlinks in DIR, and the Python that writes it, including, for zip and
tar.gz, its zlib. If writing fails, FILE and the --sha256 file are removed.

A symlink must be relative, with `..` only at the start of its target, and resolve to a file or
directory in DIR without passing outside it. tar stores symlinks; zip stores what they point to,
and fails on a symlink to a directory that contains it.
"""

COMPRESS_HELP = """\
Compress FILE to OUT by running `ZSTD... -o OUT FILE`, and write OUT's SHA-256 in hex, with a
newline, to the --sha256 file. ZSTD is the rest of the command line, so --zstd comes last. If ZSTD
or writing the --sha256 file fails, OUT and the --sha256 file are removed.
"""

# 1985-10-26T08:15:00Z, the mtime npm gives every entry of a package tarball.
_TAR_MTIME = 499162500
# The DOS time that Buck's Java zip tool gives every entry.
_ZIP_DATE_TIME = (1985, 2, 1, 0, 0, 0)
_DEFLATE_LEVEL = 9
_DIR_MODE = stat.S_IFDIR | 0o755


class _Entry(NamedTuple):
    name: str
    mode: int
    # The file to copy, the target of a symlink, or "" for a directory.
    source: str


def _check_symlink(
    path: str, name: str, location: str, real_root: str
) -> tuple[str, str]:
    """Checks the symlink `path`, whose path in `real_root` without symlinks is `location`.

    Returns its target and the path in `real_root` without symlinks of what it resolves to.
    """
    target = os.readlink(path).replace(os.sep, "/")
    if target.startswith("/") or os.path.splitdrive(target)[0]:
        raise ValueError(f"symlink {name} -> {target} is absolute")
    try:
        os.stat(path)
    except OSError as e:
        raise ValueError(
            f"symlink {name} -> {target} does not resolve: {e.strerror}"
        ) from e
    real = os.path.realpath(path)
    inside = os.path.join(real_root, "")
    if real != real_root and not real.startswith(inside):
        raise ValueError(f"symlink {name} -> {target} resolves outside the directory")
    # Extracted, the target resolves from the symlink's place under another root, so its path
    # must stay in the directory: its leading `..`s may not climb above it, and no `..` may
    # follow a name, since after a symlink `..` climbs from where the symlink points.
    parts = [part for part in target.split("/") if part not in ("", ".")]
    names = list(itertools.dropwhile(lambda part: part == "..", parts))
    if ".." in names:
        raise ValueError(f"symlink {name} -> {target} has `..` after a name")
    if len(parts) - len(names) > location.count("/"):
        raise ValueError(f"symlink {name} -> {target} passes outside the directory")
    return target, real[len(inside) :].replace(os.sep, "/")


def _archive_entries(
    src: Path, root: str, follow_symlinks: bool, executables: Collection[str]
) -> list[_Entry]:
    if root and not (
        root.isprintable() and all(map(_is_portable_name, root.split("/")))
    ):
        raise ValueError(f"root {root!r} is not a portable relative path")
    parts = root.split("/") if root else []
    entries = [
        _Entry("/".join(parts[: i + 1]), _DIR_MODE, "") for i in range(len(parts))
    ]
    prefix = "".join(part + "/" for part in parts)
    real_root = os.path.realpath(src)
    listed = frozenset(executables)
    files = set()

    # `locations` are the paths in `src`, without symlinks, of `directory` and of the directories
    # the walk went through to reach it.
    def walk(directory: str, rel: str, locations: tuple[str, ...]) -> None:
        for entry in sorted(os.scandir(directory), key=lambda e: e.name):
            name = rel + entry.name
            location = posixpath.join(locations[-1], entry.name)
            st = entry.stat(follow_symlinks=False)
            if stat.S_ISLNK(st.st_mode):
                target, location = _check_symlink(entry.path, name, location, real_root)
                if not follow_symlinks:
                    entries.append(_Entry(prefix + name, stat.S_IFLNK | 0o777, target))
                    continue
                if location in locations:
                    raise ValueError(
                        f"symlink {name} -> {target} points to a directory that contains it"
                    )
                st = os.stat(entry.path)
            if stat.S_ISDIR(st.st_mode):
                entries.append(_Entry(prefix + name, _DIR_MODE, ""))
                walk(entry.path, name + "/", (*locations, location))
            elif stat.S_ISREG(st.st_mode):
                files.add(location)
                # Windows has no execute bits, though `os.stat` makes some up from file names.
                executable = location in listed or (
                    sys.platform != "win32" and st.st_mode & stat.S_IXUSR
                )
                mode = 0o755 if executable else 0o644
                entries.append(_Entry(prefix + name, stat.S_IFREG | mode, entry.path))
            else:
                raise ValueError(f"{name} is not a file, directory or symlink")

    walk(os.fspath(src), "", ("",))
    unknown = sorted(listed - files)
    if unknown:
        raise ValueError(
            f"executable {unknown[0]} names no file in the directory without following symlinks"
        )
    return entries


def _write_zip(entries: list[_Entry], out: Path) -> None:
    with zipfile.ZipFile(out, "w") as archive:
        for entry in entries:
            is_dir = stat.S_ISDIR(entry.mode)
            info = zipfile.ZipInfo(
                entry.name + "/" if is_dir else entry.name, _ZIP_DATE_TIME
            )
            # Unix, so that unzip and the like apply the modes in external_attr.
            info.create_system = 3
            # 0x10 is the MS-DOS directory attribute.
            info.external_attr = entry.mode << 16 | (0x10 if is_dir else 0)
            if is_dir:
                archive.writestr(info, b"")
            else:
                # `writestr` needs the whole file, but before Python 3.13 it is the only public
                # way to give a `ZipInfo` a compression level.
                with open(entry.source, "rb") as f:
                    archive.writestr(
                        info,
                        f.read(),
                        compress_type=zipfile.ZIP_DEFLATED,
                        compresslevel=_DEFLATE_LEVEL,
                    )


def _write_tar(entries: list[_Entry], fileobj: BinaryIO) -> None:
    with tarfile.open(
        fileobj=fileobj, mode="w", format=tarfile.PAX_FORMAT, encoding="utf-8"
    ) as archive:
        for entry in entries:
            # uid and gid default to 0, and uname and gname to "".
            info = tarfile.TarInfo(entry.name)
            info.mode = stat.S_IMODE(entry.mode)
            info.mtime = _TAR_MTIME
            if stat.S_ISDIR(entry.mode):
                info.type = tarfile.DIRTYPE
                archive.addfile(info)
            elif stat.S_ISLNK(entry.mode):
                info.type = tarfile.SYMTYPE
                info.linkname = entry.source
                archive.addfile(info)
            else:
                with open(entry.source, "rb") as f:
                    info.size = os.fstat(f.fileno()).st_size
                    archive.addfile(info, f)


def _write_sha256(path: Path, out: Path) -> None:
    digest = hashlib.sha256()
    with open(path, "rb") as f:
        for block in iter(lambda: f.read(1 << 20), b""):
            digest.update(block)
    out.write_bytes(digest.hexdigest().encode("ascii") + b"\n")


@contextlib.contextmanager
def _removed_on_failure(*paths: Path | None) -> Iterator[None]:
    try:
        yield
    except BaseException:
        for path in paths:
            if path is not None:
                path.unlink(missing_ok=True)
        raise


def write_archive(
    src: Path,
    archive_format: str,
    out: Path,
    root: str = "",
    executables: Collection[str] = (),
    sha256: Path | None = None,
) -> None:
    """Write `src` to `out` as an `archive_format` archive; see `ARCHIVE_HELP`."""
    if archive_format not in ARCHIVE_FORMATS:
        raise ValueError(f"unknown archive format {archive_format!r}")
    entries = _archive_entries(
        src, root, follow_symlinks=archive_format == "zip", executables=executables
    )
    with _removed_on_failure(out, sha256):
        if archive_format == "zip":
            _write_zip(entries, out)
        else:
            with open(out, "wb") as f:
                if archive_format == "tar":
                    _write_tar(entries, f)
                else:
                    # The empty filename keeps the output's name out of the gzip header.
                    with gzip.GzipFile(
                        filename="",
                        mode="wb",
                        compresslevel=_DEFLATE_LEVEL,
                        fileobj=f,
                        mtime=0,
                    ) as gz:
                        _write_tar(entries, gz)
        if sha256 is not None:
            _write_sha256(out, sha256)


def compress(
    src: Path, out: Path, zstd: Sequence[str], sha256: Path | None = None
) -> None:
    """Compress `src` to `out` with the zstd command `zstd`; see `COMPRESS_HELP`."""
    if not zstd:
        raise ValueError("no zstd command")
    with _removed_on_failure(out, sha256):
        subprocess.run([*zstd, "-o", os.fspath(out), os.fspath(src)], check=True)
        if sha256 is not None:
            _write_sha256(out, sha256)


_DECLARATION_SUFFIXES = (
    ".d.ts",
    ".d.mts",
    ".d.cts",
    ".d.ts.map",
    ".d.mts.map",
    ".d.cts.map",
)


def strip_types(src: Path, out: Path) -> None:
    """Copy `src` into `out` without TypeScript declarations or their source maps.

    Symlinks are copied as their targets, so declarations behind a linked file
    or directory are stripped too. Raises `ValueError` on a symlink cycle.
    """
    copy_tree(src, out, keep_symlinks=False, skip_suffixes=_DECLARATION_SUFFIXES)


def _path(arg: str) -> Path:
    # `node_modules` trees routinely exceed MAX_PATH on Windows.
    if sys.platform == "win32":
        return Path("\\\\?\\" + os.path.abspath(arg))
    return Path(arg)


def _read_executables(path: Path | None) -> list[str]:
    if path is None:
        return []
    lines = path.read_text(encoding="utf-8").splitlines()
    return [line.strip() for line in lines if line.strip()]


def _run_flat_package(args: argparse.Namespace) -> int:
    flat_package(args.emit, args.package_json, args.out)
    return 0


def _run_merge(args: argparse.Namespace) -> int:
    clashes = merge(args.deps, args.prebuilt, args.out)
    for name in clashes:
        print(
            f"node_modules entry {name} is in both the deps tree and the prebuilt tree",
            file=sys.stderr,
        )
    return 1 if clashes else 0


def _run_launchers(args: argparse.Namespace) -> int:
    write_launchers(json.loads(args.spec.read_text(encoding="utf-8")), args.out)
    return 0


def _run_archive(args: argparse.Namespace) -> int:
    write_archive(
        args.src,
        args.format,
        args.out,
        args.root,
        _read_executables(args.executables),
        args.sha256,
    )
    return 0


def _run_compress(args: argparse.Namespace) -> int:
    compress(args.src, args.out, args.zstd, args.sha256)
    return 0


def _run_strip_types(args: argparse.Namespace) -> int:
    strip_types(args.src, args.out)
    return 0


def _parser() -> argparse.ArgumentParser:
    parser = argparse.ArgumentParser(description=__doc__)
    commands = parser.add_subparsers(dest="command", required=True)
    flat = commands.add_parser("flat-package")
    flat.add_argument("--emit", type=_path, required=True)
    flat.add_argument("--package-json", type=_path)
    flat.add_argument("--out", type=_path, required=True)
    flat.set_defaults(run=_run_flat_package, errors=(OSError,))
    merged = commands.add_parser("merge")
    merged.add_argument("--deps", type=_path, required=True)
    merged.add_argument("--prebuilt", type=_path, required=True)
    merged.add_argument("--out", type=_path, required=True)
    merged.set_defaults(run=_run_merge, errors=(ValueError,))
    launchers = commands.add_parser(
        "launchers",
        description=LAUNCHERS_HELP,
        formatter_class=argparse.RawDescriptionHelpFormatter,
    )
    launchers.add_argument("--spec", type=_path, required=True, metavar="FILE")
    launchers.add_argument("--out", type=_path, required=True, metavar="DIR")
    launchers.set_defaults(run=_run_launchers, errors=(OSError, ValueError))
    archive = commands.add_parser(
        "archive",
        description=ARCHIVE_HELP,
        formatter_class=argparse.RawDescriptionHelpFormatter,
    )
    archive.add_argument("--src", type=_path, required=True, metavar="DIR")
    archive.add_argument("--format", choices=ARCHIVE_FORMATS, required=True)
    archive.add_argument("--out", type=_path, required=True, metavar="FILE")
    archive.add_argument("--root", default="")
    archive.add_argument("--executables", type=_path, metavar="FILE")
    archive.add_argument("--sha256", type=_path, metavar="FILE")
    archive.set_defaults(run=_run_archive, errors=(ValueError, OSError))
    compressed = commands.add_parser(
        "compress",
        description=COMPRESS_HELP,
        formatter_class=argparse.RawDescriptionHelpFormatter,
    )
    compressed.add_argument("--src", type=_path, required=True, metavar="FILE")
    compressed.add_argument("--out", type=_path, required=True, metavar="OUT")
    compressed.add_argument("--sha256", type=_path, metavar="FILE")
    compressed.add_argument("--zstd", nargs=argparse.REMAINDER, required=True)
    compressed.set_defaults(
        run=_run_compress,
        errors=(ValueError, OSError, subprocess.CalledProcessError),
    )
    strip = commands.add_parser("strip-types")
    strip.add_argument("--src", type=_path, required=True)
    strip.add_argument("--out", type=_path, required=True)
    strip.set_defaults(run=_run_strip_types, errors=(ValueError, OSError))
    return parser


def main() -> int:
    args = _parser().parse_args()
    try:
        return args.run(args)
    except args.errors as e:
        print(f"{args.command}: {e}", file=sys.stderr)
        return 1


if __name__ == "__main__":
    sys.exit(main())
