#!/usr/bin/env python3
# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

import argparse
import collections
import contextlib
import errno
import hashlib
import io

import marshal
import os
import platform
import re
import shlex
import shutil
import stat
import struct
import subprocess
import sys
import tempfile
import time
from difflib import get_close_matches
from importlib.util import cache_from_source, MAGIC_NUMBER, source_from_cache
from itertools import chain
from os.path import basename, dirname, join, split, splitext
from pathlib import Path

import errors

PAR_STYLES = ["zip", "live", "fastzip", "native", "xar", "pex"]
# TODO(T138941322): We should move toward using select to set par_style
# For now, only buck2 should be hitting make_par for standalone packaging and pex is all we support
DEFAULT_STYLE = "pex" if sys.platform == "win32" else "fastzip"

MANIFEST_MODULE = "__manifest__"
RUN_PAR_MAIN_MODULE = "__run_par_main__"
RUN_LIVEPAR_MAIN_MODULE = "__run_lpar_main__"

DSO_SUFFIX_RE = re.compile(r"\.so(?:\.\d+)*[a-z]?$")

SOURCE_SUFFIX = ".py"
BYTECODE_SUFFIX = ".pyc"
PURE_PYTHON_MODULE_SUFFIXES = (SOURCE_SUFFIX, ".pyc")  # native is a poor name for this
NATIVE_EXTENSION_SUFFIXES = (
    ".so",
    ".pyd",
    # Count native python stubs as extensions so that we properly generate
    # `__init__` files for them which, while not needed for XARs (due to
    # implicit py3 namespacing) is required for fastzip.
    ".empty_stub",
)
MODULE_SUFFIXES = PURE_PYTHON_MODULE_SUFFIXES + NATIVE_EXTENSION_SUFFIXES
EXTENSION_ABI_FLAGS = ("abi3", "cpython-", "cp")

# The earliest representable time in ZIP's timestamp.  We use this in-place
# of current timestamps in the ZIP to make the PARs deterministic.
PAR_EPOCH = (2004, 2, 4, 0, 0, 0, 0, 0, 0)
PAR_EPOCH_TS = time.mktime(PAR_EPOCH)


TargetInfo = collections.namedtuple("TargetInfo", ["lib_path_env", "lib_preload_env"])


def get_target_info(system):
    if system == "darwin":
        return TargetInfo(
            lib_path_env="DYLD_LIBRARY_PATH", lib_preload_env="DYLD_INSERT_LIBRARIES"
        )
    if system == "linux":
        return TargetInfo(lib_path_env="LD_LIBRARY_PATH", lib_preload_env="LD_PRELOAD")
    if system == "windows":
        return TargetInfo(lib_path_env="", lib_preload_env="")
    raise Exception("unknown system: {}".format(system))


def generate_file(path, fn, mode=None, checksum_fn=None):
    # Write to a temporary output file, then atomically
    # rename to the real destination only if we succeed
    (dirname, basename) = os.path.split(path)
    prefix = basename + ".tmp."
    output_file = tempfile.NamedTemporaryFile(dir=dirname, prefix=prefix, delete=False)
    if mode is not None:
        os.chmod(output_file.name, mode)

    # Call the function to generate the file contents
    fn(output_file)

    # Flush the contents of the generated file to disk.
    output_file.flush()

    # if a checksum handler is supplied, generate a hash of the generated
    # file and pass it, along with the location of the temp file to the
    # handler function.
    if checksum_fn is not None:
        hasher = hashlib.md5()
        blocksize = 65536
        output_file.seek(0)
        while True:
            buf = output_file.read(blocksize)
            if len(buf) == 0:
                break
            hasher.update(buf)
        checksum_fn(output_file.name, hasher.hexdigest())

    # Rename the temporary output file to the real destination
    os.rename(output_file.name, path)
    output_file.delete = False
    output_file.close()


def split_module(module_name):
    """
    split_module(module_name) --> (package, relative module)

    Split the module name into the package name and the relative module
    name within the package.  For top-level modules, the package name
    returned will be the empty string.
    """
    parts = module_name.rsplit(".", 1)
    if len(parts) == 1:
        return ("", module_name)
    return parts


def get_compiled_path_from_source(path, zipimport=False):
    """
    Return the path to use for the compiled source.
    """

    assert path.endswith(SOURCE_SUFFIX)

    # PEP-3147: Python 3.2 and later put compiled byte code into __pycache__,
    # unless we're generating compiled zip-style PARs, which must find their
    # compiled code in the PY2 location.
    if sys.version_info[0] >= 3 and not zipimport:
        compiled_path = cache_from_source(path)

    # If __debug__ is unset, the builtin compile() function returns
    # optimized bytecode (.pyo)
    else:
        compiled_path = path + ("c" if __debug__ else "o")

    return compiled_path


def get_source_from_compiled_path(path, zipimport=False):
    """
    Return the path to use for the compiled source.
    """

    assert path.endswith(BYTECODE_SUFFIX)

    # PEP-3147: Python 3.2 and later put compiled byte code into __pycache__,
    # unless we're generating compiled zip-style PARs, which must find their
    # compiled code in the PY2 location.
    if sys.version_info[0] >= 3 and not zipimport:
        source_path = source_from_cache(path)
    else:
        source_path = path[:-1]

    return source_path


def is_suffix_valid(suffix):
    """We're stripping parts of the filename that are put by PEP-3147."""

    for invalid in (".cpython-", ".pypy-", ".opt-", ".pyc", ".pyo"):
        if suffix.startswith(invalid):
            return False

    return True


def redo_path(path):
    """
    Converts paths like fb303/__pycache__/__init__.cpython-35.pyc into
    fb303/__init__.pyc.
    """

    stem = basename(splitext(path)[0])
    while not is_suffix_valid(splitext(stem)[1]):
        stem = basename(splitext(stem)[0])
    assert basename(dirname(path)) == "__pycache__", (
        "Unexpected subdirectories in __pycache__: {}".format(dirname(path)),
    )
    return join(split(dirname(path))[0], stem + ".pyc")


def is_path_in_pycache(path):
    head = path
    while head:
        head, tail = split(head)
        if tail == "__pycache__":
            return True
    return False


def get_user_main(options, manifest):
    """
    Get the name of the user's main module
    """
    main_function = ""
    if options.main_module is not None:
        main_module = options.main_module
        if "/" in main_module:
            # This looks like a relative path to a python module possibly even
            # including extension use path_to_module to turn into a module path
            main_module = path_to_module(main_module)
    elif options.main_function is not None:
        main_module, main_function = options.main_function.rsplit(".", 1)
    else:
        # If no main module name is specified, default to "__main__".
        # (This is what python uses for its own built-in zip archive support.)
        main_module = "__main__"

    # Verify that the user's main module exists
    if manifest and main_module not in manifest.modules:
        if any(
            main_module + suffix in manifest.modules
            for suffix in [".__main__", ".__init__"]
        ):
            return (main_module, main_function)

        candidates = get_close_matches(main_module, manifest.modules.keys(), n=5)
        suffix = None
        if len(candidates) > 0:
            if len(candidates) == 1:
                suffix = "Did you mean %s?" % candidates[0]
            else:
                suffix = "Did you mean one of: %s?" % ", ".join(candidates)

        msg = 'main module "%s" not found in the manifest' % main_module
        if suffix is not None:
            msg = msg + "\n" + suffix
        raise errors.MainModuleError(msg)
    return (main_module, main_function)


def strip_extension_abi(path: str) -> str:
    """
    Given foo/bar.cpython-39-fb-009-x86_64.so -> foo/bar.so
    """
    if path.endswith(NATIVE_EXTENSION_SUFFIXES):
        parts = path.split("/")  # we only want to edit the filename
        module_path, _, ext = parts[-1].partition(".")
        # We do a *suffix because it may be empty
        *suffixes, ext = ext.rsplit(".", 1)
        # This is a safe guard against moving non-module .so's or anything thats weird
        # If there are no suffixes then we don't need to rename.
        if (
            suffixes
            and suffixes[0].startswith(EXTENSION_ABI_FLAGS)
            and suffixes[0].count(".") == 0
        ):
            parts[-1] = f"{module_path}.{ext}"  # we replace the filename
            return "/".join(parts)  # we join back the parts together to get the path
    return path


def unpep3147_bytecode(path: str) -> str:
    """
    Given foo/__pycache__/bar.cpython-39.pyc -> foo/bar.pyc
    Also remove any suffixes between the extension and module name see: pep-488
    """
    if path.endswith(BYTECODE_SUFFIX):
        parts = path.split("/")  # we only want to edit the filename
        parts = [
            p for p in parts if p != "__pycache__"
        ]  # and remove the __pycache__ folder
        module_path, _, ext = parts[-1].partition(".")
        parts[-1] = f"{module_path}{BYTECODE_SUFFIX}"
        return "/".join(parts)
    return path


def path_to_module(path: str) -> str:
    """
    Given a path to a python module file this returns the import path
    This is only safe for dir paths with extension information.  If its already
    a python import path the results can be wrong if the module name happens to be
    a in MODULE_SUFFIXES
    """
    if path.endswith(MODULE_SUFFIXES):
        path = splitext(unpep3147_bytecode(strip_extension_abi(path)))[0]
        path = path.replace("/", ".").replace("\\", ".")
    return path


def is_opt_bytecode(path: str) -> bool:
    if path.endswith(BYTECODE_SUFFIX):
        if "__pycache__" in path:
            name, *parts = path.rsplit(".", 3)
            if (
                len(parts) == 3
                and parts[0].startswith("cpython-")
                and parts[1].startswith("opt-")
            ):
                return True

    return False


def get_libs_needed_for(manifest, roots):
    """
    Return all libraries in the manifest which are transitively needed by the
    libraries in the given preloaded roots.
    """

    # Manifest entries indexed by destination path.
    entry_map = {e.dest_path: e.src_path for e in manifest.entries}

    libs = collections.OrderedDict()

    queued = roots[:]
    while queued:
        lib = queued.pop()
        for needed in find_needed(entry_map[lib]):
            if needed not in libs and needed in entry_map:
                libs[needed] = None
                queued.append(needed)

    return libs.keys()


def find_needed(lib):
    """
    Return all libs listed in the `DT_NEEDED` tags of the given lib.
    """

    needed = []

    out = subprocess.check_output(["objdump", "-p", lib], encoding="utf-8")
    for line in out.splitlines():
        if " NEEDED " in line:
            needed.append(line.split()[1])

    return needed


def get_ld_library_path(options, base_dirs):
    """
    Build the LD_LIBRARY_PATH components passed in via the command line
    options.
    """

    ld_library_paths = []

    for lib_dir in options.ld_library:
        if os.path.isabs(lib_dir):
            ld_library_paths.append(shlex.quote(lib_dir))
        else:
            for base_dir in base_dirs:
                ld_library_paths.append(os.path.join(base_dir, shlex.quote(lib_dir)))

    return ld_library_paths


def format_ld_library_path(path_env, shell_escaped_paths, inherit=True):
    """
    Format the string to set `LD_LIBRARY_PATH` to from the given
    pre-shell-escaped
    """

    ld_library_path = ":".join(shell_escaped_paths)

    # If LD_LIBRARY_PATH is set in the environment when the script is run,
    # we also want to include any paths it contains, so expand $LD_LIBRARY_PATH
    # at runtime too.
    #
    # The following expands to ":<paths>" when $LD_LIBRARY_PATH is set to
    # <paths>, but the empty string when $LD_LIBRARY_PATH is unset.
    # This is important: we don't want a trailing colon if $LD_LIBRARY_PATH.
    # A trailing colon will cause ld.so to also look in the current directory
    # where the program is run from, which is undesirable.
    if inherit:
        ld_library_path += "${{{env}:+:${{{env}}}}}".format(env=path_env)
    # The following expands to ":PAR_APPEND_LD_LIBRARY_PATH" when $PAR_APPEND_LD_LIBRARY_PATH is set
    # This is important: we don't want a trailing colon in $LD_LIBRARY_PATH.
    return (
        ld_library_path + "${PAR_APPEND_LD_LIBRARY_PATH:+:$PAR_APPEND_LD_LIBRARY_PATH}"
    )


def make_ld_library_path(options, lib_path_env, base_dirs):
    return format_ld_library_path(
        lib_path_env,
        get_ld_library_path(options, base_dirs),
        inherit=options.inherit_native_lib_path,
    )


def make_ld_preload(preload_env, libs, base_dir=""):
    # As was the case for `LD_LIBRARY_PATH` above, if `LD_PRELOAD` is already
    # set in the environment when the script is run, we also want to include
    # any libs it mentions, so expand `$LD_PRELOAD` at runtime too.
    if not libs:
        return None

    # Darwin requires full paths to preloaded libs.
    full_libs = [os.path.join(base_dir, l) for l in libs]

    # Bash expansion to preserve existing env setting.
    existing_env = "${{{env}:+:${{{env}}}}}".format(env=preload_env)

    return ":".join(full_libs) + existing_env


def _make_clean_dir(path):
    try:
        os.mkdir(path)
        return
    except OSError as ex:
        if ex.errno != errno.EEXIST:
            return

    # Blow away the old contents
    for entry in os.listdir(path):
        full_path = os.path.join(path, entry)
        s = os.lstat(full_path)
        if stat.S_ISDIR(s.st_mode):
            shutil.rmtree(full_path)
        else:
            os.unlink(full_path)


def is_elf(name, path):
    """
    Quick check for whether the given path refers to an ELF file.
    """
    # Fast path, if we have a DSO suffix, don't even check the file.
    if DSO_SUFFIX_RE.search(name):
        return True

    # Fast path, if we have any other suffix, assume this isn't an ELF file.
    _, ext = os.path.splitext(name)
    if ext:
        return False

    # Fast path, assume
    # runtime/bin/<target-name>#native-main#<bundle-runtime>#<interpreter>#<py_version>
    # is a native python executable
    if "runtime/bin/" in name and "#native-main#" in name:
        return True

    # Otherwise, check for the ELF magic.
    with open(path, mode="rb") as f:
        return f.read(4) == b"\x7fELF"


def _has_debug_info(entries):
    if not entries:
        return []

    paths = [entry.src_path for entry in entries]

    cmd = ["objdump", "--section-headers", "--wide"] + paths
    env = os.environ.copy()

    # Workaround an issue where sandcatle builds don't have the newer objdump
    # in their path.
    if platform.system() == "Linux":
        path = ["/usr/local/fbcode/bin"]
        prev_path = env.get("PATH")
        if prev_path is not None:
            path.append(prev_path)
        env["PATH"] = ":".join(path)
        cmd += ["--dwarf=no-follow-links"]

    # objdump will return an error code if at least one of the files cannot be read
    # but an error reading a file does not affect the rest
    # If some file cannot be read we assume it does not have debug info
    proc = subprocess.run(
        cmd,
        encoding="utf-8",
        env=env,
        capture_output=True,
    )

    has_debugs = []
    buf = io.StringIO(proc.stdout)
    line = buf.readline()
    # Output has at max len(path) items
    # Files that cannot be read have no effect on stdout
    for _path in paths:
        # Skip any empty lines.
        while line == os.linesep:
            line = buf.readline()

        # EOF?
        if not line:
            break
        filename, _ = line.split(":", 1)
        line = buf.readline()
        # There is no empty line when compiled with @mode/opt-mac, otherwise there is.
        if buf.readline() == os.linesep:
            buf.readline()  # "Sections:"
        line = buf.readline()
        # Search through section names for debug sections.
        has_debug = False
        while len(line) > 1:
            section = line.split()[1]
            if (
                has_debug
                # ELF
                or section.startswith(".debug")
                or section.startswith(".zdebug")
                # Mach-O
                or section.startswith("__debug")
                or section.startswith("__zdebug")
            ):
                has_debug = True
            line = buf.readline()
        if has_debug:
            has_debugs.append(filename)

    return [(entry, entry.src_path in has_debugs) for entry in entries]


def has_debug_info(all_entries):
    """
    Quickly check if native objects have debug section.


    Returns a list of tuples of entries and bools, where the bool indicates
    if the entry contains debug info or not
    """

    def chunkify(entries):
        chunk = 512  # ARG_MAX / PATH_MAX
        for i in range(0, len(entries), chunk):
            yield entries[i : i + chunk]

    # use chunks to avoid objdump 'ProcessException: Argument list too long'.
    return chain.from_iterable(
        _has_debug_info(entry_chunk) for entry_chunk in chunkify(all_entries)
    )


def strip_save_debuginfo(
    input,
    output,
    debuginfo,
    strip="strip",
    objcopy="objcopy",
):
    subprocess.check_call([strip, "--only-keep-debug", "-o", debuginfo, input])
    subprocess.check_call(
        [
            objcopy,
            "--strip-debug",
            "--keep-file-symbols",
            "--add-gnu-debuglink=" + debuginfo,
            input,
            output,
        ]
    )


def strip_only(input, output, strip="strip"):
    subprocess.check_call([strip, "-g", "-o", output, input])


def type_safe_add_newline(data):
    if isinstance(data, str) and not data.endswith("\n"):
        return data + "\n"
    elif isinstance(data, bytes) and not data.endswith(b"\n"):
        return data + b"\n"
    else:
        return data


def compile_code(src_data, path, timestamp, zipimport):
    """
    compile_code(src_data, path, timestamp, zipimport)
                 --> (compiled_data, output_path)

    Takes a string containing python source code, and returns compiled data
    suitable for putting in a .pyc file.

    Returns a tuple of (compiled_data, output_path)
    """
    # Make sure the data ends with a newline;
    # Otherwise python will report a syntax error.
    fixed_src_data = type_safe_add_newline(src_data)

    # Compile the source data into a code object
    code_obj = compile(fixed_src_data, path, "exec")

    # Dump the code object into a string suitable for putting in
    # a .pyc/.pyo file.  This logic is similar to what py_compile.compile()
    # does.
    magic = MAGIC_NUMBER
    # Python 3.7 and later has 4 bytes of dummy data after magic.
    # If this breaks again we should probably just move to py_compile.compile()
    if sys.version_info[:2] >= (3, 7):
        magic += bytes(4)  # zero for now it seems.
    ts_data = struct.pack("<I", int(timestamp))
    sz_data = struct.pack("<I", len(src_data))
    compiled_code = marshal.dumps(code_obj)
    compiled_data = magic + ts_data
    if sys.version_info[0] >= 3:
        compiled_data += sz_data
    compiled_data += compiled_code

    # Determine the output path name.
    if path.endswith(SOURCE_SUFFIX):
        output_path = path
    else:
        output_path = path + SOURCE_SUFFIX

    output_path = get_compiled_path_from_source(output_path, zipimport)

    return [compiled_data, output_path]


def compile_files(python, dest, files, zipimport=False):
    """
    Compile list of python files.
    files: list of files to compile in format [(src, relative_dest, data), ...]
    dest: directory to put compiled files in
    python: python executable to compile with
    zipimport: flag to determine where the compile files should be placed.
               For zip packages the .pyc is located in the same directory as the .py
    """

    script = f"""
import py_compile, sys
from importlib.util import cache_from_source
from pathlib import Path

def get_compiled_path_from_source(path, zipimport=False):
    if sys.version_info[0] >= 3 and not zipimport:
        return cache_from_source(path)
    else:
        return path + ("c" if __debug__ else "o")

dest = Path("{dest}")

for src, relative_dest, _ in {files}:
    dest_file = dest.joinpath(relative_dest)
    src = Path(src) if src else dest_file
    dest_file.parent.mkdir(parents=True, exist_ok=True)
    cfile = get_compiled_path_from_source(dest_file.as_posix(), zipimport={zipimport})
    # If src is not present assume it was written to the destination
    py_compile.compile(src, cfile=cfile, invalidation_mode=py_compile.PycInvalidationMode.UNCHECKED_HASH)
"""

    path = Path(dest).joinpath("compile.py")
    with path.open("wb") as temp_script:
        temp_script.write(script.encode("utf-8"))

    cmd = [python, path]
    subprocess.run(cmd)
    path.unlink()


@contextlib.contextmanager
def timed_ms(sample, name):
    start = time.time()
    try:
        yield
    finally:
        sample["int"][name] = int((time.time() - start) * 1000)


def warn(msg, *args, **kwargs):
    print(sys.argv[0] + ": " + msg.format(*args, **kwargs), file=sys.stderr)


def get_args_parser(**kwargs):
    parser = argparse.ArgumentParser(**kwargs)

    parser.add_argument("--manifest", nargs="+", default=[])
    # Should be set for par_style = pex
    parser.add_argument(
        "--pex-component-manifest", default=None, help=argparse.SUPPRESS
    )
    entry_point = parser.add_mutually_exclusive_group()
    entry_point.add_argument(
        "-m",
        "--main-module",
        action="store",
        default=None,
        metavar="MODULE",
        help="run MODULE as the main program",
    )
    entry_point.add_argument(
        "--main-function",
        help="Fully qualified name of the function that serves as the entry point.",
    )
    parser.add_argument(
        "--main-runner",
        help=(
            "Fully qualified name of the function that handles invoking"
            " the executable's entry point."
        ),
        required=True,
    )
    parser.add_argument(
        "-L",
        "--ld-library",
        action="append",
        default=[],
        metavar="LIB_PATH",
        help="add LIB_PATH to library path env var",
    )
    parser.add_argument(
        "--ld-preload",
        action="append",
        default=[],
        metavar="LIB",
        help="export LIB to library preload env var",
    )
    parser.add_argument(
        "-e",
        "--runtime_env",
        action="append",
        default=[],
        help="environment variables to set before launching the runtime. (e.g. -e FOO=BAR BAZ=QUX)",
    )
    parser.add_argument(
        "--runtime-manifest",
        help="A json file containing details about the runtime to bundle",
        default=None,
        action="store",
    )
    parser.add_argument(
        "--runtime-root",
        help="The root dir where the runtime should be copied from",
        default=None,
        action="store",
    )
    parser.add_argument(
        "--inherit-native-lib-path",
        action="store_true",
        default=True,
        help="inherit pre-existng native lib paths set in the env at runtime (e.g. `LD_LIBRARY_PATH`)",
    )
    parser.add_argument(
        "--no-inherit-native-lib-path",
        action="store_false",
        dest="inherit_native_lib_path",
    )
    parser.add_argument(
        "--target",
        action="store",
        default=platform.system().lower(),
        metavar="TARGET",
        help="the system (e.g. linux, darwin) the PAR will run on",
    )
    parser.add_argument(
        "-o",
        "--output",
        help="write the archive to FILE",
        metavar="FILE",
        required=True,
    )
    parser.add_argument(
        "-O",
        "--optimize",
        default=False,
        action="store_true",
        help="Create an optimized PAR, which persists unzipped " "between runs",
    )
    parser.add_argument(
        "--debug-info",
        choices=("keep", "strip", "separate", "extract"),
        default="keep",
        help="What to do with debug symbols in shared libraries."
        "Keep means leave the libraries as is. Strip "
        "removes the debug symbols. Separate means to "
        "extract the debuginfo into a separate file within "
        "the final archive. Extract means to strip the "
        "debug info but keep it in a file in the build "
        "folder",
    )
    parser.add_argument(
        "-p",
        "--python",
        default="python",
        help="use PYTHON as the interpreter when " "running the PAR",
        metavar="PYTHON",
    )
    parser.add_argument(
        "-T",
        "--no-strict-tabs",
        action="store_false",
        dest="strict_tabs",
        default=True,
        help="",
    )
    parser.add_argument(
        "-I",
        "--no-add-inits",
        action="store_false",
        dest="add_inits",
        default=True,
        help="Do not add missing __init__.py files",
    )
    parser.add_argument(
        "--compiler",
        type=Path,
        help="Path to interpreter to use to compile bytecode",
    )
    parser.add_argument(
        "-C",
        "--no-compile",
        action="store_false",
        dest="compile",
        default=True,
        help="Disable generating compiled .pyc files",
    )
    parser.add_argument(
        "-S",
        "--store-source",
        action="store_true",
        dest="store_source",
        default=True,
        help="Include .py files in par file in addition to .pyc " "files",
    )
    parser.add_argument(
        "--compress", default=False, help="compress files stored in par file"
    )
    parser.add_argument(
        "--additional-interpreter",
        action="append",
        dest="additional_interps",
        default=[],
        help="additional interpreters to generate lpar " "wrappers for",
    )
    parser.add_argument(
        "-s",
        "--par-style",
        action="store",
        choices=PAR_STYLES,
        default=DEFAULT_STYLE,
        help="The style of archive to generate (%s)" % ", ".join(PAR_STYLES),
    )
    parser.add_argument(
        "--live-link-tree",
    )
    parser.add_argument("--warnings", help="Warnings to enable when executing PAR")
    parser.add_argument(
        "--argcomplete",
        dest="argcomplete_hint",
        action="store_const",
        const="PYTHON_ARGCOMPLETE_OK",
        default="PYTHON_ARGCOMPLETE_DISABLED",
        help="Add autocompletion hint (PYTHON_ARGCOMPLETE_OK)",
    )
    parser.add_argument(
        "-H",
        "--comment",
        default="",
        help="A string that will be placed as commented text"
        "near the top of the par file; particularly for "
        "noting the rules used to generate the file",
    )
    parser.add_argument(
        "--disable-usage-logging",
        default=False,
        action="store_true",
        help="Do not include code to log tool usage to scribe",
    )
    parser.add_argument(
        "--no-manifest",
        action="store_false",
        dest="generate_manifest",
        default=True,
        help="Don't generate a `__manifest__` module with metadata about the PAR.",
    )
    parser.add_argument(
        "--manifest-env",
        action="append",
        default=[],
        help="Env to write to manifest",
    )
    parser.add_argument(
        "--strip",
        default="strip",
        help="Path to strip utility",
    )
    parser.add_argument(
        "--objcopy",
        default="objcopy",
        help="Path to objcopy utility",
    )

    parser.add_argument(
        "--xar-compression-level",
        action="store",
        metavar="LEVEL",
        type=int,
        default=3,
        help="The ZSTD compressiong level to use when building the squashfs",
    )
    parser.add_argument(
        "--mksquashfs",
        default="/usr/sbin/mksquashfs",
        metavar="MKSQUASHFS",
        help="Path to mksquashfs utility",
    )
    parser.add_argument(
        "--extra-xar-trampoline-names",
        default=[],
        action="append",
        help="Additional trampoline names. Only valid for XAR style pars",
    )
    parser.add_argument(
        "--xar-verifier",
        help="Path to xar verifier utility",
    )
    parser.add_argument(
        "--build-info-python-home",
        default=sys.prefix,
        help="The Python home to embed in the build info manifest",
    )
    parser.add_argument(
        "--build-info-value",
        default=[],
        dest="build_info_values",
        action="append",
        help="<name>=<json-val>",
    )
    # PEX Specific
    parser.add_argument(
        "--pex-directory", action="store_true", default=False, help=argparse.SUPPRESS
    )
    parser.add_argument("--pex-python-version", default="", help=argparse.SUPPRESS)
    parser.add_argument("--pex-python-shebang", default=None, help=argparse.SUPPRESS)
    parser.add_argument(
        "--pex-absolute-shebang",
        action="store_true",
        default=False,
        help=argparse.SUPPRESS,
    )
    parser.add_argument(
        "--pex-no-zip-safe",
        action="store_false",
        dest="pex_zip_safe",
        default=True,
        help=argparse.SUPPRESS,
    )
    return parser
