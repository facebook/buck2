#!/usr/bin/env python3

"""
Build a standalone PEX (Python EXecutable) file.

A PEX is a zip file with a shebang prepended. Python can execute zip files
directly: when you run `python3 archive.pex`, Python finds `__main__.py`
inside the zip and runs it. With a shebang (`#!/usr/bin/env python3`), the
OS can run it directly too: `./archive.pex`.

This tool replaces Meta's internal `make_par` standalone builder. It accepts
the same CLI interface that the Buck2 prelude's `make_py_package` action
generates (module manifests, bootstrap args, etc.) and produces a single
self-contained executable.

The generated PEX bootstrap (__main__.py) works as follows:
  1. Extracts the zip to a cache directory (~/.cache/par/<key>/)
  2. Sets LD_LIBRARY_PATH for native libraries
  3. Sets PYTHONPATH to the extracted modules
  4. Re-execs Python so the dynamic linker picks up LD_LIBRARY_PATH
     (glibc caches LD_LIBRARY_PATH at process start, so os.environ changes
     after startup don't affect dlopen — re-exec is required)
  5. The re-exec'd Python loads the entry point via __par__.bootstrap.run_as_main

Why extract instead of running from the zip?
  - Native extensions (.so) cannot be dlopen'd from inside a zip file
  - Even "zip-safe" pure Python code benefits from extraction when native
    libraries are present, since LD_LIBRARY_PATH must be set before Python
    starts loading extensions

CLI interface (matches what make_py_package.bzl generates):

  Module args (via @argfile expansion):
    --module-manifest=PATH    JSON: [[dest, src, origin], ...]
    --resource-manifest=PATH  JSON: [[dest, src, origin], ...]
    --native-library-src=PATH  .so file to bundle
    --native-library-dest=PATH relative dest for the .so

  Bootstrap args:
    --python INTERP           interpreter for shebang
    --host-python INTERP      host interpreter (unused, for compat)
    --entry-point MODULE      module to run (mutually exclusive with --main-function)
    --main-function MOD.FUNC  function entry point
    --main-runner MOD.FUNC    bootstrap runner (default: __par__.bootstrap.run_as_main)
    --no-zip-safe             force extraction mode (always on when native libs present)
    --native-library-runtime-path=PATH  extra LD_LIBRARY_PATH entries
    --preload=PATH            libraries to LD_PRELOAD
    OUTPUT                    output .pex file path (positional)

  Passthrough:
    --passthrough ARG         forwarded to bootstrap (e.g. --runtime_env=K=V)
"""

import argparse
import json
import os
import sys
import tempfile
import zipfile
from pathlib import Path


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(
        description="Create a standalone PEX file",
        fromfile_prefix_chars="@",
    )

    # -- Module args (from _pex_modules_args / _pex_modules_common_args) --
    parser.add_argument(
        "--module-manifest",
        action="append",
        dest="module_manifests",
        default=[],
    )
    parser.add_argument(
        "--resource-manifest",
        action="append",
        dest="resource_manifests",
        default=[],
    )
    parser.add_argument(
        "--native-library-src",
        type=Path,
        dest="native_library_srcs",
        action="append",
        default=[],
    )
    parser.add_argument(
        "--native-library-dest",
        type=Path,
        dest="native_library_dests",
        action="append",
        default=[],
    )

    # -- Bootstrap args (from _pex_bootstrap_args) --
    parser.add_argument(
        "--preload",
        dest="preload_libraries",
        action="append",
        default=[],
    )
    parser.add_argument("--python", required=True)
    parser.add_argument("--host-python", required=True)

    entry_point = parser.add_mutually_exclusive_group(required=True)
    entry_point.add_argument("--entry-point")
    entry_point.add_argument("--main-function")

    parser.add_argument("--main-runner", required=True)
    parser.add_argument("--no-zip-safe", action="store_true")
    parser.add_argument(
        "--native-library-runtime-path",
        dest="native_library_runtime_paths",
        action="append",
        default=[],
    )

    # Positional output path
    parser.add_argument("output", type=Path)

    # -- Compatibility / passthrough --
    parser.add_argument("--passthrough", action="append", default=[])
    parser.add_argument("--modules-dir", type=Path, default=None)
    parser.add_argument("--dwp-src", dest="dwp_srcs", action="append", default=[])
    parser.add_argument("--dwp-dest", dest="dwp_dests", action="append", default=[])
    parser.add_argument("--debuginfo-src", dest="debuginfo_srcs", action="append", default=[])
    parser.add_argument(
        "--bytecode-artifacts",
        dest="bytecode_artifacts",
        action="append",
        default=[],
    )
    parser.add_argument("--omnibus-debug-info", default=None)

    return parser.parse_args()


# ---------------------------------------------------------------------------
# File collection
# ---------------------------------------------------------------------------


MODULE_SUFFIXES = {".py", ".so", ".pyd", ".dll"}


def collect_files(
    args: argparse.Namespace,
) -> dict[str, str]:
    """Return {zip_dest_path: abs_src_path} for every file to bundle."""
    files: dict[str, str] = {}

    for manifest_path in args.module_manifests:
        with open(manifest_path) as f:
            for dest, src, _origin in json.load(f):
                files[dest] = src

    for manifest_path in args.resource_manifests:
        with open(manifest_path) as f:
            for dest, src, _origin in json.load(f):
                files[dest] = src

    for src, dest in zip(args.native_library_srcs, args.native_library_dests):
        files[str(dest)] = str(src)

    return files


def compute_init_pys(files: dict[str, str]) -> set[str]:
    """Compute __init__.py paths needed so Python recognises packages."""
    init_dirs: set[Path] = set()
    for dest in files:
        p = Path(dest)
        if p.suffix in MODULE_SUFFIXES:
            package = p.parent
            while package != Path("") and package != Path("."):
                init_dirs.add(package)
                package = package.parent

    needed: set[str] = set()
    for d in init_dirs:
        init_path = str(d / "__init__.py")
        if init_path not in files:
            needed.add(init_path)
    return needed


# ---------------------------------------------------------------------------
# Bootstrap generation
# ---------------------------------------------------------------------------


def parse_passthrough_env(passthroughs: list[str]) -> dict[str, str]:
    env: dict[str, str] = {}
    for arg in passthroughs:
        if arg.startswith("--runtime_env="):
            kv = arg[len("--runtime_env=") :]
            k, _, v = kv.partition("=")
            if k:
                env[k] = v
    return env


def generate_main_py(args: argparse.Namespace) -> str:
    """Generate the __main__.py bootstrap that goes inside the zip."""
    main_module: str
    main_function: str
    if args.main_function:
        mod, _, func = args.main_function.rpartition(".")
        main_module = mod
        main_function = func
    else:
        main_module = args.entry_point
        main_function = ""

    main_runner_module, _, main_runner_function = args.main_runner.rpartition(".")
    runtime_env = parse_passthrough_env(args.passthrough)

    if args.no_zip_safe:
        preload_basenames = [os.path.basename(p) for p in args.preload_libraries]
        template = _BOOTSTRAP_EXTRACT_TEMPLATE
        template = template.replace("<PRELOAD_BASENAMES>", repr(preload_basenames))
        template = template.replace("<NATIVE_RUNTIME_PATHS>", repr(args.native_library_runtime_paths))
    else:
        template = _BOOTSTRAP_ZIP_TEMPLATE

    template = template.replace("<MAIN_MODULE>", repr(main_module))
    template = template.replace("<MAIN_FUNCTION>", repr(main_function))
    template = template.replace("<MAIN_RUNNER_MODULE>", main_runner_module)
    template = template.replace("<MAIN_RUNNER_FUNCTION>", main_runner_function)
    template = template.replace("<RUNTIME_ENV>", repr(runtime_env))
    return template


_BOOTSTRAP_ZIP_TEMPLATE = """\
import os
import sys

MAIN_MODULE = <MAIN_MODULE>
MAIN_FUNCTION = <MAIN_FUNCTION>
RUNTIME_ENV = <RUNTIME_ENV>

for k, v in RUNTIME_ENV.items():
    os.environ[k] = v

from <MAIN_RUNNER_MODULE> import <MAIN_RUNNER_FUNCTION> as _run_as_main
_run_as_main(MAIN_MODULE, MAIN_FUNCTION)
"""

_BOOTSTRAP_EXTRACT_TEMPLATE = """\
import fcntl
import hashlib
import os
import platform
import signal
import subprocess
import sys
import time
import zipfile

MAIN_MODULE = <MAIN_MODULE>
MAIN_FUNCTION = <MAIN_FUNCTION>
MAIN_RUNNER_MODULE = "<MAIN_RUNNER_MODULE>"
MAIN_RUNNER_FUNCTION = "<MAIN_RUNNER_FUNCTION>"
PRELOAD_BASENAMES = <PRELOAD_BASENAMES>
NATIVE_RUNTIME_PATHS = <NATIVE_RUNTIME_PATHS>
RUNTIME_ENV = <RUNTIME_ENV>


def _par_path():
    return os.path.abspath(sys.argv[0])


def _cache_dir(par):
    st = os.stat(par)
    key = "{}:{}:{}".format(par, st.st_size, st.st_mtime_ns)
    digest = hashlib.sha256(key.encode()).hexdigest()[:16]
    base = os.environ.get(
        "PAR_TEMP_DIR",
        os.path.join(os.path.expanduser("~"), ".cache", "par"),
    )
    return os.path.join(base, "{}--{}".format(os.path.basename(par), digest))


def _extract(par, dest):
    stamp = os.path.join(dest, ".par_extracted")
    if os.path.exists(stamp):
        return

    os.makedirs(dest, exist_ok=True)
    lock_path = dest + ".lock"
    with open(lock_path, "w") as lf:
        # fcntl.lockf wraps POSIX fcntl() locks; prefer it over flock() which is not
        # in POSIX. See https://apenwarr.ca/log/20101213
        fcntl.lockf(lf, fcntl.LOCK_EX)
        try:
            if os.path.exists(stamp):
                return
            with zipfile.ZipFile(par, "r") as zf:
                zf.extractall(dest)
            for root, _dirs, filenames in os.walk(dest):
                for fn in filenames:
                    if fn.endswith(".so") or ".so." in fn:
                        p = os.path.join(root, fn)
                        os.chmod(p, os.stat(p).st_mode | 0o555)
            with open(stamp, "w") as sf:
                sf.write(par + "\\n")
        finally:
            fcntl.lockf(lf, fcntl.LOCK_UN)


def main():
    os.environ["PAR_LAUNCH_TIMESTAMP"] = str(time.time())

    par = _par_path()
    cache = _cache_dir(par)
    _extract(par, cache)

    for k, v in RUNTIME_ENV.items():
        os.environ[k] = v

    if platform.system() == "Darwin":
        native_env = "DYLD_LIBRARY_PATH"
    else:
        native_env = "LD_LIBRARY_PATH"

    lib_dirs = [cache] + NATIVE_RUNTIME_PATHS
    old_lib_path = os.environ.get(native_env, "")
    if old_lib_path:
        os.environ["FB_SAVED_" + native_env] = old_lib_path
    os.environ[native_env] = os.pathsep.join(
        d for d in lib_dirs + [old_lib_path] if d
    )

    if PRELOAD_BASENAMES:
        if platform.system() == "Darwin":
            preload_env = "DYLD_INSERT_LIBRARIES"
        else:
            preload_env = "LD_PRELOAD"
        preload_paths = [os.path.join(cache, b) for b in PRELOAD_BASENAMES]
        old_preload = os.environ.get(preload_env, "")
        if old_preload:
            os.environ["FB_SAVED_" + preload_env] = old_preload
        os.environ[preload_env] = os.pathsep.join(
            p for p in preload_paths + [old_preload] if p
        )

    old_pythonpath = os.environ.get("PYTHONPATH", "")
    if old_pythonpath:
        os.environ["FB_SAVED_PYTHONPATH"] = old_pythonpath
    os.environ["PYTHONPATH"] = cache

    os.environ["PAR_INVOKED_NAME_TAG"] = sys.argv[0]

    startup = (
        "# " + sys.argv[0] + "\\n"
        "def __run():\\n"
        "    import sys\\n"
        "    assert sys.argv[0] == \\"-c\\"\\n"
        "    sys.argv[0] = " + repr(sys.argv[0]) + "\\n"
        "    if \\"\\" in sys.path:\\n"
        "        sys.path.remove(\\"\\")\\n"
        "    from <MAIN_RUNNER_MODULE> import <MAIN_RUNNER_FUNCTION> as run_as_main\\n"
        "    run_as_main(" + repr(MAIN_MODULE) + ", " + repr(MAIN_FUNCTION) + ")\\n"
        "__run()\\n"
    )

    args = [sys.executable, "-c", startup] + sys.argv[1:]

    if platform.system() == "Windows":
        p = subprocess.Popen(args)
        def handler(signum, frame):
            if signum == signal.SIGINT:
                p.send_signal(signal.CTRL_C_EVENT)
            else:
                p.terminate()
        signal.signal(signal.SIGINT, handler)
        p.wait()
        sys.exit(p.returncode)
    else:
        os.execv(sys.executable, args)


main()
"""


# ---------------------------------------------------------------------------
# PEX assembly
# ---------------------------------------------------------------------------


import time as _time

_ZIP_EPOCH = (1980, 1, 1, 0, 0, 0)


def _zip_add_file(zf: zipfile.ZipFile, src: str, dest: str) -> bool:
    """Add a file to the zip, clamping timestamps for Nix store files.

    Returns False if the file could not be read (e.g. broken symlink).
    """
    try:
        st = os.stat(src)
    except FileNotFoundError:
        print(
            "warning: skipping {} (broken symlink or missing)".format(src),
            file=sys.stderr,
        )
        return False
    mtime = _time.localtime(st.st_mtime)
    date_time = (
        mtime.tm_year,
        mtime.tm_mon,
        mtime.tm_mday,
        mtime.tm_hour,
        mtime.tm_min,
        mtime.tm_sec,
    )
    if date_time < _ZIP_EPOCH:
        date_time = _ZIP_EPOCH
    info = zipfile.ZipInfo(dest, date_time)
    info.compress_type = zipfile.ZIP_DEFLATED
    info.external_attr = (st.st_mode & 0xFFFF) << 16
    with open(src, "rb") as f:
        zf.writestr(info, f.read())
    return True


def build_pex(args: argparse.Namespace) -> None:
    files = collect_files(args)

    if not args.no_zip_safe:
        native_files = [
            d for d in files if d.endswith(".so") or ".so." in d or d.endswith(".pyd") or d.endswith(".dll")
        ]
        if native_files:
            print(
                "error: PEX contains native extensions but zip_safe is not False.\n"
                "Native .so files cannot be loaded from inside a zip.\n"
                "Set zip_safe = False on your python_binary target. The resulting\n"
                "pex will extract itself to a cache directory and execute from there.\n"
                "First 5 native files:\n" + "\n".join("  " + f for f in native_files[:5]),
                file=sys.stderr,
            )
            sys.exit(1)

    init_pys = compute_init_pys(files)
    main_py = generate_main_py(args)

    python = args.python
    if os.path.isabs(python):
        shebang = "#!{}\n".format(python)
    else:
        shebang = "#!/usr/bin/env {}\n".format(python)

    output_dir = args.output.parent
    output_dir.mkdir(parents=True, exist_ok=True)

    fd, tmp_path = tempfile.mkstemp(
        dir=output_dir,
        prefix=".{}.tmp.".format(args.output.name),
    )
    try:
        with os.fdopen(fd, "wb") as f:
            f.write(shebang.encode("utf-8"))

            with zipfile.ZipFile(f, "w", zipfile.ZIP_DEFLATED) as zf:
                zf.writestr("__main__.py", main_py)

                for init_path in sorted(init_pys):
                    zf.writestr(init_path, "")

                for dest, src in sorted(files.items()):
                    _zip_add_file(zf, src, dest)

        os.chmod(tmp_path, 0o755)
        os.rename(tmp_path, args.output)
    except BaseException:
        try:
            os.unlink(tmp_path)
        except OSError:
            pass
        raise


def main() -> None:
    args = parse_args()
    build_pex(args)


if __name__ == "__main__":
    main()
