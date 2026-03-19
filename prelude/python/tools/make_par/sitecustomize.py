#!/usr/bin/env python3
# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

# pyre-strict

from __future__ import annotations

import itertools
import multiprocessing.util as mp_util
import os
import sys
import threading
import warnings
from importlib.machinery import PathFinder
from importlib.util import module_from_spec
from typing import Callable

lock = threading.Lock()


def __patch_ctypes(saved_env: dict[str, str]) -> None:
    from ctypes import util as ctypes_util

    orig_find_library: Callable[[str], str | None] = ctypes_util.find_library

    def _patched_find_library(name: str) -> str | None:
        if sys.platform == "darwin":
            from ctypes.macholib.dyld import dyld_find

            env = {}
            env.update(os.environ)
            env.update(saved_env)
            for candidate in [
                f"lib{name}.dylib",
                f"{name}.dylib",
                f"{name}.framework/{name}",
            ]:
                try:
                    return dyld_find(candidate, env=env)
                except ValueError:
                    continue
        elif os.name == "posix":
            from pathlib import Path

            for path in saved_env.get("LD_LIBRARY_PATH", "").split(os.pathsep):
                if not path:
                    continue
                p = Path(path)
                try:
                    if not p.exists() or not p.is_dir():
                        continue
                    for candidate in [
                        f"{name}.so",
                        f"lib{name}.so",
                        f"{name}.so.[0-9]",
                        f"lib{name}.so.[0-9]",
                    ]:
                        for item in p.glob(candidate):
                            return str(item)
                except Exception:
                    continue

        return orig_find_library(name)

    # the weird lambda is just to satisfy Pyre:
    # ctypes.util.find_library is declared to have type
    # `typing.Callable(ctypes_util.find_library)[[Named(name, str)],
    # Optional[str]]` but is used as type
    # `typing.Callable(__patch_ctypes._patched_find_library)[[Named(name, str)],
    # Optional[str]]`
    ctypes_util.find_library = lambda name: _patched_find_library(name)


def _write_proxy_sitecustomize() -> None:
    """Write a proxy sitecustomize.py to the unpack dir for child processes.

    Only needed for fastzip PARs where sitecustomize.py lives inside the zip
    and can't be found via PYTHONPATH (zip can't be in PYTHONPATH due to
    RecursionError risk in zipimport).
    """
    unpack_dir = os.environ.get("FB_PAR_UNZIP_LOCATION")
    par_filename = os.environ.get("FB_PAR_FILENAME")
    if not unpack_dir or not par_filename:
        return
    if not any(entry.startswith(("/proc/self/fd/", "/dev/fd/")) for entry in sys.path):
        return  # Not a fastzip PAR — real sitecustomize is findable

    proxy_path = os.path.join(unpack_dir, "sitecustomize.py")
    if os.path.exists(proxy_path):
        return  # Already written

    proxy_code = """\
import os as _os, sys as _sys
_par = _os.environ.get("FB_PAR_FILENAME", "")
if _par and _os.path.isfile(_par) and _par not in _sys.path:
    _sys.path.insert(0, _par)
    # Find and execute the real sitecustomize from the PAR without manipulating
    # sys.modules, which can cause import machinery issues.
    from importlib.machinery import PathFinder
    from importlib.util import module_from_spec
    _spec = PathFinder.find_spec("sitecustomize", path=[_par])
    if _spec and _spec.loader:
        _mod = module_from_spec(_spec)
        _spec.loader.exec_module(_mod)
"""
    try:
        with open(proxy_path, "w") as f:
            f.write(proxy_code)
    except OSError:
        pass  # Best effort


def __patch_spawn(var_names: list[str], saved_env: dict[str, str]) -> None:
    std_spawn = mp_util.spawnv_passfds

    # Compute resolved PYTHONPATH once at patch time (not per-spawn).
    # dirs_only=True filters out zip files to avoid RecursionError from
    # zipimport's lazy `import struct` cycle.
    resolved_pythonpath = os.path.pathsep.join(
        _resolve_path_entries(sys.path, dirs_only=True)
    )

    # pyre-fixme[53]: Captured variable is not annotated.
    # pyre-fixme[2]: Parameter must be annotated.
    def spawnv_passfds(path, args, passfds) -> None | int:
        with lock:
            try:
                _write_proxy_sitecustomize()
                for var in var_names:
                    val = os.environ.get(var, None)
                    if val is not None:
                        os.environ["FB_SAVED_" + var] = val
                    saved_val = saved_env.get(var, None)
                    if saved_val is not None:
                        os.environ[var] = saved_val

                # Ensure PYTHONPATH includes resolved sys.path dirs so the
                # child interpreter finds sitecustomize.py during Py_Initialize.
                existing = os.environ.get("PYTHONPATH", "")
                if existing:
                    os.environ["PYTHONPATH"] = (
                        existing + os.path.pathsep + resolved_pythonpath
                    )
                else:
                    os.environ["PYTHONPATH"] = resolved_pythonpath

                return std_spawn(path, args, passfds)
            finally:
                __clear_env(apply_monkeypatching=False)

    mp_util.spawnv_passfds = spawnv_passfds


def _resolve_path_entries(path: list[str], dirs_only: bool = False) -> list[str]:
    resolved = []
    for entry in path:
        if entry.startswith("/proc/self/fd/"):
            try:
                entry = os.readlink(entry)
            except OSError:
                pass
        elif entry.startswith("/dev/fd/"):
            # On macOS, /dev/fd/N is not a symlink (it has dup() semantics),
            # so os.readlink doesn't work.  Resolve to the original PAR path
            # via the FB_PAR_FILENAME environment variable.
            par_filename = os.environ.get("FB_PAR_FILENAME", "")
            if par_filename:
                entry = par_filename
        if not dirs_only or os.path.isdir(entry):
            resolved.append(entry)
    return resolved


def __patch_spawn_preparation_data() -> None:
    # Only needed for fastzip PARs, which use /proc/self/fd/<N> (Linux) or
    # /dev/fd/<N> (macOS) paths in sys.path. Other PAR styles (e.g. xar) use
    # real filesystem paths and don't need this patch.
    #
    # We must also avoid importing multiprocessing.spawn during sitecustomize
    # for xar PARs: importing it captures sys.executable in a module-level
    # _python_exe variable (via set_executable(sys.executable)), but xar PARs
    # fix up sys.executable later in __run_xar_main__.py. Importing too early
    # would cause multiprocessing to spawn subprocesses (e.g. the resource
    # tracker) with the wrong executable, crashing them.
    if not any(entry.startswith(("/proc/self/fd/", "/dev/fd/")) for entry in sys.path):
        return

    import multiprocessing.spawn as mp_spawn

    orig_get_preparation_data = mp_spawn.get_preparation_data

    def get_preparation_data(name):  # type: ignore
        d = orig_get_preparation_data(name)
        if "sys_path" in d:
            d["sys_path"] = _resolve_path_entries(d["sys_path"])
        return d

    mp_spawn.get_preparation_data = get_preparation_data


def __patch_subprocess_run(saved_env: dict[str, str]) -> None:
    import subprocess
    from functools import wraps

    std_run = subprocess.run

    if sys.platform == "darwin":
        _lib_path_vars = ("DYLD_LIBRARY_PATH", "DYLD_INSERT_LIBRARIES")
    else:
        _lib_path_vars = ("LD_LIBRARY_PATH", "LD_PRELOAD")

    @wraps(std_run)
    # pyre-fixme[2]: Parameter must be annotated.
    # pyre-fixme[53]: Captured variable `std_run` is not annotated.
    def _patched_run(args, env=None, **kwargs) -> subprocess.CompletedProcess[str]:
        if (
            args
            and isinstance(args, (list, tuple))
            # We use is for a few reasons:
            # 1) It's super conservative, you must literally be passing sys.executable
            # 2) We don't need to worry about checking the type if you're passing a path
            and args[0] is sys.executable
            and (env is None or "PYTHONPATH" not in env)
            and (env is None or "PYTHONHOME" not in env)
        ):
            # make subprocess.run work with par files when invoking sys.executable
            if env is None:
                env = os.environ.copy()
            # Resolve /proc/self/fd/<N> paths (used by fastzip PARs) to real
            # filesystem paths so the child process can find modules (including
            # sitecustomize.py which clears PYTHONHOME).
            # dirs_only=True filters out .par zip files from PYTHONPATH to
            # avoid RecursionError during early Python init (zipimport's
            # _read_directory triggers a lazy `import struct` cycle).
            resolved = _resolve_path_entries(sys.path, dirs_only=True)
            env["PYTHONPATH"] = os.path.pathsep.join(resolved)
            # Ensure the proxy sitecustomize.py exists in the unpack dir
            # so the child interpreter can find it via PYTHONPATH.
            _write_proxy_sitecustomize()
            # Only set PYTHONHOME if the child will find sitecustomize.py
            # (which clears it). Otherwise PYTHONHOME leaks and the child
            # uses the PAR's prefix for stdlib, causing hangs.
            if any(
                os.path.isfile(os.path.join(d, "sitecustomize.py")) for d in resolved
            ):
                env["PYTHONHOME"] = sys.prefix
            # Restore library path env vars so the child process can find
            # bundled native libraries (e.g. libpython, libX11).
            for var in _lib_path_vars:
                if var not in env and var in saved_env:
                    env[var] = saved_env[var]

        return std_run(args, env=env, **kwargs)

    subprocess.run = _patched_run


def __patch_resource_tracker_fork() -> None:
    """
    Fix deadlock between multiprocessing resource tracker and forked children.

    SEV: S630420
    Upstream Python issue: https://github.com/python/cpython/issues/88887

    Problem: Python 3.12 added ResourceTracker.__del__ which calls waitpid() on
    the tracker subprocess during shutdown. The tracker subprocess exits when it
    sees EOF on its pipe. However, if a child process was forked (e.g., by
    jetter's get_temp_dir), it inherits the tracker's pipe FD. The parent's
    close() doesn't produce EOF because the child still holds the FD open.
    Result: waitpid() blocks forever → DEADLOCK.

    Fix: Register an after_in_child callback that closes the inherited tracker
    pipe FD and resets tracker state. This ensures:
    1. The child doesn't keep the tracker pipe alive
    2. If the child later uses multiprocessing, it starts a fresh tracker
    """
    # only relevant for Linux
    if sys.platform != "linux":
        return

    def _reset_tracker_in_child() -> None:
        # Check sys.modules instead of importing to avoid pulling in
        # multiprocessing for processes that never use it
        rt_mod = sys.modules.get("multiprocessing.resource_tracker")
        if rt_mod is None:
            return
        tracker = getattr(rt_mod, "_resource_tracker", None)
        if tracker is None:
            return
        fd = getattr(tracker, "_fd", None)
        if fd is not None:
            # Reset state before closing to avoid TOCTOU race where another
            # thread could see the stale _fd between close() and reset
            tracker._fd = None
            tracker._pid = None
            try:
                os.close(fd)
            except OSError:
                pass

    os.register_at_fork(after_in_child=_reset_tracker_in_child)


def __clear_env(
    apply_monkeypatching: bool = True,
) -> None:
    saved_env = {}

    var_names = [
        "PYTHONPATH",
        "PYTHONHOME",
        # We use this env var to tag the process and it's `multiprocessing`
        # workers.  It's important that we clear it out (so that unrelated sub-
        # processes don't inherit it), but it can be read via
        # `/proc/<pid>/environ`.
        "PAR_INVOKED_NAME_TAG",
    ]

    if sys.platform == "darwin":
        var_names.extend(
            [
                "DYLD_LIBRARY_PATH",
                "DYLD_INSERT_LIBRARIES",
            ]
        )
    else:
        var_names.extend(
            [
                "LD_LIBRARY_PATH",
                "LD_PRELOAD",
            ]
        )

    # Restore the original value of environment variables that we altered
    # as part of the startup process.
    for var in var_names:
        curr_val = os.environ.pop(var, None)
        if curr_val is not None:
            saved_env[var] = curr_val
        val = os.environ.pop("FB_SAVED_" + var, None)
        if val is not None:
            os.environ[var] = val

    if apply_monkeypatching:
        __patch_spawn(var_names, saved_env)
        __patch_spawn_preparation_data()
        __patch_ctypes(saved_env)
        __patch_subprocess_run(saved_env)
        __patch_resource_tracker_fork()


def __startup__() -> None:
    try:
        # pyre-fixme[21]: Could not find module `__par__.__startup_function_loader__`.
        from __par__.__startup_function_loader__ import load_startup_functions

        load_startup_functions()
    except Exception:
        warnings.warn("could not load startup functions", stacklevel=1)


def __passthrough_exec_module() -> None:
    # Delegate this module execution to the next module in the path, if any,
    # effectively making this sitecustomize.py a passthrough module.
    paths = itertools.dropwhile(lambda p: not __file__.startswith(p), sys.path)
    spec = PathFinder.find_spec(
        __name__, path=[p for p in paths if not __file__.startswith(p)]
    )
    if spec:
        mod = module_from_spec(spec)
        sys.modules[__name__] = mod
        # pyre-fixme[16]: Optional type has no attribute `exec_module`.
        spec.loader.exec_module(mod)


__clear_env()
__startup__()
__passthrough_exec_module()
