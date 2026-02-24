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


def __patch_spawn(var_names: list[str], saved_env: dict[str, str]) -> None:
    std_spawn = mp_util.spawnv_passfds

    # pyre-fixme[53]: Captured variable `std_spawn` is not annotated.
    # pyre-fixme[53]: Captured variable `saved_env` is not annotated.
    # pyre-fixme[53]: Captured variable `var_names` is not annotated.
    # pyre-fixme[2]: Parameter must be annotated.
    def spawnv_passfds(path, args, passfds) -> None | int:
        with lock:
            try:
                for var in var_names:
                    val = os.environ.get(var, None)
                    if val is not None:
                        os.environ["FB_SAVED_" + var] = val
                    saved_val = saved_env.get(var, None)
                    if saved_val is not None:
                        os.environ[var] = saved_val
                return std_spawn(path, args, passfds)
            finally:
                __clear_env(False)

    mp_util.spawnv_passfds = spawnv_passfds


def __patch_spawn_preparation_data() -> None:
    # Only needed for fastzip PARs, which use /proc/self/fd/<N> paths in
    # sys.path. Other PAR styles (e.g. xar) use real filesystem paths and
    # don't need this patch.
    #
    # We must also avoid importing multiprocessing.spawn during sitecustomize
    # for xar PARs: importing it captures sys.executable in a module-level
    # _python_exe variable (via set_executable(sys.executable)), but xar PARs
    # fix up sys.executable later in __run_xar_main__.py. Importing too early
    # would cause multiprocessing to spawn subprocesses (e.g. the resource
    # tracker) with the wrong executable, crashing them.
    if not any(entry.startswith("/proc/self/fd/") for entry in sys.path):
        return

    import multiprocessing.spawn as mp_spawn

    orig_get_preparation_data = mp_spawn.get_preparation_data

    def get_preparation_data(name):  # type: ignore
        d = orig_get_preparation_data(name)
        if "sys_path" in d:
            resolved = []
            for entry in d["sys_path"]:
                if entry.startswith("/proc/self/fd/"):
                    try:
                        resolved.append(os.readlink(entry))
                    except OSError:
                        resolved.append(entry)
                else:
                    resolved.append(entry)
            d["sys_path"] = resolved
        return d

    mp_spawn.get_preparation_data = get_preparation_data


def __patch_subprocess_run() -> None:
    import subprocess
    from functools import wraps

    std_run = subprocess.run

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
            resolved_path = []
            for entry in sys.path:
                if entry.startswith("/proc/self/fd/"):
                    try:
                        resolved_path.append(os.readlink(entry))
                    except OSError:
                        resolved_path.append(entry)
                else:
                    resolved_path.append(entry)
            env["PYTHONPATH"] = os.path.pathsep.join(resolved_path)
            env["PYTHONHOME"] = sys.prefix

        return std_run(args, env=env, **kwargs)

    subprocess.run = _patched_run


def __clear_env(
    patch_spawn: bool = True,
    patch_ctypes: bool = True,
    patch_subprocess_run: bool = True,
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

    if patch_spawn:
        __patch_spawn(var_names, saved_env)
        __patch_spawn_preparation_data()

    if patch_ctypes:
        __patch_ctypes(saved_env)

    if patch_subprocess_run:
        __patch_subprocess_run()


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
