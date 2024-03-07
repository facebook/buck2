#!/usr/bin/env python3
# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

# pyre-strict

from __future__ import annotations

import multiprocessing.util as mp_util
import os
import sys
import threading
import warnings
from importlib.machinery import PathFinder
from importlib.util import module_from_spec

lock = threading.Lock()


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


def __clear_env(patch_spawn: bool = True) -> None:
    saved_env = {}

    var_names = [
        "PYTHONPATH",
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
    spec = PathFinder.find_spec(
        __name__, path=[p for p in sys.path if not __file__.startswith(p)]
    )
    if spec:
        mod = module_from_spec(spec)
        sys.modules[__name__] = mod
        # pyre-fixme[16]: Optional type has no attribute `exec_module`.
        spec.loader.exec_module(mod)


__clear_env()
__startup__()
__passthrough_exec_module()
