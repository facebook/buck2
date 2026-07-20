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
import os
import stat
import sys
import threading
import warnings
from collections.abc import Callable, MutableMapping
from importlib.machinery import PathFinder
from importlib.util import module_from_spec

lock = threading.Lock()


def __patch_ctypes(saved_env: dict[str, str]) -> None:
    from ctypes import util as ctypes_util

    orig_find_library: Callable[[str], str | None] = ctypes_util.find_library

    def _patched_find_library(name: str) -> str | None:
        if sys.platform == "darwin":
            from ctypes.macholib.dyld import dyld_find

            env = {**os.environ, **saved_env}
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


_PATH_PROPAGATING_FINDER_SENTINEL: str = "_fb_par_path_propagating_finder_installed"


def __install_path_propagating_finder() -> None:
    """Install a meta-path finder that grafts the unpack tree onto package __path__.

    Standalone PARs extract .so files (but not .py / .pyc / .pyo) into the
    unpack dir at FB_PAR_RUNTIME_FILES / FB_PAR_UNZIP_LOCATION. For ext-bearing
    packages, __init__.py lives only in the PAR zip while the .so lives only
    in the unpack dir; zipimport cannot load .so from a zip. This finder
    appends the matching unpack subdirectory to a parent package's __path__ on
    first submodule import, so Python's standard PathFinder can resolve the
    on-disk .so.

    Runs in the parent (via __run_par_main__.py: `import sitecustomize`) AND
    in spawn / forkserver / subprocess.run children (via __patch_spawn /
    __patch_subprocess_run, which export PYTHONPATH so the child finds this
    sitecustomize during Py_Initialize). Idempotent.
    """
    if any(getattr(f, _PATH_PROPAGATING_FINDER_SENTINEL, False) for f in sys.meta_path):
        return

    expanded_par_tree = os.environ.get("FB_PAR_RUNTIME_FILES") or os.environ.get(
        "FB_PAR_UNZIP_LOCATION"
    )
    if not expanded_par_tree or not os.path.isdir(expanded_par_tree):
        return

    _stat = os.stat
    _S_ISDIR = stat.S_ISDIR
    _join = os.path.join
    _sep = os.sep

    class PathPropagatingFinder:
        def __init__(self, expanded_par_tree: str) -> None:
            self.expanded_par_tree = expanded_par_tree
            self._propagated: set[str] = set()

        def find_spec(self, fullname, path=None, target=None):
            if "." not in fullname:
                return None
            parent_name = fullname.rsplit(".", 1)[0]
            parent = sys.modules.get(parent_name)
            if parent is None or not hasattr(parent, "__path__"):
                return None
            if parent_name in self._propagated:
                return None
            extracted_dir = _join(
                self.expanded_par_tree, parent_name.replace(".", _sep)
            )
            try:
                st = _stat(extracted_dir)
            except (OSError, ValueError, TypeError):
                return None
            if _S_ISDIR(st.st_mode):
                parent.__path__.append(extracted_dir)
                self._propagated.add(parent_name)
            return None

    finder = PathPropagatingFinder(expanded_par_tree)
    setattr(finder, _PATH_PROPAGATING_FINDER_SENTINEL, True)
    sys.meta_path.insert(0, finder)


_SITECUSTOMIZE_SUBDIR: str = "__par_sitecustomize_proxy__"


def _extract_sitecustomize() -> str | None:
    """Extract sitecustomize.py from the PAR zip to a subdir of the unpack dir.

    Returns the directory containing the extracted file, or None if not needed.
    Uses a dedicated subdirectory to prevent unrelated Python processes from
    picking it up when the unpack dir appears on PYTHONPATH.
    """
    import zipimport

    # If sitecustomize wasn't loaded from a zip the child process
    # can find it the same way the parent did.
    if not isinstance(__loader__, zipimport.zipimporter):
        return None

    unpack_dir = os.environ.get("FB_PAR_RUNTIME_FILES") or os.environ.get(
        "FB_PAR_UNZIP_LOCATION"
    )
    par_filename = os.environ.get("FB_PAR_FILENAME") or __loader__.archive
    if not unpack_dir or not par_filename:
        return None

    sitecustomize = os.path.basename(__file__)
    extract_dir = os.path.join(unpack_dir, _SITECUSTOMIZE_SUBDIR)
    extract_path = os.path.join(extract_dir, sitecustomize)
    if os.path.exists(extract_path):
        return extract_dir  # Already extracted

    try:
        import zipfile

        os.makedirs(extract_dir, exist_ok=True)
        with zipfile.ZipFile(par_filename) as zf:
            with zf.open(sitecustomize) as src, open(extract_path, "wb") as dst:
                dst.write(src.read())
        return extract_dir
    except (OSError, KeyError, zipfile.BadZipFile):
        return None  # Best effort


def _runtime_lib_path_var() -> str:
    return "DYLD_LIBRARY_PATH" if sys.platform == "darwin" else "LD_LIBRARY_PATH"


def _par_runtime_lib_dir() -> str | None:
    """Absolute path to the PAR's runtime/lib dir, or None if unavailable.

    Under NativeLinkStrategy("native"), sys.executable is the native-main ELF,
    whose $ORIGIN-relative RPATH only resolves to runtime/lib when the ELF is
    materialized adjacent to the link-tree. When it is materialized elsewhere
    (e.g. the content-addressed anon-target outputs produced under
    python.use_anon_target_for_analysis), $ORIGIN realpath-resolves into an
    unrelated tree and the bundled .so's cannot be found. FB_PAR_RUNTIME_FILES
    points at the link-tree root at runtime, so runtime/lib is reconstructable
    here regardless of where the ELF itself lives.
    """
    runtime_files = os.environ.get("FB_PAR_RUNTIME_FILES")
    if not runtime_files:
        return None
    lib_dir = os.path.join(runtime_files, "runtime", "lib")
    return lib_dir if os.path.isdir(lib_dir) else None


def _inject_runtime_lib_path(env: MutableMapping[str, str], lib_path_var: str) -> None:
    """Prepend the PAR's runtime/lib to `lib_path_var` in `env`, if resolvable.

    A bare self-re-exec of sys.executable cannot rely on the parent's scrubbed
    lib-path env, so reconstruct it from FB_PAR_RUNTIME_FILES. Any existing
    value is preserved by appending after ours.
    """
    lib_dir = _par_runtime_lib_dir()
    if lib_dir is None:
        return
    existing = env.get(lib_path_var)
    if not existing:
        env[lib_path_var] = lib_dir
        return
    # Dedup against canonicalized entries, so the same directory reached via a
    # symlink, redundant separators, or a relative spelling is not prepended a
    # second time and does not accumulate across chained re-execs.
    lib_real = os.path.realpath(lib_dir)
    for entry in existing.split(os.pathsep):
        if entry == lib_dir or os.path.realpath(entry) == lib_real:
            return
    env[lib_path_var] = lib_dir + os.pathsep + existing


def _is_self_reexec(executable: object) -> bool:
    """True when `executable` is a re-exec of our own interpreter.

    We only ever want to inject runtime/lib for a self-re-exec of
    sys.executable (see _par_runtime_lib_dir), never for arbitrary child
    programs, so this is deliberately narrow: an exact match against
    sys.executable, with an os.path.realpath() fallback so resolved-path forms
    also match. Bounding it to the interpreter keeps runtime/lib from ever
    leaking into unrelated subprocesses.
    """
    if not isinstance(executable, (str, bytes, os.PathLike)):
        return False
    exe = os.fsdecode(executable)
    if exe == sys.executable:
        return True
    try:
        return os.path.realpath(exe) == os.path.realpath(sys.executable)
    except (OSError, ValueError):
        return False


def _shell_command_reexecs_self(args: object) -> bool:
    """True when a `shell=True` command string re-execs our own interpreter.

    Popen(cmd, shell=True) / asyncio.create_subprocess_shell exec /bin/sh, not
    sys.executable, so _is_self_reexec only ever sees the shell (or the whole
    command string) and never injects runtime/lib. But the command string
    itself can re-exec sys.executable (e.g. `sh -c "<native-main> -c ..."`), and
    that grandchild ELF needs runtime/lib exactly as a direct self-re-exec
    would. Detect it by looking for sys.executable as a command token in the
    string. Kept narrow -- gated on the command actually invoking sys.executable
    -- so runtime/lib is not injected into shells that only run unrelated
    programs (buck / hg / git).
    """
    # With shell=True and a sequence, args[0] is the command string; any
    # remaining items are extra args to the shell itself.
    command = args[0] if isinstance(args, (list, tuple)) and args else args
    if isinstance(command, bytes):
        try:
            command = os.fsdecode(command)
        except ValueError:
            return False
    if not isinstance(command, str) or not sys.executable:
        return False
    # Match sys.executable as a whole shell token, not a raw substring: a
    # substring search false-positives when the path appears inside another
    # token or in quoted data, over-injecting runtime/lib into unrelated shells.
    # shlex.split word-splits the way /bin/sh does.
    import shlex

    try:
        tokens = shlex.split(command)
    except ValueError:
        # Unbalanced quotes etc.: can't tokenize safely, so don't inject rather
        # than risk a substring false-positive on unrelated shells. A real
        # self-re-exec command (built via shlex.quote) always parses.
        return False
    if sys.executable in tokens:
        return True
    try:
        resolved = os.path.realpath(sys.executable)
    except (OSError, ValueError):
        return False
    return bool(resolved) and resolved in tokens


def _is_shell_invocation(pargs: tuple[object, ...], kwargs: dict[str, object]) -> bool:
    """Whether Popen was invoked with shell=True (keyword or positional).

    `pargs` is what the `_patched_init(self, args, *pargs)` wrapper captures
    AFTER `self` and `args`, so it starts at `bufsize`. Popen's positional order
    after `args` is: bufsize, executable, stdin, stdout, stderr, preexec_fn,
    close_fds, shell, ... -- so `shell` is `pargs[7]` (bufsize is pargs[0]).
    asyncio.create_subprocess_shell passes shell by keyword.
    """
    if "shell" in kwargs:
        return bool(kwargs["shell"])
    _SHELL_POS = 7  # index into pargs (which excludes self+args); see docstring
    return len(pargs) > _SHELL_POS and bool(pargs[_SHELL_POS])


def _env_for_self_reexec(
    executable: object,
    env: dict[str, str] | None,
    args: object = None,
    is_shell: bool = False,
) -> dict[str, str] | None:
    """Return an env that lets a self-re-exec child find the PAR's runtime/lib.

    When `executable` is a self-re-exec of sys.executable -- or `is_shell` and
    the `args` command string re-execs sys.executable (see
    _shell_command_reexecs_self) -- return an env mapping (copying os.environ
    when the caller passed None) with the lib path injected; otherwise return
    `env` unchanged so inherit-env semantics are preserved for non-self-re-exec
    children. This is purely additive to the lib path and never touches
    PYTHONPATH/PYTHONHOME.
    """
    if not (
        _is_self_reexec(executable) or (is_shell and _shell_command_reexecs_self(args))
    ):
        return env
    if env is None:
        env = os.environ.copy()
    _inject_runtime_lib_path(env, _runtime_lib_path_var())
    return env


def __patch_spawn(var_names: list[str], saved_env: dict[str, str]) -> None:
    # Compute resolved PYTHONPATH once at patch time (not per-spawn).
    # dirs_only=True filters out zip files to avoid RecursionError from
    # zipimport's lazy `import struct` cycle.
    resolved_pythonpath = os.path.pathsep.join(
        _resolve_path_entries(sys.path, dirs_only=True)
    )

    # pyre-fixme[53]: Captured variable is not annotated.
    def _setup_child_env() -> None:
        proxy_dir = _extract_sitecustomize()
        for var in var_names:
            val = os.environ.get(var, None)
            if val is not None:
                os.environ["FB_SAVED_" + var] = val
            saved_val = saved_env.get(var, None)
            if saved_val is not None:
                os.environ[var] = saved_val

        # Ensure PYTHONPATH includes resolved sys.path dirs so the
        # child interpreter finds sitecustomize.py during Py_Initialize.
        # The extraction dir (if any) is prepended so the child finds
        # the extracted sitecustomize.py first.
        parts = []
        if proxy_dir:
            parts.append(proxy_dir)
        existing = os.environ.get("PYTHONPATH", "")
        if existing:
            parts.append(existing)
        parts.append(resolved_pythonpath)
        os.environ["PYTHONPATH"] = os.path.pathsep.join(parts)

        # Reconstruct the lib path for native-strategy self-re-exec: the
        # saved_env restore above is empty when the C++ RestoreEnv scrubbed the
        # bootstrap LD_LIBRARY_PATH before Python captured it. __clear_env in the
        # finally clause pops this back out after the child is spawned.
        _inject_runtime_lib_path(os.environ, _runtime_lib_path_var())

    if sys.platform == "win32":
        import multiprocessing.popen_spawn_win32 as popen_win32

        orig_init = popen_win32.Popen.__init__

        def _patched_init(self, process_obj) -> None:
            with lock:
                try:
                    _setup_child_env()
                    orig_init(self, process_obj)
                finally:
                    __clear_env(apply_monkeypatching=False)

        popen_win32.Popen.__init__ = _patched_init
    else:
        import multiprocessing.util as mp_util

        std_spawn = mp_util.spawnv_passfds

        # pyre-fixme[53]: Captured variable is not annotated.
        # pyre-fixme[2]: Parameter must be annotated.
        def spawnv_passfds(path, args, passfds) -> None | int:
            with lock:
                try:
                    _setup_child_env()
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
            # Extract sitecustomize.py from the PAR zip to a namespaced
            # subdir so the child interpreter can find it via PYTHONPATH.
            proxy_dir = _extract_sitecustomize()
            parts = []
            if proxy_dir:
                parts.append(proxy_dir)
            parts.extend(resolved)
            env["PYTHONPATH"] = os.path.pathsep.join(parts)
            # Only set PYTHONHOME if the child will find sitecustomize.py
            # (which clears it). Otherwise PYTHONHOME leaks and the child
            # uses the PAR's prefix for stdlib, causing hangs.
            if proxy_dir is not None or any(
                os.path.isfile(os.path.join(d, "sitecustomize.py")) for d in resolved
            ):
                env["PYTHONHOME"] = sys.prefix
            # Restore library path env vars so the child process can find
            # bundled native libraries (e.g. libpython, libX11).
            for var in _lib_path_vars:
                if var not in env and var in saved_env:
                    env[var] = saved_env[var]
            # saved_env is empty when the bootstrap lib path was scrubbed before
            # Python captured it, so also reconstruct runtime/lib directly. This
            # is what lets a native-strategy sys.executable child load its
            # bundled .so's when $ORIGIN doesn't resolve to the link-tree.
            _inject_runtime_lib_path(env, _runtime_lib_path_var())

        return std_run(args, env=env, **kwargs)

    subprocess.run = _patched_run


def __patch_subprocess_popen(saved_env: dict[str, str]) -> None:
    import subprocess

    orig_init = subprocess.Popen.__init__

    # pyre-fixme[2]: Parameter must be annotated.
    # pyre-fixme[53]: Captured variable `orig_init` is not annotated.
    def _patched_init(self, args, *pargs, **kwargs) -> None:
        # run / check_call / check_output and direct Popen(...) all funnel
        # through Popen.__init__, so injecting the lib path here (rather than
        # only in subprocess.run) covers every self-re-exec form -- including
        # callers that pass an explicit env= already carrying PYTHONPATH, which
        # the narrower subprocess.run gate deliberately skips. It also covers
        # shell=True (asyncio.create_subprocess_shell / os.system-style)
        # invocations whose command string re-execs sys.executable: those exec
        # /bin/sh, so the direct self-re-exec check below misses them, but the
        # native-main grandchild still needs runtime/lib. Only the lib path is
        # touched, and only for a self-re-exec; PYTHONPATH/PYTHONHOME stay in
        # __patch_subprocess_run and are idempotent with this
        # (_inject_runtime_lib_path is membership-guarded).
        executable = kwargs.get("executable")
        candidate = (
            executable
            if executable is not None
            else (args[0] if isinstance(args, (list, tuple)) and args else args)
        )
        env = _env_for_self_reexec(
            candidate,
            kwargs.get("env"),
            args=args,
            is_shell=_is_shell_invocation(pargs, kwargs),
        )
        if env is not None:
            kwargs["env"] = env
        orig_init(self, args, *pargs, **kwargs)

    subprocess.Popen.__init__ = _patched_init


def _wrap_os_functions(
    names: tuple[str, ...],
    # pyre-fixme[2]: Parameter must be annotated.
    make_wrapper,
) -> None:
    """Replace each existing os.<name> with make_wrapper(original), if present."""
    for name in names:
        orig = getattr(os, name, None)
        if orig is not None:
            setattr(os, name, make_wrapper(orig))


def __patch_os_exec_inherit_env(saved_env: dict[str, str]) -> None:
    """execv/execvp/execl/execlp: child inherits the current environment, so
    publish the lib path via os.environ. See __patch_os_exec_argv_env for the
    shared invariant.
    """
    lib_path_var: str = _runtime_lib_path_var()

    def _wrap(orig: Callable[..., None]) -> Callable[..., None]:
        # pyre-fixme[2]: Parameter must be annotated.
        # pyre-fixme[3]: Return type must be annotated.
        def _patched(path, *args):
            if _is_self_reexec(path):
                _inject_runtime_lib_path(os.environ, lib_path_var)
            return orig(path, *args)

        return _patched

    _wrap_os_functions(("execv", "execvp", "execl", "execlp"), _wrap)


def __patch_os_exec_argv_env(saved_env: dict[str, str]) -> None:
    """execve/execvpe(path, argv, env): env is the third positional mapping.

    subprocess covers run / check_call / check_output / Popen, but a target can
    also re-exec sys.executable directly. The invariant is the same as
    _par_runtime_lib_dir: a native-strategy sys.executable child needs the
    bundled lib path that the C++ RestoreEnv scrubbed from the parent.
    """
    lib_path_var: str = _runtime_lib_path_var()

    def _wrap(orig: Callable[..., None]) -> Callable[..., None]:
        # pyre-fixme[2]: Parameter must be annotated.
        # pyre-fixme[3]: Return type must be annotated.
        def _patched(path, argv, env, *args):
            # env may be None (inherit the current environment); leave it
            # untouched in that case so we don't change inherit-env semantics.
            if env is not None and _is_self_reexec(path):
                env = dict(env)
                _inject_runtime_lib_path(env, lib_path_var)
            return orig(path, argv, env, *args)

        return _patched

    _wrap_os_functions(("execve", "execvpe"), _wrap)


def __patch_os_exec_trailing_env(saved_env: dict[str, str]) -> None:
    """execle/execlpe(path, *args, env): env is the trailing positional mapping."""
    lib_path_var: str = _runtime_lib_path_var()

    def _wrap(orig: Callable[..., None]) -> Callable[..., None]:
        # pyre-fixme[2]: Parameter must be annotated.
        # pyre-fixme[3]: Return type must be annotated.
        def _patched(path, *args):
            # The trailing env may be None (inherit the current environment);
            # leave it untouched so we don't change inherit-env semantics.
            if args and args[-1] is not None and _is_self_reexec(path):
                env = dict(args[-1])
                _inject_runtime_lib_path(env, lib_path_var)
                args = args[:-1] + (env,)
            return orig(path, *args)

        return _patched

    _wrap_os_functions(("execle", "execlpe"), _wrap)


def __patch_os_posix_spawn(saved_env: dict[str, str]) -> None:
    """posix_spawn/posix_spawnp(path, argv, env, ...): env is a required mapping."""
    lib_path_var: str = _runtime_lib_path_var()

    def _wrap(orig: Callable[..., int]) -> Callable[..., int]:
        # pyre-fixme[2]: Parameter must be annotated.
        # pyre-fixme[3]: Return type must be annotated.
        def _patched(path, argv, env, *args, **kwargs):
            if _is_self_reexec(path):
                env = dict(env)
                _inject_runtime_lib_path(env, lib_path_var)
            return orig(path, argv, env, *args, **kwargs)

        return _patched

    _wrap_os_functions(("posix_spawn", "posix_spawnp"), _wrap)


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


def __add_win_dll_directories() -> None:
    """
    Windows requires explicit os.add_dll_directory() calls for Python
    extension modules to find their DLL dependencies.
    """
    if sys.platform != "win32":
        return
    dll_dirs = os.environ.get("FB_PAR_WIN_DLL_DIRS", "")
    if not dll_dirs:
        return
    for d in dll_dirs.split(os.pathsep):
        if d and os.path.isdir(d):
            try:
                os.add_dll_directory(d)
            except OSError:
                pass


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
        # PYTHON_GIL=0 is set by the PAR bootstrap for free-threading builds
        # to force-disable the GIL.  The interpreter reads it once during
        # Py_Initialize and stores the result in config->enable_gil, so the
        # env var is not needed after startup.  Clearing it prevents leakage
        # to child processes that may use non-free-threading Python builds,
        # which would crash with "Disabling the GIL is not supported".
        "PYTHON_GIL",
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
        __add_win_dll_directories()
        __patch_spawn(var_names, saved_env)
        __patch_spawn_preparation_data()
        __patch_ctypes(saved_env)
        __patch_subprocess_run(saved_env)
        __patch_subprocess_popen(saved_env)
        __patch_os_exec_inherit_env(saved_env)
        __patch_os_exec_argv_env(saved_env)
        __patch_os_exec_trailing_env(saved_env)
        __patch_os_posix_spawn(saved_env)
        __patch_resource_tracker_fork()


def __startup__() -> None:
    try:
        # pyre-fixme[21]: Could not find module `__par__.__startup_function_loader__`.
        from __par__.__startup_function_loader__ import load_startup_functions
    except ImportError:
        par = os.environ.get("FB_PAR_FILENAME", "")
        if par and os.path.isfile(par) and par not in sys.path:
            sys.path.insert(0, par)
            # pex requires bootstrap dir to be in the path
            bootstrap = os.environ.get("FB_PAR_BOOTSTRAP_DIR", "")
            if bootstrap and bootstrap not in sys.path:
                sys.path.append(bootstrap)
            try:
                from __par__.__startup_function_loader__ import load_startup_functions
            except ImportError:
                warnings.warn("could not load startup functions", stacklevel=1)
                return
        else:
            warnings.warn("could not load startup functions", stacklevel=1)
            return
    except Exception:
        warnings.warn("could not load startup functions", stacklevel=1)
        return

    try:
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


__install_path_propagating_finder()
__clear_env()
__startup__()
__passthrough_exec_module()
