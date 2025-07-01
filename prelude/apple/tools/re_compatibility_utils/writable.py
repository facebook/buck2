# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

# pyre-strict

import os
import pathlib
import platform
import stat
import sys


def make_path_user_writable(path: str) -> None:
    # On Linux, `os.chmod()` does not support setting the permissions on a symlink.
    # `chmod` manpage says:
    #   > AT_SYMLINK_NOFOLLOW     If pathname is a symbolic link, do not
    #   >     dereference it: instead operate on the link itself.
    #   >     This flag is not currently implemented.
    #
    # In Python, an exception will be thrown:
    # > NotImplementedError: chmod: follow_symlinks unavailable on this platform
    #
    # Darwin supports permission setting on symlinks.
    follow_symlinks = platform.system() != "Darwin"
    st = os.stat(path, follow_symlinks=False)

    try:
        os.chmod(path, st.st_mode | stat.S_IWUSR, follow_symlinks=follow_symlinks)
    except FileNotFoundError as e:
        path_obj = pathlib.Path(path)
        if path_obj.is_symlink():
            resolved_path_obj = path_obj.resolve()
            if not resolved_path_obj.exists():
                # On Linux systems, all symlinks are followed when `chmod`-ing
                # (see comment above about `AT_SYMLINK_NOFOLLOW`). If that happens,
                # we can ignore the `chmod` error as its harmless.
                print(
                    f"Tried setting permission on a symlink to a non-existing path, ignoring error... {e}",
                    file=sys.stderr,
                )
                return
        raise e


def make_dir_recursively_writable(dir: str) -> None:
    for dirpath, _, filenames in os.walk(dir):
        make_path_user_writable(dirpath)
        for filename in filenames:
            make_path_user_writable(os.path.join(dirpath, filename))
