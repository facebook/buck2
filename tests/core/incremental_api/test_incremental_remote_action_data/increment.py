# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

import argparse
import os
import pathlib
import platform
import shutil
import stat
import sys


def parse_args():
    parser = argparse.ArgumentParser()
    parser.add_argument(
        "--dir",
        action="store_true",
    )
    parser.add_argument(
        "--out",
        type=str,
        required=True,
    )
    return parser.parse_args()


def make_path_user_writable(path: str) -> None:
    if not os.path.exists(path):
        return
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
    backup_path = f"{path}.bak"
    shutil.move(path, backup_path)
    shutil.copy2(backup_path, path)
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


def main() -> None:
    args = parse_args()
    if args.dir:
        file_path = os.path.join(args.out, "MYFILE")
    else:
        file_path = args.out

    if os.path.exists(file_path):
        with open(file_path, "r") as file:
            content = int(file.read()) + 1
    else:
        content = 0

    if args.dir:
        try:
            os.mkdir(args.out)
        except FileExistsError:
            pass

    make_path_user_writable(file_path)

    with open(file_path, "w") as f:
        f.write(f"{content}")


if __name__ == "__main__":
    main()
