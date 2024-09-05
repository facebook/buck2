import argparse
import shutil
import stat
import json
import sys
import os


# Copy only file contents and exec permission bit.
def _copy(src, dst, *, follow_symlinks=True):
    shutil.copyfile(src, dst, follow_symlinks=False)
    src_mode = os.lstat(src).st_mode
    dst_mode = os.lstat(dst).st_mode
    os.chmod(
        dst,
        dst_mode | (src_mode & (stat.S_IXUSR | stat.S_IXGRP | stat.S_IXOTH)),
        follow_symlinks=False,
    )


def main(argv):
    parser = argparse.ArgumentParser(fromfile_prefix_chars="@")
    parser.add_argument(
        "--manifest", dest="manifests", nargs=2, action="append", default=[]
    )
    parser.add_argument("--path", dest="paths", nargs=2, action="append", default=[])
    parser.add_argument(
        "--symlink", dest="symlinks", nargs=2, action="append", default=[]
    )
    parser.add_argument("output")
    args = parser.parse_args(argv[1:])

    os.makedirs(args.output)

    all_paths = []
    all_paths.extend(args.paths)

    for bdst, manifest in args.manifests:
        with open(manifest) as mf:
            manifest = json.load(mf)
        for dst, src, _ in manifest:
            dst = os.path.join(bdst, dst)
            all_paths.append((dst, src))

    for dst, src in all_paths:
        fdst = os.path.join(args.output, dst)
        os.makedirs(os.path.dirname(fdst), exist_ok=True)
        if os.path.isdir(src):
            shutil.copytree(
                src, fdst, symlinks=True, dirs_exist_ok=True, copy_function=_copy
            )
        else:
            shutil.copy(src, fdst)

    for dst, target in args.symlinks:
        fdst = os.path.join(args.output, dst)
        os.makedirs(os.path.dirname(fdst), exist_ok=True)
        os.symlink(target, fdst)


sys.exit(main(sys.argv))
