import argparse
import shutil
import stat
import json
import sys
import os


# Copy only file contents and exec permission bit.
def _copy(src, dst, follow_symlinks=True):
    if os.path.islink(src) and not follow_symlinks:
        target = os.readlink(src)
        os.symlink(target, dst)
    else:
        shutil.copyfile(src, dst)
        src_mode = os.lstat(src).st_mode
        dst_mode = os.lstat(dst).st_mode
        os.chmod(
            dst,
            dst_mode | (src_mode & (stat.S_IXUSR | stat.S_IXGRP | stat.S_IXOTH)),
        )


def _copy_tree(src, dst, *, dirs_exist_ok=False):
    os.makedirs(dst, exist_ok=dirs_exist_ok)

    for root, dirs, files in os.walk(src):
        rel_root = os.path.relpath(root, src)

        for f in files:
            src_path = os.path.join(root, f)
            dst_path = os.path.join(dst, rel_root, f)
            _copy(src_path, dst_path, follow_symlinks=False)

        for d in dirs:
            dst_path = os.path.join(dst, rel_root, d)
            os.makedirs(dst_path, exist_ok=dirs_exist_ok)


def main(argv):
    parser = argparse.ArgumentParser(fromfile_prefix_chars="@")
    parser.add_argument(
        "--manifest", dest="manifests", nargs=2, action="append", default=[]
    )
    parser.add_argument("--path", dest="paths", nargs=2, action="append", default=[])
    parser.add_argument("--file-follow", dest="files_follow", nargs=2, action="append", default=[])
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
            _copy_tree(src, fdst, dirs_exist_ok=True)
        else:
            _copy(src, fdst, follow_symlinks=False)

    for dst, src in args.files_follow:
        fdst = os.path.join(args.output, dst)
        os.makedirs(os.path.dirname(fdst), exist_ok=True)
        _copy(src, fdst, follow_symlinks=True)

    for dst, target in args.symlinks:
        fdst = os.path.join(args.output, dst)
        os.makedirs(os.path.dirname(fdst), exist_ok=True)
        os.symlink(target, fdst)


sys.exit(main(sys.argv))
