import argparse
import shutil
import json
import sys
import os


def main(argv):
    parser = argparse.ArgumentParser(fromfile_prefix_chars="@")
    parser.add_argument(
        "--manifest", dest="manifests", nargs=2, action="append", default=[]
    )
    parser.add_argument("--path", dest="paths", nargs=2, action="append", default=[])
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
            shutil.copytree(src, fdst, symlinks=True, dirs_exist_ok=True)
        else:
            shutil.copy(src, fdst)


sys.exit(main(sys.argv))
