# Copyright (c) Facebook, Inc. and its affiliates.
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

"""
Example usage:
$ cat inputs.manifest
[["foo.py", "input/foo.py", "//my_rule:foo"]]
$ compile.py --output=out-dir --ignore-errors inputs.manifest
$ find out-dir -type f
out-dir/foo.pyc
"""

# pyre-unsafe

import argparse
import errno
import json
import os
import py_compile
import sys


if sys.version_info[0] == 3:
    import importlib

    DEFAULT_FORMAT = importlib.util.cache_from_source("{pkg}/{name}.py")
else:
    DEFAULT_FORMAT = "{pkg}/{name}.pyc"


def get_py_path(module):
    return module.replace(".", os.sep) + ".py"


def get_pyc_path(module, fmt):
    try:
        package, name = module.rsplit(".", 1)
    except ValueError:
        package, name = "", module
    parts = fmt.split(os.sep)
    for idx in range(len(parts)):
        if parts[idx] == "{pkg}":
            parts[idx] = package.replace(".", os.sep)
        elif parts[idx].startswith("{name}"):
            parts[idx] = parts[idx].format(name=name)
    return os.path.join(*parts)


def _mkdirs(dirpath):
    try:
        os.makedirs(dirpath)
    except OSError as e:
        if e.errno != errno.EEXIST:
            raise


def main(argv):
    parser = argparse.ArgumentParser(fromfile_prefix_chars="@")
    parser.add_argument("-o", "--output", required=True)
    parser.add_argument("-f", "--format", default=DEFAULT_FORMAT)
    parser.add_argument("-i", "--ignore-errors", action="store_true")
    parser.add_argument("manifests", nargs="*")
    args = parser.parse_args(argv[1:])

    _mkdirs(args.output)

    for manifest_path in args.manifests:
        with open(manifest_path) as mf:
            manifest = json.load(mf)
        for dst, src, _ in manifest:
            # We only care about python sources.
            base, ext = os.path.splitext(dst)
            if ext != ".py":
                continue
            module = base.replace(os.sep, ".")
            pyc = os.path.join(args.output, get_pyc_path(module, args.format))
            _mkdirs(os.path.dirname(pyc))
            py_compile.compile(
                src,
                cfile=pyc,
                dfile=get_py_path(module),
                doraise=not args.ignore_errors,
                invalidation_mode=py_compile.PycInvalidationMode.UNCHECKED_HASH,
            )


sys.exit(main(sys.argv))
