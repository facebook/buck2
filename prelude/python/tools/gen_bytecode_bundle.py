# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

# pyre-strict

"""Pack the `.pyc` files produced by `compile.py` into one blob.

Each `python_library` contributes a bytecode manifest and its matching
`py_compile` output directory.

Format (little endian):

    header     <8sII: magic, pyc_magic length, index length
    pyc_magic  importlib.util.MAGIC_NUMBER
    index      marshalled `{tree relative source path: (payload offset, size)}`
    payload    concatenated `.pyc` bodies with the 16-byte header stripped

The index is keyed by path, not by module name, because
`__par__.bytecode_bundle` hangs a `FileFinder` off `sys.path_hooks`: the tree
decides which file answers an import and the blob only supplies that file's
bytecode. Keys are `/` separated on every platform.
"""

from __future__ import annotations

import argparse
import json
import marshal
import os
import shutil
import struct
import sys
from importlib.util import MAGIC_NUMBER, source_from_cache

MAGIC = b"PYCBNDL4"
HEADER: struct.Struct = struct.Struct("<8sII")
PYC_HEADER_LEN = 16
_UINT32_MAX: int = (1 << 32) - 1


def source_key(cache_relpath: str) -> str:
    """`pkg/__pycache__/mod.cpython-312.pyc` -> `pkg/mod.py`."""
    return source_from_cache(cache_relpath).replace(os.sep, "/")


def collect(
    manifest_paths: list[str], bytecode_dirs: list[str]
) -> list[tuple[str, str]]:
    """Return `(source path, .pyc path)` entries.

    Each manifest is paired with its `bytecode_dir`. The real `.pyc` path is
    `bytecode_dir/dest`; the manifest's placeholder path is ignored.
    """
    if len(manifest_paths) != len(bytecode_dirs):
        raise ValueError(
            "Got {} bytecode manifests but {} bytecode directories".format(
                len(manifest_paths), len(bytecode_dirs)
            )
        )

    entries = []
    for manifest_path, bytecode_dir in zip(manifest_paths, bytecode_dirs):
        with open(manifest_path) as f:
            manifest = json.load(f)
        for dest, _pyc, _source in manifest:
            dest = os.path.normpath(dest)
            entries.append((source_key(dest), os.path.join(bytecode_dir, dest)))

    entries.sort(key=lambda entry: entry[0])
    return entries


def build(entries: list[tuple[str, str]], out_path: str) -> None:
    index: dict[str, tuple[int, int]] = {}
    selected: list[tuple[str, int]] = []
    payload_end = 0
    for source_relpath, pyc_path in entries:
        if source_relpath in index:
            # Two libraries own the same path in the tree, which make_par
            # tolerates for bytecode. Keep the first: `collect` fixed the order.
            continue
        with open(pyc_path, "rb") as f:
            header = f.read(PYC_HEADER_LEN)
            body_size = os.fstat(f.fileno()).st_size - PYC_HEADER_LEN
        if len(header) != PYC_HEADER_LEN or body_size <= 0:
            raise ValueError(f"Truncated .pyc has no bytecode payload: {pyc_path}")
        if header[: len(MAGIC_NUMBER)] != MAGIC_NUMBER:
            raise ValueError(f".pyc has the wrong interpreter magic: {pyc_path}")
        index[source_relpath] = (payload_end, body_size)
        payload_end += body_size
        selected.append((pyc_path, body_size))

    # `collect` sorted the entries and dicts keep insertion order, so the
    # marshalled index is byte-identical across builds.
    index_bytes = marshal.dumps(index)
    if len(index_bytes) > _UINT32_MAX:
        raise ValueError("Bytecode bundle index is too large")
    with open(out_path, "wb") as f:
        f.write(HEADER.pack(MAGIC, len(MAGIC_NUMBER), len(index_bytes)))
        f.write(MAGIC_NUMBER)
        f.write(index_bytes)
        for pyc_path, expected_size in selected:
            start = f.tell()
            with open(pyc_path, "rb") as pyc:
                pyc.seek(PYC_HEADER_LEN)
                shutil.copyfileobj(pyc, f, length=1024 * 1024)
            if f.tell() - start != expected_size:
                raise OSError(f".pyc changed while bundling: {pyc_path}")


def _read_lines(path: str) -> list[str]:
    with open(path) as f:
        return [line.rstrip("\n") for line in f if line.rstrip("\n")]


def main(argv: list[str]) -> None:
    parser = argparse.ArgumentParser()
    parser.add_argument("--output", required=True)
    parser.add_argument(
        "--bytecode-manifests-from",
        required=True,
        help="file listing py_compile bytecode manifests, one per line",
    )
    parser.add_argument(
        "--bytecode-artifacts-from",
        required=True,
        help="file listing py_compile output directories, one per line, in the "
        "same order as --bytecode-manifests-from",
    )
    args = parser.parse_args(argv[1:])

    entries = collect(
        _read_lines(args.bytecode_manifests_from),
        _read_lines(args.bytecode_artifacts_from),
    )
    build(entries, args.output)


if __name__ == "__main__":
    main(sys.argv)
