# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

"""Stage per-crate rustdoc HTML into a shared directory, then run
`rustdoc --merge=finalize` to write cross-crate index pages over the
top.

RFC 3662 splits rustdoc into a "per-crate" pass (`--merge=none` writing
HTML plus a sidecar parts directory) and a "finalize" pass
(`--merge=finalize` reading the parts dirs). Finalize only writes the
top-level index/search pages; the per-crate HTML must be present
alongside for the merged tree to be browseable.
"""

import argparse
import os
import shutil
import subprocess
import sys
import tempfile
from pathlib import Path

from rustdoc_emit_compat import resolve_emits


def stage_html(out_dir: Path, html_dirs: list[str]) -> None:
    for html_dir in html_dirs:
        src = Path(html_dir)
        if src.is_dir():
            _merge_into(out_dir, src)


def _merge_into(dest_dir: Path, src_dir: Path) -> None:
    # Per-crate html artifacts share top-level dirs (`src/`, `trait.impl/`),
    # so collisions have to be unioned recursively rather than replaced.
    # RFC 3662 was supposed to handle cross-crate html assembly itself;
    # hopefully a future rustdoc makes this manual merge unnecessary.
    for entry in src_dir.iterdir():
        dest = dest_dir / entry.name
        if entry.is_dir() and (dest.exists() or dest.is_symlink()):
            if dest.is_symlink():
                prior = dest.resolve()
                dest.unlink()
                dest.mkdir()
                if prior.is_dir():
                    _merge_into(dest, prior)
            _merge_into(dest, entry)
        else:
            if dest.is_symlink() or dest.exists():
                dest.unlink()
            os.symlink(entry.resolve(), dest)


def main() -> int:
    p = argparse.ArgumentParser(fromfile_prefix_chars="@")
    p.add_argument("--out-dir", required=True)
    p.add_argument("--rustdoc", required=True)
    p.add_argument("--html-dir", action="append", default=[])
    p.add_argument("--parts-dir", action="append", default=[])
    p.add_argument(
        "--rustdoc-flag",
        action="append",
        default=[],
        help="Extra flag forwarded verbatim to rustdoc (repeatable).",
    )
    p.add_argument(
        "--theme",
        action="append",
        default=[],
        help="Path to a real theme CSS file to register with rustdoc at "
        "finalize time (repeatable).",
    )
    args = p.parse_args()

    out = Path(args.out_dir)
    if out.exists():
        shutil.rmtree(out)
    out.mkdir(parents=True)

    stage_html(out, args.html_dir)

    env = dict(os.environ)
    env["RUSTC_BOOTSTRAP"] = "1"

    # Finalize is the one step that should emit the shared CSS/JS for
    # the whole tree (the per-crate parts step skipped them); also
    # emit the non-static files (index.html, search.index, etc.) that
    # finalize itself produces.
    emit_arg = resolve_emits(args.rustdoc, ["html-static-files", "html-non-static-files"])

    rustdoc_args = [
        "-Zunstable-options",
        "--merge=finalize",
        "--enable-index-page",
        f"--out-dir={args.out_dir}",
    ]
    if emit_arg is not None:
        rustdoc_args.append(emit_arg)
    for pd in args.parts_dir:
        rustdoc_args.append(f"--include-parts-dir={pd}")
    for theme in args.theme:
        rustdoc_args.append("--theme")
        rustdoc_args.append(theme)
    rustdoc_args.extend(args.rustdoc_flag)

    with tempfile.NamedTemporaryFile(
        mode="w",
        suffix=".argfile",
        dir=os.environ.get("BUCK_SCRATCH_PATH"),
    ) as f:
        f.write("\n".join(rustdoc_args))
        f.flush()
        return subprocess.run([args.rustdoc, f"@{f.name}"], env=env).returncode


if __name__ == "__main__":
    sys.exit(main())
