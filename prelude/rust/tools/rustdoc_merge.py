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
from pathlib import Path

from rustdoc_emit_compat import resolve_emits


def stage_html(out_dir: Path, html_dirs: list[str]) -> None:
    for html_dir in html_dirs:
        src = Path(html_dir)
        if not src.is_dir():
            continue
        for entry in src.iterdir():
            dest = out_dir / entry.name
            if dest.exists() or dest.is_symlink():
                dest.unlink()
            os.symlink(entry.resolve(), dest)


def main() -> int:
    p = argparse.ArgumentParser()
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

    cmd = [
        args.rustdoc,
        "-Zunstable-options",
        "--merge=finalize",
        "--enable-index-page",
        f"--out-dir={args.out_dir}",
    ]
    # Finalize is the one step that should emit the shared CSS/JS for
    # the whole tree (the per-crate parts step skipped them); also
    # emit the non-static files (index.html, search.index, etc.) that
    # finalize itself produces.
    emit_arg = resolve_emits(args.rustdoc, ["html-static-files", "html-non-static-files"])
    if emit_arg is not None:
        cmd.append(emit_arg)
    for pd in args.parts_dir:
        cmd.append(f"--include-parts-dir={pd}")
    for theme in args.theme:
        cmd.extend(["--theme", theme])
    cmd.extend(args.rustdoc_flag)

    return subprocess.run(cmd, env=env).returncode


if __name__ == "__main__":
    sys.exit(main())
