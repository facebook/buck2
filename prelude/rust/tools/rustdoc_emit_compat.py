# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

"""Shared helper for rustdoc's 2026-03 `--emit=` kind rename
(https://github.com/rust-lang/rust/commit/c6b7f630a5365ccfe2d4f24c7f5a4cc8499c5a55):

    invocation-specific        -> html-non-static-files
    toolchain-shared-resources -> html-static-files

Used by `rustdoc_parts_emit_argfile.py` (per-crate parts step) and
`rustdoc_merge.py` (finalize step).
"""

import subprocess


RENAMES = {
    "html-non-static-files": "invocation-specific",
    "html-static-files": "toolchain-shared-resources",
}


def resolve_emits(rustdoc: str, emits: list) -> str | None:
    """Return a `--emit=...` value with each post-rename name in `emits`
    substituted to its pre-rename equivalent if the current rustdoc
    doesn't recognise it.

    Returns `None` if rustdoc has no `--emit` line at all, or if every
    requested name (and its pre-rename equivalent) is absent from that
    line — callers should omit the flag rather than feed rustdoc a name
    it'll reject.
    """
    help_out = subprocess.run(
        [rustdoc, "--help"],
        stdout=subprocess.PIPE,
        stderr=subprocess.STDOUT,
        check=True,
    ).stdout.decode("utf-8", errors="replace")
    # `rustdoc --help` prints the valid emit kinds on the `--emit` line,
    # e.g. `--emit [toolchain-shared-resources,invocation-specific,dep-info]`.
    # Scope the substring check to that line so we don't false-match on
    # `html-non-static-files` appearing in some other description.
    emit_line = next(
        (line for line in help_out.splitlines() if "--emit" in line),
        None,
    )
    if emit_line is None:
        return None

    resolved = []
    for e in emits:
        if e in emit_line:
            resolved.append(e)
        elif e in RENAMES and RENAMES[e] in emit_line:
            resolved.append(RENAMES[e])
        # else: rustdoc knows neither name — drop this emit silently.

    if not resolved:
        return None
    return "--emit=" + ",".join(resolved)
