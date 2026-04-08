# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

"""pydeer: generate BUCK files from a uv.lock."""

from __future__ import annotations

import argparse
import logging
import os
import subprocess
import sys
from pathlib import Path

import generator
import lockfile
import wheels

log = logging.getLogger("pydeer")


class PydeerError(Exception):
    pass


def _ensure_lockfile(third_party_dir: Path, allow_lock: bool) -> Path:
    pyproject = third_party_dir / "pyproject.toml"
    uv_lock = third_party_dir / "uv.lock"

    if not pyproject.is_file():
        raise PydeerError(f"Missing pyproject.toml at {pyproject}")

    needs_lock = (
        not uv_lock.is_file()
        or uv_lock.stat().st_mtime < pyproject.stat().st_mtime
    )
    if not needs_lock:
        return uv_lock
    if not allow_lock:
        raise PydeerError(
            f"{uv_lock} is missing or stale and --no-lock was given. "
            "Run `uv lock` manually."
        )

    log.info("running `uv lock` in %s", third_party_dir)
    try:
        subprocess.run(["uv", "lock"], cwd=third_party_dir, check=True)
    except FileNotFoundError as e:
        raise PydeerError(
            "`uv` was not found on PATH. Install it via `pip install uv` "
            "or `brew install uv`."
        ) from e
    except subprocess.CalledProcessError as e:
        raise PydeerError(f"`uv lock` failed with exit code {e.returncode}") from e
    return uv_lock


def _parse_python_version(value: str) -> tuple[int, int]:
    parts = value.split(".")
    if len(parts) != 2 or not all(p.isdigit() for p in parts):
        raise argparse.ArgumentTypeError(
            f"--python expects MAJOR.MINOR (e.g. 3.11), got {value!r}"
        )
    return int(parts[0]), int(parts[1])


def _select_targets(names: list[str]) -> tuple[wheels.Target, ...]:
    by_name = {t.name: t for t in wheels.DEFAULT_TARGETS}
    chosen: list[wheels.Target] = []
    unknown: list[str] = []
    for n in names:
        if n in by_name:
            chosen.append(by_name[n])
        else:
            unknown.append(n)
    if unknown:
        raise PydeerError(
            f"Unknown platform(s): {', '.join(unknown)}. "
            f"Known: {', '.join(t.name for t in wheels.DEFAULT_TARGETS)}"
        )
    return tuple(chosen)


def _label_for(third_party_dir: Path, cell_root: Path | None) -> str:
    """Compute the cell-relative directory label used as a label prefix.

    For example cell_root=shim, third_party_dir=shim/third-party/pypi
    yields //third-party/pypi.

    `cell_root` defaults to $BUCK2_OSS_REPO_DIR if set, else cwd."""
    if cell_root is None:
        cell_root = Path(os.environ.get("BUCK2_OSS_REPO_DIR", os.getcwd()))
    cell_root = cell_root.resolve()
    abs_dir = third_party_dir.resolve()
    try:
        rel = abs_dir.relative_to(cell_root)
    except ValueError as e:
        raise PydeerError(
            f"--third-party-dir {third_party_dir} is not under cell root "
            f"{cell_root}; pass --cell-root or set BUCK2_OSS_REPO_DIR."
        ) from e
    return "//" + rel.as_posix()


def buckify(args: argparse.Namespace) -> None:
    third_party_dir = Path(args.third_party_dir).resolve()
    cell_root = Path(args.cell_root).resolve() if args.cell_root else None
    targets = _select_targets([n.strip() for n in args.platforms.split(",") if n.strip()])
    py_major, py_minor = args.python

    uv_lock_path = _ensure_lockfile(third_party_dir, allow_lock=not args.no_lock)
    lock = lockfile.parse(uv_lock_path)
    log.info("parsed %d packages from %s", len(lock.packages), uv_lock_path)

    label_prefix = _label_for(third_party_dir, cell_root)
    log.debug("third-party label prefix: %s", label_prefix)

    sdist_only: list[tuple[str, str]] = []
    no_wheel_match: list[tuple[str, str]] = []
    builds: list[generator.PackageBuild] = []

    for pkg in lock.packages:
        if not pkg.wheels:
            sdist_only.append((pkg.name, pkg.version))
            continue
        selection = wheels.select_for_package(pkg, targets, py_major, py_minor)
        if not selection:
            no_wheel_match.append((pkg.name, pkg.version))
            continue
        builds.append(
            generator.PackageBuild(
                package=pkg,
                wheels_by_target=selection,
                third_party_dir_label=label_prefix,
            )
        )

    errors: list[str] = []
    if sdist_only:
        rows = "\n".join(f"  - {n} {v}" for n, v in sorted(sdist_only))
        errors.append(
            "The following packages have no wheels (sdist-only). pydeer does "
            f"not yet support building from source:\n{rows}"
        )
    if no_wheel_match:
        rows = "\n".join(f"  - {n} {v}" for n, v in sorted(no_wheel_match))
        errors.append(
            "The following packages have wheels, but none match Python "
            f"{py_major}.{py_minor} on any of the requested platforms "
            f"({', '.join(t.name for t in targets)}):\n{rows}"
        )
    if errors:
        raise PydeerError("\n\n".join(errors))

    third_party_dir.mkdir(parents=True, exist_ok=True)
    for build in builds:
        generator.write_package(build, targets, third_party_dir)

    log.info(
        "generated %d BUCK.pydeer files in %s for platforms: %s",
        len(builds),
        third_party_dir,
        ", ".join(t.name for t in targets),
    )


def lock_only(args: argparse.Namespace) -> None:
    third_party_dir = Path(args.third_party_dir).resolve()
    _ensure_lockfile(third_party_dir, allow_lock=True)


def build_parser() -> argparse.ArgumentParser:
    parser = argparse.ArgumentParser(
        prog="pydeer",
        description="Generate BUCK files from a uv.lock.",
    )
    sub = parser.add_subparsers(dest="command", required=True)

    p_buckify = sub.add_parser("buckify", help="Generate BUCK files from uv.lock.")
    p_buckify.add_argument("--third-party-dir", required=True)
    p_buckify.add_argument(
        "--cell-root",
        default=None,
        help="Path to the Buck2 cell root containing --third-party-dir. "
        "Generated labels are relative to this. "
        "Defaults to $BUCK2_OSS_REPO_DIR or cwd.",
    )
    p_buckify.add_argument(
        "--platforms",
        default="linux-arm64,linux-x86_64,macos-arm64",
        help="Comma-separated list of target platforms.",
    )
    p_buckify.add_argument(
        "--python",
        type=_parse_python_version,
        default=(3, 11),
        help="Target Python version as MAJOR.MINOR (default: 3.11).",
    )
    p_buckify.add_argument(
        "--no-lock",
        action="store_true",
        help="Do not run `uv lock`; require uv.lock to be present.",
    )
    p_buckify.add_argument("-v", "--verbose", action="store_true")
    p_buckify.set_defaults(func=buckify)

    p_lock = sub.add_parser("lock", help="Run `uv lock` only; do not generate BUCK files.")
    p_lock.add_argument("--third-party-dir", required=True)
    p_lock.add_argument("-v", "--verbose", action="store_true")
    p_lock.set_defaults(func=lock_only)

    return parser


def main(argv: list[str] | None = None) -> int:
    parser = build_parser()
    args = parser.parse_args(argv)
    logging.basicConfig(
        level=logging.DEBUG if args.verbose else logging.INFO,
        format="%(levelname)s %(name)s: %(message)s",
    )
    try:
        args.func(args)
    except PydeerError as e:
        log.error("%s", e)
        return 1
    return 0


if __name__ == "__main__":
    sys.exit(main())
