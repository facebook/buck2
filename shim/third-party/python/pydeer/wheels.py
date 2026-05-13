# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

"""PEP 425/427 wheel tag parsing and per-platform wheel selection."""

from __future__ import annotations

import logging
import re
from dataclasses import dataclass

from lockfile import Package, Wheel

log = logging.getLogger(__name__)


@dataclass(frozen=True)
class Target:
    """A target platform pydeer generates a wheel selection for."""

    name: str
    """Stable identifier (e.g. "linux-arm64")."""

    select_keys: tuple[str, ...]
    """Buck select() keys to express this target. A single key means a flat
    select; multiple keys imply a nested os→cpu select."""

    platform_pattern: re.Pattern[str]
    """Regex matched against the platform_tag of a wheel."""


_LINUX_ARM64 = re.compile(r"^(manylinux\w*_aarch64|linux_aarch64|musllinux\w*_aarch64)$")
_LINUX_X86_64 = re.compile(r"^(manylinux\w*_x86_64|linux_x86_64|musllinux\w*_x86_64)$")
_MACOS_ARM64 = re.compile(r"^macosx_\d+_\d+_(arm64|universal2)$")

DEFAULT_TARGETS: tuple[Target, ...] = (
    Target(
        name="linux-arm64",
        select_keys=("prelude//os:linux-arm64",),
        platform_pattern=_LINUX_ARM64,
    ),
    Target(
        name="linux-x86_64",
        select_keys=("prelude//os:linux", "prelude//cpu:x86_64"),
        platform_pattern=_LINUX_X86_64,
    ),
    Target(
        name="macos-arm64",
        select_keys=("prelude//os:macos", "prelude//cpu:arm64"),
        platform_pattern=_MACOS_ARM64,
    ),
)


@dataclass(frozen=True)
class WheelTags:
    python_tags: frozenset[str]
    abi_tags: frozenset[str]
    platform_tags: frozenset[str]


@dataclass(frozen=True)
class ParsedWheel:
    wheel: Wheel
    tags: WheelTags


def parse_filename(filename: str) -> WheelTags:
    """Split a wheel filename into its compatibility tag sets per PEP 427."""
    if not filename.endswith(".whl"):
        raise ValueError(f"Not a wheel filename: {filename!r}")
    stem = filename[: -len(".whl")]
    parts = stem.split("-")
    if len(parts) not in (5, 6):
        raise ValueError(
            f"Wheel filename {filename!r} has {len(parts)} dash-separated "
            "components; expected 5 (no build tag) or 6 (with build tag)"
        )
    python_tag, abi_tag, platform_tag = parts[-3], parts[-2], parts[-1]
    return WheelTags(
        python_tags=frozenset(python_tag.split(".")),
        abi_tags=frozenset(abi_tag.split(".")),
        platform_tags=frozenset(platform_tag.split(".")),
    )


_PYTHON_VERSION_RE = re.compile(r"^(?:py|cp)(\d+)(\d)?$")


def _python_supports(python_tag: str, py_major: int, py_minor: int) -> bool:
    """Whether a python_tag covers the target Python version.

    `py3` and `cp3` match any 3.x. `py311` and `cp311` match 3.11 exactly.
    `py310` does NOT match 3.11 (different minor)."""
    m = _PYTHON_VERSION_RE.match(python_tag)
    if not m:
        return False
    major = int(m.group(1)[0])
    if major != py_major:
        return False
    if m.group(2) is None and len(m.group(1)) == 1:
        return True
    minor_part = m.group(1)[1:]
    if not minor_part:
        return True
    return int(minor_part) == py_minor


def _abi3_supports(python_tag: str, py_major: int, py_minor: int) -> bool:
    """abi3 wheels are forward-compatible: a cp310 abi3 wheel works on 3.10+."""
    m = _PYTHON_VERSION_RE.match(python_tag)
    if not m or not python_tag.startswith("cp"):
        return False
    major = int(m.group(1)[0])
    if major != py_major:
        return False
    minor_part = m.group(1)[1:]
    if not minor_part:
        return True
    return int(minor_part) <= py_minor


def _platform_matches(platform_tags: frozenset[str], target: Target) -> bool:
    if "any" in platform_tags:
        return True
    return any(target.platform_pattern.match(t) for t in platform_tags)


def _wheel_priority(
    tags: WheelTags,
    target: Target,
    py_major: int,
    py_minor: int,
) -> int | None:
    """Return a priority for this wheel against the target. Lower is better.
    None means the wheel does not satisfy the target."""
    if not _platform_matches(tags.platform_tags, target):
        return None

    exact = f"cp{py_major}{py_minor}"
    if exact in tags.python_tags and exact in tags.abi_tags:
        return 0

    if "abi3" in tags.abi_tags and any(
        _abi3_supports(p, py_major, py_minor) for p in tags.python_tags
    ):
        return 1

    if "none" in tags.abi_tags and "any" in tags.platform_tags and any(
        _python_supports(p, py_major, py_minor) for p in tags.python_tags
    ):
        return 2

    if "none" in tags.abi_tags and any(
        _python_supports(p, py_major, py_minor) for p in tags.python_tags
    ):
        return 3

    return None


def select_wheel(
    parsed: list[ParsedWheel],
    target: Target,
    py_major: int,
    py_minor: int,
) -> ParsedWheel | None:
    """Pick the best wheel for the target, or None if none match."""
    candidates: list[tuple[int, ParsedWheel]] = []
    for pw in parsed:
        priority = _wheel_priority(pw.tags, target, py_major, py_minor)
        if priority is not None:
            candidates.append((priority, pw))
    if not candidates:
        return None
    candidates.sort(key=lambda x: (x[0], x[1].wheel.filename))
    return candidates[0][1]


def select_for_package(
    package: Package,
    targets: tuple[Target, ...],
    py_major: int,
    py_minor: int,
) -> dict[str, ParsedWheel]:
    """Return target.name -> chosen wheel for every target that has a match."""
    parsed: list[ParsedWheel] = []
    for w in package.wheels:
        try:
            parsed.append(ParsedWheel(wheel=w, tags=parse_filename(w.filename)))
        except ValueError as e:
            log.warning("skipping unparseable wheel for %s: %s", package.name, e)

    selection: dict[str, ParsedWheel] = {}
    for target in targets:
        chosen = select_wheel(parsed, target, py_major, py_minor)
        if chosen is not None:
            selection[target.name] = chosen
    return selection
