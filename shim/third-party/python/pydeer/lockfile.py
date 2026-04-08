# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

"""Parse uv.lock into typed dataclasses."""

from __future__ import annotations

import logging
import re
import tomllib
from dataclasses import dataclass
from pathlib import Path
from urllib.parse import unquote, urlparse

log = logging.getLogger(__name__)

_SHA256_PREFIX = "sha256:"


@dataclass(frozen=True)
class Wheel:
    url: str
    sha256: str
    filename: str


@dataclass(frozen=True)
class Sdist:
    url: str
    sha256: str


@dataclass(frozen=True)
class Dependency:
    name: str


@dataclass(frozen=True)
class Package:
    name: str
    version: str
    source: str
    deps: tuple[Dependency, ...]
    wheels: tuple[Wheel, ...]
    sdist: Sdist | None


@dataclass(frozen=True)
class Lockfile:
    requires_python: str
    packages: tuple[Package, ...]


def canonicalize(name: str) -> str:
    """PEP 503 name canonicalization."""
    return re.sub(r"[-_.]+", "-", name).lower()


def _strip_sha256(hash_value: str) -> str:
    if not hash_value.startswith(_SHA256_PREFIX):
        raise ValueError(f"Expected sha256: prefix on hash, got {hash_value!r}")
    return hash_value[len(_SHA256_PREFIX) :]


def _filename_from_url(url: str) -> str:
    path = urlparse(url).path
    return unquote(path.rsplit("/", 1)[-1])


def _classify_source(source: dict[str, str]) -> str:
    for key in ("registry", "virtual", "editable", "git", "path", "url", "directory"):
        if key in source:
            return key
    return "unknown"


def _parse_wheel(entry: dict[str, object]) -> Wheel:
    url = str(entry["url"])
    return Wheel(
        url=url,
        sha256=_strip_sha256(str(entry["hash"])),
        filename=_filename_from_url(url),
    )


def _parse_sdist(entry: dict[str, object]) -> Sdist:
    return Sdist(
        url=str(entry["url"]),
        sha256=_strip_sha256(str(entry["hash"])),
    )


def _parse_package(raw: dict[str, object]) -> Package:
    name = canonicalize(str(raw["name"]))
    version = str(raw["version"])
    source = _classify_source(raw.get("source") or {})  # type: ignore[arg-type]

    raw_deps = raw.get("dependencies") or []
    deps = tuple(
        Dependency(name=canonicalize(str(d["name"])))
        for d in raw_deps  # type: ignore[union-attr]
    )

    raw_wheels = raw.get("wheels") or []
    wheels = tuple(_parse_wheel(w) for w in raw_wheels)  # type: ignore[arg-type]

    raw_sdist = raw.get("sdist")
    sdist = _parse_sdist(raw_sdist) if raw_sdist else None  # type: ignore[arg-type]

    return Package(
        name=name,
        version=version,
        source=source,
        deps=deps,
        wheels=wheels,
        sdist=sdist,
    )


def parse(path: Path) -> Lockfile:
    log.debug("parsing %s", path)
    with path.open("rb") as f:
        data = tomllib.load(f)

    requires_python = str(data.get("requires-python", ""))
    raw_packages = data.get("package") or []

    packages: list[Package] = []
    for raw in raw_packages:
        pkg = _parse_package(raw)
        if pkg.source in ("virtual", "editable", "directory", "path"):
            log.debug("skipping workspace package %s (source=%s)", pkg.name, pkg.source)
            continue
        packages.append(pkg)

    log.debug("parsed %d packages from %s", len(packages), path)
    return Lockfile(
        requires_python=requires_python,
        packages=tuple(packages),
    )
