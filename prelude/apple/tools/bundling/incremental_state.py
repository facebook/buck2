# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

# pyre-strict

from __future__ import annotations

import json
from dataclasses import dataclass
from io import TextIOBase
from pathlib import Path
from typing import Any, Dict, List, Optional, Union

from apple.tools.code_signing.codesign_bundle import CodesignConfiguration

_VERSION = 7


@dataclass
class IncrementalStateItem:
    source: Path
    """
    Path relative to buck project
    """
    destination_relative_to_bundle: Path
    digest: Optional[str]
    """
    Required when the source file is not a symlink
    """
    resolved_symlink: Optional[Path]
    """
    Required when the source file is a symlink
    """


@dataclass
class CodesignedOnCopy:
    path: Path
    """
    Path relative to bundle root which needs to be codesigned
    """
    entitlements_digest: Optional[str]
    """
    Digest of entitlements used when the given path is codesigned on copy
    """
    codesign_flags_override: Optional[List[str]]
    """
    If present, overrides codesign arguments (which are used for root bundle) when the given path is codesigned on copy
    """

    def __hash__(self: CodesignedOnCopy) -> int:
        return hash(
            (
                self.path,
                self.entitlements_digest,
                (
                    tuple(self.codesign_flags_override)
                    if self.codesign_flags_override is not None
                    else hash(None)
                ),
            )
        )


@dataclass
class IncrementalState:
    """
    Describes a bundle output from a previous run of this bundling script.
    """

    items: List[IncrementalStateItem]
    codesigned: bool
    codesign_configuration: CodesignConfiguration
    codesigned_on_copy: List[CodesignedOnCopy]
    codesign_identity: Optional[str]
    codesign_arguments: List[str]
    swift_stdlib_paths: List[Path]
    versioned_if_macos: bool
    version: int = _VERSION


class IncrementalStateJSONEncoder(json.JSONEncoder):
    def default(self, o: object) -> object:
        if isinstance(o, IncrementalState):
            return {
                "items": [self.default(i) for i in o.items],
                "codesigned": o.codesigned,
                "codesign_configuration": (
                    o.codesign_configuration.value if o.codesign_configuration else None
                ),
                "codesigned_on_copy": [self.default(i) for i in o.codesigned_on_copy],
                "codesign_identity": o.codesign_identity,
                "swift_stdlib_paths": [str(p) for p in o.swift_stdlib_paths],
                "version": o.version,
                "codesign_arguments": o.codesign_arguments,
                "versioned_if_macos": o.versioned_if_macos,
            }
        elif isinstance(o, IncrementalStateItem):
            result = {
                "source": str(o.source),
                "destination_relative_to_bundle": str(o.destination_relative_to_bundle),
            }
            if o.digest is not None:
                result["digest"] = o.digest
            if o.resolved_symlink is not None:
                result["resolved_symlink"] = str(o.resolved_symlink)
            return result
        elif isinstance(o, CodesignedOnCopy):
            result = {}
            result["path"] = str(o.path)
            if o.entitlements_digest is not None:
                result["entitlements_digest"] = str(o.entitlements_digest)
            if o.codesign_flags_override is not None:
                result["codesign_flags_override"] = o.codesign_flags_override
            return result
        else:
            return super().default(o)


def _object_hook(
    dict: Dict[str, Any]
) -> Union[IncrementalState, IncrementalStateItem, CodesignedOnCopy]:
    if "version" in dict:
        codesign_configuration = dict.pop("codesign_configuration")
        dict["codesign_configuration"] = (
            CodesignConfiguration(codesign_configuration)
            if codesign_configuration
            else None
        )
        dict["swift_stdlib_paths"] = [Path(p) for p in dict.pop("swift_stdlib_paths")]
        return IncrementalState(**dict)
    elif "destination_relative_to_bundle" in dict:
        dict["source"] = Path(dict.pop("source"))
        dict["destination_relative_to_bundle"] = Path(
            dict.pop("destination_relative_to_bundle")
        )
        dict["digest"] = dict.pop("digest", None)
        resolved_symlink = dict.pop("resolved_symlink", None)
        dict["resolved_symlink"] = Path(resolved_symlink) if resolved_symlink else None
        return IncrementalStateItem(**dict)
    else:
        dict["path"] = Path(dict.pop("path"))
        dict["entitlements_digest"] = dict.pop("entitlements_digest", None)
        dict["codesign_flags_override"] = dict.pop("codesign_flags_override", None)
        return CodesignedOnCopy(**dict)


def parse_incremental_state(data: TextIOBase) -> IncrementalState:
    start_stream_position = data.tell()
    try:
        incremental_state = json.load(data, object_hook=_object_hook)
    except BaseException:
        data.seek(start_stream_position)
        version = json.load(data)["version"]
        if version != _VERSION:
            raise RuntimeError(
                f"Expected incremental state version to be `{_VERSION}` got `{version}`."
            )
        else:
            raise
    return incremental_state
