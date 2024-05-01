# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

# pyre-strict

from __future__ import annotations

import functools
from dataclasses import dataclass
from pathlib import Path
from typing import Dict, List, Optional

from apple.tools.code_signing.codesign_bundle import CodesignConfiguration

from .incremental_state import IncrementalState


@functools.total_ordering
@dataclass
class BundleSpecItem:
    src: str
    # Should be bundle relative path, empty string means the root of the bundle
    dst: str
    codesign_on_copy: bool = False
    codesign_entitlements: Optional[str] = None
    codesign_flags_override: Optional[List[str]] = None

    def __eq__(self: BundleSpecItem, other: Optional[BundleSpecItem]) -> bool:
        return (
            other is not None
            and self.src == other.src
            and self.dst == other.dst
            and self.codesign_on_copy == other.codesign_on_copy
            and self.codesign_entitlements == other.codesign_entitlements
            and self.codesign_flags_override == other.codesign_flags_override
        )

    def __ne__(self: BundleSpecItem, other: BundleSpecItem) -> bool:
        return not self.__eq__(other)

    def __hash__(self: BundleSpecItem) -> int:
        return hash(
            (
                self.src,
                self.dst,
                self.codesign_on_copy,
                self.codesign_entitlements,
                (
                    tuple(self.codesign_flags_override)
                    if self.codesign_flags_override is not None
                    else hash(None)
                ),
            )
        )

    def __lt__(self: BundleSpecItem, other: BundleSpecItem) -> bool:
        return (
            self.src < other.src
            or self.dst < other.dst
            or self.codesign_on_copy < other.codesign_on_copy
            or (
                self.codesign_entitlements < other.codesign_entitlements
                if (
                    self.codesign_entitlements is not None
                    and other.codesign_entitlements is not None
                )
                else (
                    self.codesign_entitlements is None
                    and other.codesign_entitlements is not None
                )
            )
            or (
                self.codesign_flags_override < other.codesign_flags_override
                if (
                    self.codesign_flags_override is not None
                    and other.codesign_flags_override is not None
                )
                else (
                    self.codesign_flags_override is None
                    and other.codesign_flags_override is not None
                )
            )
        )


@dataclass
class IncrementalContext:
    """
    Additional data you need to bundle incrementally (extra vs when non-incrementally).
    """

    # Maps buck-project relative path to hash digest of the input file.
    metadata: Dict[Path, str]
    # Present when there is a valid incremental state on disk (i.e. previous build produced it).
    state: Optional[IncrementalState]
    codesigned: bool
    codesign_configuration: Optional[CodesignConfiguration]
    codesign_identity: Optional[str]
    codesign_arguments: List[str]
    versioned_if_macos: bool
