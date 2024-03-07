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
from typing import Dict, Optional

from apple.tools.code_signing.codesign_bundle import CodesignConfiguration

from .incremental_state import IncrementalState


@functools.total_ordering
@dataclass
class BundleSpecItem:
    src: str
    # Should be bundle relative path, empty string means the root of the bundle
    dst: str
    codesign_on_copy: bool = False

    def __eq__(self: BundleSpecItem, other: Optional[BundleSpecItem]) -> bool:
        return (
            other is not None
            and self.src == other.src
            and self.dst == other.dst
            and self.codesign_on_copy == other.codesign_on_copy
        )

    def __ne__(self: BundleSpecItem, other: BundleSpecItem) -> bool:
        return not self.__eq__(other)

    def __hash__(self: BundleSpecItem) -> int:
        return hash((self.src, self.dst, self.codesign_on_copy))

    def __lt__(self: BundleSpecItem, other: BundleSpecItem) -> bool:
        return (
            self.src < other.src
            or self.dst < other.dst
            or self.codesign_on_copy < other.codesign_on_copy
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
