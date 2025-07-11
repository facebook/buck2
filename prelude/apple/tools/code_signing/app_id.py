# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

# pyre-strict

from __future__ import annotations

import re
from dataclasses import dataclass
from enum import Enum
from typing import Any, Dict, Optional


@dataclass
class AppId:
    team_id: str
    bundle_id: str

    class _ReGroupName(str, Enum):
        team_id = "team_id"
        bundle_id = "bundle_id"

    _re_string: str = "^(?P<{team_id}>[A-Z0-9]{{10}})\\.(?P<{bundle_id}>.+)$".format(
        team_id=_ReGroupName.team_id.value,
        bundle_id=_ReGroupName.bundle_id.value,
    )
    _re_pattern: re.Pattern[str] = re.compile(_re_string)

    # Takes a application identifier and splits it into Team ID and bundle ID.
    # Prefix is always a ten-character alphanumeric sequence. Bundle ID may be a fully-qualified name or a wildcard ending in *.
    @classmethod
    def from_string(cls, string: str) -> AppId:
        match = re.match(cls._re_pattern, string)
        if not match:
            raise RuntimeError(
                "Malformed app ID string: '{}'. "
                "We expected a prefix of a ten-character alphanumeric sequence and a Bundle ID which may be a fully-qualified name or a wildcard ending in '*'.".format(
                    string
                )
            )
        return AppId(
            match.group(cls._ReGroupName.team_id.value),
            match.group(cls._ReGroupName.bundle_id.value),
        )

    # Returns the App ID if it can be inferred from keys in the entitlement. Otherwise, it returns `None`.
    @staticmethod
    def infer_from_entitlements(entitlements: Dict[str, Any]) -> Optional[AppId]:
        try:
            keychain_access_groups = entitlements.get("keychain-access-groups")
            if not keychain_access_groups:
                return None
            app_id_string = keychain_access_groups[0]
            return AppId.from_string(app_id_string)
        except Exception as e:
            raise RuntimeError(
                "Error when parsing the entitlements for the app ID: {}".format(e)
            )
