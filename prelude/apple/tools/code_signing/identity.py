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
from typing import List


@dataclass
class CodeSigningIdentity:
    fingerprint: str
    subject_common_name: str

    class _ReGroupName(str, Enum):
        fingerprint = "fingerprint"
        subject_common_name = "subject_common_name"

    _re_string: str = '(?P<{fingerprint}>[A-F0-9]{{40}}) "(?P<{subject_common_name}>.+)"(?!.*CSSMERR_.+)'.format(
        fingerprint=_ReGroupName.fingerprint.value,
        subject_common_name=_ReGroupName.subject_common_name.value,
    )

    _pattern: re.Pattern[str] = re.compile(_re_string)

    @classmethod
    def parse_security_stdout(cls, text: str) -> List[CodeSigningIdentity]:
        return [
            CodeSigningIdentity(
                match.group(cls._ReGroupName.fingerprint.value),
                match.group(cls._ReGroupName.subject_common_name.value),
            )
            for match in re.finditer(cls._pattern, text)
        ]
