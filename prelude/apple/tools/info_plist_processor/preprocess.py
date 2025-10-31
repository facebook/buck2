# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

# pyre-strict

import json
import re
import sys
from enum import Enum
from typing import Dict, Optional, TextIO


class _ReGroupName(str, Enum):
    openparen = "openparen"
    variable = "variable"
    modifier = "modifier"
    closeparen = "closeparen"

    def __format__(self, spec):
        return f"{self.value}"


_re_string: str = "\\$(?P<{openparen}>[\\{{\\(])(?P<{variable}>[^\\}}\\):]+)(?::(?P<{modifier}>[^\\}}\\)]+))?(?P<{closeparen}>[\\}}\\)])".format(
    openparen=_ReGroupName.openparen,
    variable=_ReGroupName.variable,
    modifier=_ReGroupName.modifier,
    closeparen=_ReGroupName.closeparen,
)


def _make_substitution_dict(
    substitutions_json_file: TextIO, product_name: str
) -> Dict[str, str]:
    result = {
        "EXECUTABLE_NAME": product_name,
        "PRODUCT_NAME": product_name,
    }
    if substitutions_json_file is not None:
        # JSON file take precedence over default substitutions
        result.update(json.load(substitutions_json_file))
    return result


def _process_line(
    line: str,
    pattern: re.Pattern[str],
    substitutions: Dict[str, str],
    minimum_os_version_key: Optional[str],
) -> str:
    if (
        minimum_os_version_key is not None
        and line.strip() == f"<key>{minimum_os_version_key}</key>"
    ):
        print("Found minimum OS version key in plist file", file=sys.stderr)
        sys.exit(1)

    result = line
    pos = 0
    substituted_keys = set()
    while True:
        match = pattern.search(result, pos)
        if match is None:
            break
        key = match.group(_ReGroupName.variable)
        if key in substituted_keys:
            raise RuntimeError("Recursive plist variable: ... -> {} -> ...".format(key))
        if key in substitutions:
            result = (
                result[: match.start()] + substitutions[key] + result[match.end() :]
            )
            substituted_keys.add(key)
            # Keep the same position to handle the situation when variable was expanded into another variable
            new_pos = match.start()
        else:
            new_pos = match.end()
        if new_pos != pos:
            substituted_keys = set()
            pos = new_pos
    return result


def preprocess(
    input_file: TextIO,
    output_file: TextIO,
    substitutions_file: TextIO,
    product_name: str,
    minimum_os_version_key: Optional[str] = None,
) -> None:
    pattern = re.compile(_re_string)
    substitutions = _make_substitution_dict(substitutions_file, product_name)
    for line in input_file:
        output_file.write(
            _process_line(line, pattern, substitutions, minimum_os_version_key)
        )
