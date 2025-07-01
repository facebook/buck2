# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

# pyre-strict

import plistlib
from io import BytesIO
from typing import Any, Dict, IO


def _is_fmt_binary(header: bytes) -> bool:
    return header[:8] == b"bplist00"


def detect_format_and_load(fp: IO[bytes]) -> Dict[str, Any]:
    header = fp.read(32)
    fp.seek(0)
    if _is_fmt_binary(header):
        fmt = plistlib.FMT_BINARY
    else:
        fmt = plistlib.FMT_XML
    return plistlib.load(fp, fmt=fmt)


def detect_format_and_loads(value: bytes) -> Dict[str, Any]:
    fp = BytesIO(value)
    return detect_format_and_load(fp)
