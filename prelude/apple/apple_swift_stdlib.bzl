# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

_SKIP_COPYING_SWIFT_STDLIB_EXTENSIONS = [
    ".framework",
    ".appex",
]

def should_copy_swift_stdlib(bundle_extension: str) -> bool:
    return bundle_extension not in _SKIP_COPYING_SWIFT_STDLIB_EXTENSIONS
