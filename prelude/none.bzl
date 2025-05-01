# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

load(
    "@prelude//linking:shared_libraries.bzl",
    "SharedLibraryInfo",
)

def _impl(_ctx: AnalysisContext) -> list[Provider]:
    return [DefaultInfo(), SharedLibraryInfo()]

none_rule = rule(
    attrs = {},
    impl = _impl,
    doc = "A rule that produces nothing. Used for no-op dep in a select.",
)
