# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

_BuildGraphPatternKind = enum(
    "root",
)

BuildGraphInfo = record(
    root = Label,
)

def new_build_graph_info(ctx: AnalysisContext) -> BuildGraphInfo:
    return BuildGraphInfo(
        root = ctx.label,
    )

BuildGraphPattern = record(
    matches = field(typing.Callable[[Label, BuildGraphInfo], bool]),
)

def parse_build_graph_pattern(pattern: str) -> BuildGraphPattern:
    kind = _BuildGraphPatternKind(pattern)

    def matches(label: Label, info: BuildGraphInfo) -> bool:
        if kind == _BuildGraphPatternKind("root"):
            return label == info.root
        return False

    return BuildGraphPattern(matches = matches)
