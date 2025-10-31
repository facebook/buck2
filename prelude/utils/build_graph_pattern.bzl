# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

_BuildGraphPatternKind = enum(
    "root",
    "root/...",
    "first_order_deps",
)

BuildGraphInfo = record(
    root = Label,
    first_order_deps = set[Label],
)

def new_build_graph_info(ctx: AnalysisContext, deps: list[Dependency]) -> BuildGraphInfo:
    return BuildGraphInfo(
        root = ctx.label,
        first_order_deps = set([dep.label for dep in deps]),
    )

BuildGraphPattern = record(
    matches = field(typing.Callable[[Label, BuildGraphInfo], bool]),
)

def parse_build_graph_pattern(pattern: str) -> BuildGraphPattern:
    kind = _BuildGraphPatternKind(pattern)

    def matches(label: Label, info: BuildGraphInfo) -> bool:
        if kind == _BuildGraphPatternKind("root"):
            return label == info.root
        elif kind == _BuildGraphPatternKind("first_order_deps"):
            return label in info.first_order_deps
        elif kind == _BuildGraphPatternKind("root/..."):
            return label.package.startswith(info.root.package)
        return False

    return BuildGraphPattern(matches = matches)
