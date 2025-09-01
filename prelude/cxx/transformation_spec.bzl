# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

TransformationKind = enum(
    "debug",
    "optimized",
)

TransformationResultProvider = provider(fields = {
    "determine_transformation": provider_field(typing.Callable[[Label], TransformationKind | None]),
    "is_empty": provider_field(bool),
})

def build_determine_transformation(
        transformations: list[tuple]) -> typing.Callable[[Label], TransformationKind | None]:
    def callable(label: Label) -> TransformationKind | None:
        for transform in transformations:
            dep = transform[0]  # str | Dependency
            kind = transform[1]  # TransformationKind
            if label == dep.label:
                return TransformationKind(kind)
        return None

    return callable

def transformation_spec_impl(ctx: AnalysisContext) -> list[Provider]:
    return [
        DefaultInfo(),
        TransformationResultProvider(
            is_empty = len(ctx.attrs.transformations) == 0,
            determine_transformation = build_determine_transformation(ctx.attrs.transformations),
        ),
    ]
