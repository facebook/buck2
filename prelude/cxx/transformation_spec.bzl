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
})

def build_determine_transformation(
        _transformations: list[tuple]) -> typing.Callable[[Label], TransformationKind | None]:
    def callable(_label: Label) -> TransformationKind | None:
        return None

    return callable

def transformation_spec_impl(ctx: AnalysisContext) -> list[Provider]:
    return [
        DefaultInfo(),
        TransformationResultProvider(
            determine_transformation = build_determine_transformation(ctx.attrs.transformations),
        ),
    ]
