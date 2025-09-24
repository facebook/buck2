# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

CcTestValueInfo = provider(
    fields = {
        "value": provider_field(typing.Any)
    }
)

CcTestTypeSizeInfo = provider(
    fields = {
        "type": provider_field(str),
        "size": provider_field(int)
    }
)

def cctest_value_impl(ctx: AnalysisContext) -> list[Provider]:
    return [
        DefaultInfo(),
        CcTestValueInfo(value = ctx.attrs.value)
    ]

def cctest_type_exists_impl(ctx: AnalysisContext) -> list[Provider]:
    return [
        DefaultInfo(),
        CcTestValueInfo(value = ctx.attrs.exists)
    ]


def cctest_type_size_impl(ctx: AnalysisContext) -> list[Provider]:
    type_exists = ctx.attrs.size != None
    return [
        DefaultInfo(),
        CcTestTypeSizeInfo(type = ctx.attrs.type, size = ctx.attrs.size),
        CcTestValueInfo(value = type_exists)
    ]

def cctest_map_value_impl(ctx: AnalysisContext) -> list[Provider]:
    original = ctx.attrs.original[CcTestValueInfo].value

    if original not in ctx.attrs.actual:
        fail(f"Value {original} does not contain a mapping.")

    new = ctx.attrs.actual[original]
    return [
        DefaultInfo(),
        CcTestValueInfo(value = new)
    ]
