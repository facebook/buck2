# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

# Implementation of aliases build rules.

def alias_impl(ctx: AnalysisContext) -> list[Provider]:
    if ctx.attrs.actual:
        return ctx.attrs.actual.providers
    else:
        return [DefaultInfo()]

def configured_alias_impl(ctx: AnalysisContext) -> list[Provider]:
    if ctx.attrs.configured_actual != None and ctx.attrs.fallback_actual != None:
        fail("cannot set both of `configured_actual` and `fallback_actual`")
    if ctx.attrs.configured_actual != None:
        return ctx.attrs.configured_actual.providers
    if ctx.attrs.fallback_actual != None:
        return ctx.attrs.fallback_actual.providers
    fail("must set one of `configured_actual` or `fallback_actual`")

def versioned_alias_impl(_ctx: AnalysisContext) -> list[Provider]:
    # Should be intercepted in macro stub and converted to `alias`.
    fail("unsupported")
