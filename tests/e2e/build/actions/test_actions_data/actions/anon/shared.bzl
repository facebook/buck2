# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

# A very basic test that two things can share a single anon target

SharedInfo = provider(fields = ["information"])

def _assert_eq(a, b):
    if a != b:
        fail("Expected {} == {}".format(a, b))

def _shared_impl(ctx: AnalysisContext) -> list[Provider]:
    return [DefaultInfo(), SharedInfo(information = ctx.label.name)]

_shared = rule(impl = _shared_impl, attrs = {})

def _user_impl(ctx: AnalysisContext) -> Promise:
    def f(providers):
        _assert_eq(providers[SharedInfo].information, ctx.attrs.use)
        return [DefaultInfo()]

    return ctx.actions.anon_target(_shared, {"name": "hello//world:" + ctx.attrs.use}).promise.map(f)

_user = rule(impl = _user_impl, attrs = {"use": attrs.string()})

def shared_test():
    _user(name = "shared_a1", use = "a")
    _user(name = "shared_a2", use = "a")
    _user(name = "shared_b", use = "b")

    # It's also a regular rule
    _shared(name = "shared")
