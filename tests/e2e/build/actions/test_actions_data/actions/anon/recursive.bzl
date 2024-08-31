# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

# @lint-ignore-every FBCODEBZLADDLOADS

# A test that you can recursively keep adding more anon targets,
# or use anon_targets

IntInfo = provider(fields = ["value"])

def _assert_eq(a, b):
    if a != b:
        fail("Expected {} == {}".format(a, b))

def _int_impl(ctx: AnalysisContext) -> list[Provider]:
    return [DefaultInfo(), IntInfo(value = ctx.attrs.value)]

_int = rule(
    impl = _int_impl,
    attrs = {"value": attrs.int()},
)

def _recursive_impl(ctx: AnalysisContext) -> Promise:
    def f(x1, x2, x3):
        _assert_eq(x1[IntInfo].value, 1)
        _assert_eq(x2[IntInfo].value, 2)
        _assert_eq(x3[IntInfo].value, 1)
        return [DefaultInfo()]

    return ctx.actions.anon_target(_int, {"value": 1}).promise.map(
        lambda x1: ctx.actions.anon_target(_int, {"value": 2}).promise.map(
            lambda x2: ctx.actions.anon_target(_int, {"value": 1}).promise.map(
                lambda x3: f(x1, x2, x3),
            ),
        ),
    )

_recursive = rule(
    impl = _recursive_impl,
    attrs = {},
)

def _plural_impl(ctx: AnalysisContext) -> Promise:
    def f(xs):
        x1, x2, x3 = xs
        _assert_eq(x1[IntInfo].value, 1)
        _assert_eq(x2[IntInfo].value, 2)
        _assert_eq(x3[IntInfo].value, 1)
        return [DefaultInfo()]

    return ctx.actions.anon_targets([(_int, {"value": 1}), (_int, {"value": 2}), (_int, {"value": 1})]).promise.map(f)

_plural = rule(
    impl = _plural_impl,
    attrs = {},
)

def recursive_test():
    _recursive(name = "recursive")
    _plural(name = "plural")
