# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

load("@prelude//cxx:cxx_sources.bzl", "get_srcs_with_flags")

def _value_impl(_ctx):
    return [DefaultInfo()]

# Carries a string computed at load time, so a test can read it with `uquery --output-attribute`.
value = rule(
    impl = _value_impl,
    attrs = {"value": attrs.string()},
)

def _srcs_with_flags_impl(ctx):
    compiles = get_srcs_with_flags(ctx)
    out = ctx.actions.write("compiles.txt", [str(c.index) for c in compiles])
    return [DefaultInfo(default_output = out)]

# Lists the per-file compile index of every (source, flags) pair the cxx rules would compile.
srcs_with_flags = rule(
    impl = _srcs_with_flags_impl,
    attrs = {
        "srcs": attrs.list(attrs.one_of(attrs.source(), attrs.tuple(attrs.source(), attrs.list(attrs.arg())))),
    },
)
