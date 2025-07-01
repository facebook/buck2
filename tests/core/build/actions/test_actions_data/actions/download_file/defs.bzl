# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

def _test_impl(ctx: AnalysisContext):
    output = ctx.actions.download_file(ctx.label.name, ctx.attrs.url, sha1 = ctx.attrs.sha1)
    return [
        DefaultInfo(default_output = output),
    ]

test = rule(
    impl = _test_impl,
    attrs = {
        "sha1": attrs.string(),
        "url": attrs.string(),
    },
)
