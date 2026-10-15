# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

def _scratch_user(ctx: AnalysisContext):
    out = ctx.actions.declare_output("scratch_path", has_content_based_path = False)
    ctx.actions.run(
        [
            "fbpython",
            ctx.attrs.script,
            out.as_output(),
            "fail" if ctx.attrs.fail else "ok",
        ],
        category = "scratch_user",
        local_only = True,
    )
    return [DefaultInfo(default_output = out)]

scratch_user = rule(attrs = {"fail": attrs.bool(), "script": attrs.source()}, impl = _scratch_user)
