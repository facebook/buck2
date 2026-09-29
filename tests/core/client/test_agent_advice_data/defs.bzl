# (c) Meta Platforms, Inc. and affiliates. Confidential and proprietary.

def _pass_impl(ctx):
    out = ctx.actions.write("out", "pass")
    check = ctx.actions.write("check", "checked")
    return [
        DefaultInfo(
            default_output = out,
            sub_targets = {"check": [DefaultInfo(default_output = check)]},
        )
    ]

pass_ = rule(impl = _pass_impl, attrs = {})
