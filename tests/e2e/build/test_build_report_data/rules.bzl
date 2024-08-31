# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

def _touch_file_impl(ctx):
    if ctx.attrs.out != None:
        out = ctx.actions.write(ctx.attrs.out, "")
        default_outputs = [out]
        named_outputs = {}
    elif ctx.attrs.outs != None:
        default_outputs = []
        named_outputs = {}
        default_out_paths = ctx.attrs.default_outs or []
        for (name, path) in ctx.attrs.outs.items():
            artifact = ctx.actions.write(path, "")
            if path in default_out_paths:
                default_outputs.append(artifact)
            named_outputs[name] = artifact
    else:
        fail("One of `out` or `outs` should be set.")
    providers = [DefaultInfo(
        default_outputs = default_outputs,
        sub_targets = {k: [DefaultInfo(default_output = v)] for (k, v) in named_outputs.items()},
    )]
    return providers

touch_file = rule(
    impl = _touch_file_impl,
    attrs = {
        "default_outs": attrs.option(attrs.set(attrs.string(), sorted = False), default = None),
        "deps": attrs.list(attrs.dep(), default = []),
        "out": attrs.option(attrs.string(), default = None),
        "outs": attrs.option(attrs.dict(key = attrs.string(), value = attrs.string(), sorted = False), default = None),
    },
)
