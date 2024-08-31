# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

def _action_impl(ctx):
    out = ctx.actions.declare_output("out")

    ctx.actions.run(
        [
            "python3",
            "-c",
            "import sys; open(sys.argv[1], 'w').write(sys.argv[2])",
            out.as_output(),
            ctx.attrs.seed,
        ],
        local_only = ctx.attrs.local_only,
        category = "write",
    )

    return [DefaultInfo(default_output = out)]

action = rule(
    attrs = {
        "local_only": attrs.bool(),
        "seed": attrs.string(),
    },
    impl = _action_impl,
)

def _symlinked_dir_impl(ctx):
    f = ctx.actions.write("dst/f", "file")
    out = ctx.actions.symlinked_dir("out", {ctx.attrs.seed: f})
    return [DefaultInfo(default_output = out)]

symlinked_dir = rule(
    attrs = {
        "seed": attrs.string(),
    },
    impl = _symlinked_dir_impl,
)

def _write_impl(ctx):
    out = ctx.actions.write("out", ctx.attrs.seed)
    return [DefaultInfo(default_output = out)]

write = rule(
    attrs = {
        "seed": attrs.string(),
    },
    impl = _write_impl,
)

def _copy_impl(ctx):
    f = ctx.actions.write("dst/f", ctx.attrs.seed)
    out = ctx.actions.copy_file("out", f)
    return [DefaultInfo(default_output = out)]

copy = rule(
    attrs = {
        "seed": attrs.string(),
    },
    impl = _copy_impl,
)

def declare_targets():
    target = read_config("test", "main")

    if target == "local_action-a":
        action(name = "main", local_only = True, seed = "local-action-a")
    elif target == "local_action-b":
        action(name = "main", local_only = True, seed = "local-action-b")
    elif target == "remote_action-a":
        action(name = "main", local_only = False, seed = "remote-action-a")
    elif target == "remote_action-b":
        action(name = "main", local_only = False, seed = "remote-action-b")
    elif target == "symlinked_dir-a":
        symlinked_dir(name = "main", seed = "symlinked_dir-a")
    elif target == "symlinked_dir-b":
        symlinked_dir(name = "main", seed = "symlinked_dir-b")
    elif target == "write-a":
        write(name = "main", seed = "write-a")
    elif target == "write-b":
        write(name = "main", seed = "write-b")
    elif target == "copy-a":
        copy(name = "main", seed = "copy-a")
    elif target == "copy-b":
        copy(name = "main", seed = "copy-b")
    else:
        fail("Invalid target: `{}`".format(target))
