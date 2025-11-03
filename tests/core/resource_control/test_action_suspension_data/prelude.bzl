# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

def _merge_outputs_impl(ctx) -> list[Provider]:
    outputs = []
    for i in range(1, ctx.attrs.num_actions):
        output = ctx.actions.declare_output("output{}.txt".format(i))
        outputs.append(output)

        sh_cmd = 'head -c 10 /dev/urandom > "$1"'
        if ctx.attrs.sleep > 0:
            sh_cmd = "sleep {} && {}".format(ctx.attrs.sleep, sh_cmd)

        ctx.actions.run(["sh", "-c", sh_cmd, "--", output.as_output()], category = "my_action{}".format(i))

    merged = ctx.actions.declare_output("output.txt")
    cmd = cmd_args("cat", outputs, ">", merged.as_output(), delimiter = " ")
    ctx.actions.run(["sh", "-c", cmd], category = "merge")

    return [
        DefaultInfo(merged),
    ]

merge_outputs = rule(
    impl = _merge_outputs_impl,
    attrs = {
        "num_actions": attrs.int(default = 100),
        "sleep": attrs.int(default = 0),
    },
)

_use_some_memory = read_root_config("use_some_memory", "path")

def _freeze_unfreeze_impl(ctx) -> list[Provider]:
    output0 = ctx.actions.declare_output("output0.txt")
    cmd0 = cmd_args([_use_some_memory, "--allocate-count", "100", "--each-tick-allocate-memory", "2", "--tick-duration", "0.1", "--output", output0.as_output()])

    # this action will be frozen
    ctx.actions.run(cmd0, category = "freeze_unfreeze", identifier = "action_to_be_frozen")

    output1 = ctx.actions.declare_output("output1.txt")

    # this action will not be frozen
    cmd1 = cmd_args([_use_some_memory, "--allocate-count", "40", "--each-tick-allocate-memory", "1", "--tick-duration", "0.1", "--output", output1.as_output()])
    ctx.actions.run(cmd1, category = "freeze_unfreeze", identifier = "small_action")

    final_output = ctx.actions.declare_output("final_output.txt")

    combine_output_script = """
    (cat "$1"; echo "========================"; cat "$2") > "$3"
    """
    ctx.actions.run(["sh", "-c", combine_output_script, "--", output0, output1, final_output.as_output()], category = "merge")

    return [
        DefaultInfo(final_output),
    ]

freeze_unfreeze = rule(
    impl = _freeze_unfreeze_impl,
    attrs = {},
)
