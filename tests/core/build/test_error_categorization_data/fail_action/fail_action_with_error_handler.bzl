# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

def _error_handler_produced_error_categories(ctx):
    def error_handler(ctx: ActionErrorCtx) -> list[ActionSubError]:
        categories = []

        categories.append(ctx.new_sub_error(
            category = "FirstError",
        ))

        categories.append(ctx.new_sub_error(
            category = "SecondError",
        ))

        return categories

    out = ctx.actions.declare_output(ctx.attrs.name)

    # error handler is invoked but won't catch anything
    ctx.actions.run(
        cmd_args("false", hidden = out.as_output()),
        category = ctx.attrs.name,
        error_handler = error_handler,
    )

    return [DefaultInfo(default_outputs = [out])]

error_handler_produced_error_categories = rule(
    impl = _error_handler_produced_error_categories,
    attrs = {
    },
)
