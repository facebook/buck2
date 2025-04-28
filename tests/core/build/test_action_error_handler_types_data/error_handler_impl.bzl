# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

def error_handler_impl(ctx: ActionErrorCtx) -> list[ActionSubError]:
    categories = []

    if "foo" in ctx.stdout:
        categories.append(ctx.new_sub_error(
            category = "foo_category",
            message = "foo message",
            locations = [
                ctx.new_error_location(file = "foo_file", line = 1),
            ],
        ))

    if "bar" in ctx.stderr:
        categories.append(ctx.new_sub_error(
            category = "bar_category",
            message = "bar message",
            locations = [
                ctx.new_error_location(file = "bar_file", line = 1),
            ],
        ))

    return categories

def _does_not_use_error_handler_impl(ctx: AnalysisContext):
    out = ctx.actions.declare_output("out")
    ctx.actions.run(
        cmd_args(["python3", "-c", "import sys\nsys.exit(1)"], hidden = out.as_output()),
        category = "test_failure",
        error_handler = error_handler_impl,
    )

    return [DefaultInfo(default_output = out)]

does_not_use_error_handler = rule(
    impl = _does_not_use_error_handler_impl,
    attrs = {},
)

def _error_handler_nonetype_impl(ctx: AnalysisContext):
    out = ctx.actions.declare_output("out")
    ctx.actions.run(
        cmd_args(["python3", "-c", "import sys; open(sys.argv[1], 'w').write('something')"], out.as_output()),
        category = "test_doesnt_fail",
        error_handler = None,
    )

    return [DefaultInfo(default_output = out)]

error_handler_nonetype_impl = rule(
    impl = _error_handler_nonetype_impl,
    attrs = {},
)
