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
