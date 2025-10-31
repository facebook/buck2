# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

load(
    ":erlang_info.bzl",
    "ErlangErrorHandlers",
)

# erlc error format:
#   The erlc compiler error format is as following:
#     <file>:<line>:<column>: <message>

def erlang_erlc_action_error_handler(ctx: ActionErrorCtx) -> list[ActionSubError]:
    structured_errors = ctx.parse_with_errorformat(
        category = "erlang_compile_error",
        error = ctx.stderr,
        errorformats = [
            "%f:%l:%c:\\ %m",
        ],
    )

    return structured_errors

erlang_action_error_handler = ErlangErrorHandlers(
    erlc = erlang_erlc_action_error_handler,
)
