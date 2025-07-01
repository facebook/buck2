# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

load("@fbsource//tools/build_defs/android/action_error_handler:d8_error_handler.bzl", "d8_error_handler")
load("@fbsource//tools/build_defs/android/action_error_handler:redex_error_handler.bzl", "redex_error_handler")

def android_error_handler(ctx: ActionErrorCtx) -> list[ActionSubError]:
    categories = []

    categories += d8_error_handler(ctx)
    categories += redex_error_handler(ctx)

    return categories
