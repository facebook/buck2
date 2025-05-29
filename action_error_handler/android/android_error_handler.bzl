# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

load("@fbsource//tools/build_defs/android/action_error_handler:d8_error_handler.bzl", "d8_error_handler")
load("@fbsource//tools/build_defs/android/action_error_handler:redex_error_handler.bzl", "redex_error_handler")

def android_error_handler(ctx: ActionErrorCtx) -> list[ActionSubError]:
    categories = []

    categories += d8_error_handler(ctx)
    categories += redex_error_handler(ctx)

    return categories
