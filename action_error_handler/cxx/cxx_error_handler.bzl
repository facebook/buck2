# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

load("@fbsource//tools/build_defs/cxx/action_error_handler:cxx_linker_error_handler.bzl", "cxx_linker_error_handler")

def cxx_error_handler(ctx: ActionErrorCtx) -> list[ActionSubError]:
    return cxx_linker_error_handler(ctx)
