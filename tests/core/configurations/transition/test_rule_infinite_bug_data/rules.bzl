# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

load(":tr.bzl", "transition_increase_label_len")

def _impl(ctx):
    _ = ctx  # buildifier: disable=unused-variable
    fail("Don't care")

my_rule = rule(impl = _impl, attrs = {}, cfg = transition_increase_label_len)
