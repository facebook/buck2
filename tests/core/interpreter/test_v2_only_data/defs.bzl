# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

def _impl(ctx):
    _ignore = ctx  # buildifier: disable=unused-variable
    return [DefaultInfo()]

# This bzl file cannot be interpreted with Buck1 because there's no `rule` builtin.
my_rule = rule(impl = _impl, attrs = {})
