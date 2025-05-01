# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

def _test_cleanup_impl(ctx):
    out = ctx.actions.write("path/to/output", "")
    return [DefaultInfo(out)]

test_cleanup = rule(impl = _test_cleanup_impl, attrs = {})
