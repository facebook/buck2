# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

test_within_view_rule = rule(
    impl = lambda ctx: fail("we don't run analysis in this test, {}".format(ctx)),
    attrs = {
        "deps": attrs.list(attrs.dep()),
    },
)
