# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

def anon_targets(ctx: "context", xs, k):
    """A helper function that recursively wraps a list of deps into promises"""

    def f(xs, ps, k):
        if len(xs) == 0:
            return k(ps)
        else:
            return ctx.actions.anon_target(xs[0][0], xs[0][1]).map(lambda p: f(xs[1:], ps + [p], k))

    return f(xs, [], k)
