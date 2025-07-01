# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

def _rule_with_query(ctx):
    return [DefaultInfo()]

rule_with_query = rule(
    impl = _rule_with_query,
    attrs = {"query": attrs.query()},
)
