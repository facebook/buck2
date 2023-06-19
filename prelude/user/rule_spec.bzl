# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

RuleRegistrationSpec = record(
    name = field(str.type),
    impl = field("function"),
    attrs = field({str.type: "attribute"}),
    cfg = field([None, "transition"], None),
    is_toolchain_rule = field(bool.type, False),
    doc = field(str.type, ""),
)
