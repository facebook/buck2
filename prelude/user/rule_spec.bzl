# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

RuleRegistrationSpec = record(
    name = field(str),
    impl = field(typing.Callable),
    attrs = field(dict[str, Attr]),
    # TODO(nga): should be `transition | None`, but `transition` does not work as type.
    cfg = field(typing.Any | None, None),
    is_toolchain_rule = field(bool, False),
    doc = field(str, ""),
)
