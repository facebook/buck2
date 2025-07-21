# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

def _no_toolchain(_ctx: AnalysisContext) -> list[Provider]:
    return [DefaultInfo()]

# no_toolchain doesn't return any *ToolchainInfo providers
# it's useful when a toolchain is optional, e.q. CxxToolchain for Python and Go rules.
no_toolchain = rule(
    impl = _no_toolchain,
    attrs = {},
    is_toolchain_rule = True,
)
