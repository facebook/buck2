# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

BUCK2_LLVM_IR_PGO_PROFILES = {
    # Reserve the family before capture so Rust symbol names match profile use.
    # Replace None with the exported merged profile target before enabling use.
    "buck2-td": None,
}
