# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

# Avoid some copy-paste
def _app(s):
    return "//buck2/app/" + s + ":" + s

# These crates should only implement late bindings and not be depended on
# directly
LATE_BINDING_ONLY_CRATES = [
    _app("buck2_anon_target"),
    _app("buck2_audit_server"),
    _app("buck2_bxl"),
    _app("buck2_query_impls"),
]
