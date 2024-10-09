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

# Unordered pairs where neither crate may depend on the other
BANNED_DEP_PATHS = [
    (_app("buck2_common"), _app("buck2_directory")),
    (_app("buck2_common"), "//buck2/starlark-rust/starlark:starlark"),
    (_app("buck2_build_api"), _app("buck2_execute_impl")),
    (_app("buck2_build_api"), _app("buck2_interpreter_for_build")),
    (_app("buck2_server"), _app("buck2_server_commands")),
    (_app("buck2_bxl"), _app("buck2_configured")),
]
