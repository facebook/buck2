# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

# @oss-disable: load("@fbcode_macros//build_defs:platform_utils.bzl", "platform_utils") 

platform_utils = None # @oss-enable

def _dtp():
    return platform_utils.get_cxx_platform_for_base_path(package_name()).target_platform if platform_utils else None

def _impl(ctx: AnalysisContext) -> list[Provider]:
    path = ctx.attrs.client_to_re_path
    if len(path) != 0:
        m = "Buck2 client binary may not have a dependency on `fbcode//remote_execution/`!"
        m += "\nDependency path:"
        m += "".join(["\n" + str(t) for t in path])
        fail(m)
    return [DefaultInfo()]

_test_buck2_dep_graph = rule(
    impl = _impl,
    attrs = {
        "client_to_re_path": attrs.query(),
    },
)

_CLIENT_BIN = "fbcode//buck2/app/buck2:buck2_client-bin"
_RE_CLIENT_TARGET = "//remote_execution/client_lib/wrappers/rust:re_client_lib"

_CLIENT_TO_RE = "somepath({}, filter(fbcode//remote_execution/, deps({})) + {})".format(_CLIENT_BIN, _CLIENT_BIN, _RE_CLIENT_TARGET)

def test_buck2_dep_graph(name):
    _test_buck2_dep_graph(
        name = name,
        client_to_re_path = _CLIENT_TO_RE,
        default_target_platform = _dtp(),
    )
