# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

def _classpath_with_content_based_path_impl(ctx):
    out = ctx.actions.declare_output("out.txt", uses_experimental_content_based_path_hashing = True)
    out = ctx.actions.write(out, "out")
    return [DefaultInfo(default_output = out), TemplatePlaceholderInfo(keyed_variables = {"classpath": cmd_args(out)})]

classpath_with_content_based_path = rule(
    impl = _classpath_with_content_based_path_impl,
    attrs = {},
)
