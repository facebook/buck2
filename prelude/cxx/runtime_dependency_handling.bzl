# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

# Additional behavior for how to handle runtime dependencies
RuntimeDependencyHandling = enum(
    # Do no additional handling
    "none",
    # Always include runtime dependencies in a symlink tree, regardless
    # of whether shared linkage is used or not. Only include first level deps.
    "symlink_single_level_only",
    # Always include runtime dependencies in a symlink tree, regardless
    # of whether shared linkage is used or not. Include transitive deps.
    "symlink",
)

def cxx_attr_runtime_dependency_handling(ctx: AnalysisContext) -> RuntimeDependencyHandling:
    return RuntimeDependencyHandling(ctx.attrs.runtime_dependency_handling or "none")
