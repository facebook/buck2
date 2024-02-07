# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

Buck2Compatibility = enum(
    "unknown",  # No warnings or failures, default state
    "compatible",  # Compatible with Buck2, Buck1 will show warning to migrate to Buck2
    "incompatible",  # Incompatible with Buck2, Buck2 will show warning about correctness of result
    "required",  # Buck2 required, Buck1 will fail the build
)

BUCK2_COMPATIBILITY_ATTRIB_NAME = "buck2_compatibility"
BUCK2_COMPATIBILITY_ATTRIB_TYPE = attrs.enum(Buck2Compatibility.values(), default = "unknown")

def check_buck2_compatibility(ctx: AnalysisContext):
    if hasattr(ctx.attrs, "buck2_compatibility") and ctx.attrs.buck2_compatibility == "incompatible":
        warning("The target '{}' is marked as incompatible with buck2, output might be incorrect".format(ctx.label))
