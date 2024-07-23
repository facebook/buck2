# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

def _version_is_greater(left: str, right: str) -> bool:
    # Assumes version strings are in dotted format 1.2.4.
    # After comparing components the longer remainder is
    # considered larger.
    left_components = left.split(".")
    right_components = right.split(".")
    for pair in zip(left_components, right_components):
        x = int(pair[0])
        y = int(pair[1])
        if x < y:
            return False
        elif x > y:
            return True

    return len(left_components) > len(right_components)

def get_toolchain_target_sdk_version(ctx: AnalysisContext) -> [None, str]:
    min_version = ctx.attrs.min_sdk_version
    target_version = ctx.attrs.target_sdk_version
    if min_version == None and target_version == None:
        return None
    elif min_version != None and target_version == None:
        return min_version
    elif min_version == None and target_version != None:
        fail("Cannot set target_sdk_version without min_sdk_version")
    elif _version_is_greater(min_version, target_version):
        warning("Target SDK version {} is less than minimum supported version {}".format(target_version, min_version))
        return min_version
    else:
        return target_version
