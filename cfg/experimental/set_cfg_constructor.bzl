# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

load(":alias.bzl?v2_only", "ALIASES")
load(":cfg_constructor.bzl?v2_only", "cfg_constructor_post_constraint_analysis", "cfg_constructor_pre_constraint_analysis")
load(":common.bzl?v2_only", "MODIFIER_METADATA_KEY")

def set_cfg_constructor():
    # This is to be buck1-proof.
    set_cfg_constructor_func = getattr(native, "set_cfg_constructor", None)
    if set_cfg_constructor_func:
        set_cfg_constructor_func(
            stage0 = cfg_constructor_pre_constraint_analysis,
            stage1 = cfg_constructor_post_constraint_analysis,
            key = MODIFIER_METADATA_KEY,
            aliases = ALIASES,
        )
