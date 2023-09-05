# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

# @starlark-rust: allow_string_literals_in_type_expr

# Utility functions used by "fdb.bxl"

load("@prelude//debugging/types.bzl", "TargetInfo")

def target_name(node: "target_node") -> str:
    return "{}:{}".format(str(node.label.path), node.label.name)

def rule_type(node: "target_node") -> str:
    return node.rule_type

def create_target_info(target: "target_node") -> TargetInfo:
    attrs = target.attrs_lazy()
    return TargetInfo(
        target = target_name(target),
        target_type = rule_type(target),
        labels = attrs.get("labels").value() if attrs.get("labels") != None else [],
    )

def resolve_actual_target(ctx: "bxl_ctx", target: "target_node") -> "target_node":
    attrs = target.attrs_lazy()

    if rule_type(target) in ["prelude//rules.bzl:alias", "prelude//rules.bzl:configured_alias", "prelude//debugging/target_ref.bzl:target_ref"]:
        actual = attrs.get("actual")
        if actual != None:
            return resolve_actual_target(ctx, ctx.configured_targets(actual.value().configured_target()))

    return target
