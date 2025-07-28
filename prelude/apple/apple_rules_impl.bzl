# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

# @oss-disable[end= ]: load("@prelude//apple/meta_only:meta_only_rules.bzl", "meta_only_apple_rule_attributes", "meta_only_apple_rule_implementations")
load(
    ":apple_rules_impl_utility.bzl",
    "apple_bundle_extra_attrs",
)

implemented_rules = {
# @oss-disable[end= ]: } | meta_only_apple_rule_implementations()
} # @oss-enable

extra_attributes = {
    "apple_bundle": apple_bundle_extra_attrs(),
# @oss-disable[end= ]: } | meta_only_apple_rule_attributes()
} # @oss-enable
