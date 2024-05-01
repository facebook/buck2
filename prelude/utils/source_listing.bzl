# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

load("@prelude//:is_buck2.bzl", "is_buck2")
load(":source_listing_impl.bzl?v2_only", "SourceListingInfoAlias", "source_listing_impl")

SourceListingInfo = SourceListingInfoAlias

def source_listing():
    if is_buck2():
        source_listing_impl()
