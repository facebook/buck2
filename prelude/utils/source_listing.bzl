# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

load(":source_listing_impl.bzl", "SourceListingInfoAlias", "empty_source_listing_impl", "source_listing_impl")

SourceListingInfo = SourceListingInfoAlias

def source_listing(exclude = None):
    source_listing_impl(exclude = exclude or [])

def empty_source_listing():
    empty_source_listing_impl()
