# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

LinkExecutionPreferenceTypes = [
    "any",
    "local",
    "local_only",
    "remote",
]

LinkExecutionPreference = enum(*LinkExecutionPreferenceTypes)

LinkExecutionPreferenceDeterminatorInfo = provider(fields = [
    "preference_for_links",  # function that takes a list of target labels and returns a LinkExecutionPreference
])

def link_execution_preference_attr():
    # The attribute is optional, allowing for None to represent that no preference has been set and we should fallback on the toolchain.
    return attrs.option(attrs.one_of(attrs.enum(LinkExecutionPreferenceTypes), attrs.dep(providers = [LinkExecutionPreferenceDeterminatorInfo])), default = None)

def cxx_attr_link_execution_preference(ctx) -> [LinkExecutionPreference.type, None]:
    return LinkExecutionPreference(ctx.attrs.link_execution_preference) if ctx.attrs.link_execution_preference else None
