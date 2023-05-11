# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

LinkExecutionPreferenceTypes = [
    "any",
    "full_hybrid",
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
    return attrs.option(attrs.one_of(attrs.enum(LinkExecutionPreferenceTypes), attrs.dep(providers = [LinkExecutionPreferenceDeterminatorInfo])), default = None, doc = """
    The execution preference for linking. Options are:\n
        - any : No preference is set, and the link action will be performed based on buck2's executor configuration.\n
        - full_hybrid : The link action will execute both locally and remotely, regardless of buck2's executor configuration (if\n
                        the executor is capable of hybrid execution). The use_limited_hybrid setting of the hybrid executor is ignored.\n
        - local : The link action will execute locally if compatible on current host platform.\n
        - local_only : The link action will execute locally, and error if the current platform is not compatible.\n
        - remote : The link action will execute remotely if a compatible remote platform exists, otherwise locally.\n

    The default is None, expressing that no preference has been set on the target itself.
    """)

def cxx_attr_link_execution_preference(ctx) -> [LinkExecutionPreference.type, None]:
    return LinkExecutionPreference(ctx.attrs.link_execution_preference) if ctx.attrs.link_execution_preference else None
