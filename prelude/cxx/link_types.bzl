# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

load("@prelude//linking:execution_preference.bzl", "LinkExecutionPreference")
load(
    "@prelude//linking:link_info.bzl",
    "LinkArgs",
    "LinkOrdering",
)

CxxLinkResultType = enum(
    "executable",
    "shared_library",
)

LinkOptions = record(
    links = list[LinkArgs.type],
    link_execution_preference = LinkExecutionPreference.type,
    link_weight = int,
    link_ordering = [LinkOrdering.type, None],
    enable_distributed_thinlto = bool,
    # A category suffix that will be added to the category of the link action that is generated.
    category_suffix = [str, None],
    # An identifier that will uniquely name this link action in the context of a category. Useful for
    # differentiating multiple link actions in the same rule.
    identifier = [str, None],
    strip = bool,
    # A function/lambda which will generate the strip args using the ctx.
    strip_args_factory = ["function", None],
    import_library = [Artifact, None],
    allow_cache_upload = bool,
    # Force callers to use link_options() or merge_link_options() to create.
    __private_use_link_options_function_to_construct = None,
)

def link_options(
        links: list[LinkArgs.type],
        link_execution_preference: LinkExecutionPreference.type,
        link_weight: int = 1,
        link_ordering: [LinkOrdering.type, None] = None,
        enable_distributed_thinlto: bool = False,
        category_suffix: [str, None] = None,
        identifier: [str, None] = None,
        strip: bool = False,
        strip_args_factory = None,
        import_library: [Artifact, None] = None,
        allow_cache_upload: bool = False) -> LinkOptions:
    """
    A type-checked constructor for LinkOptions because by default record
    constructors aren't typed.
    """
    return LinkOptions(
        links = links,
        link_execution_preference = link_execution_preference,
        link_weight = link_weight,
        link_ordering = link_ordering,
        enable_distributed_thinlto = enable_distributed_thinlto,
        category_suffix = category_suffix,
        identifier = identifier,
        strip = strip,
        strip_args_factory = strip_args_factory,
        import_library = import_library,
        allow_cache_upload = allow_cache_upload,
        __private_use_link_options_function_to_construct = None,
    )

# A marker instance to differentiate explicitly-passed None and a field tha
# isn't provided in merge_link_options.
_NotProvided = record()
_NOT_PROVIDED = _NotProvided()

def merge_link_options(
        base: LinkOptions,
        links: [list[LinkArgs.type], _NotProvided] = _NOT_PROVIDED,
        link_execution_preference: [LinkExecutionPreference.type, _NotProvided] = _NOT_PROVIDED,
        link_weight: [int, _NotProvided] = _NOT_PROVIDED,
        link_ordering: [LinkOrdering.type, None, _NotProvided] = _NOT_PROVIDED,
        enable_distributed_thinlto: [bool, _NotProvided] = _NOT_PROVIDED,
        category_suffix: [str, None, _NotProvided] = _NOT_PROVIDED,
        identifier: [str, None, _NotProvided] = _NOT_PROVIDED,
        strip: [bool, _NotProvided] = _NOT_PROVIDED,
        strip_args_factory = _NOT_PROVIDED,
        import_library: [Artifact, None, _NotProvided] = _NOT_PROVIDED,
        allow_cache_upload: [bool, _NotProvided] = _NOT_PROVIDED) -> LinkOptions:
    """
    Also something we would ideally auto-generate as LinkOptions.merge in
    Starlark.
    """

    return LinkOptions(
        links = base.links if links == _NOT_PROVIDED else links,
        link_execution_preference = base.link_execution_preference if link_execution_preference == _NOT_PROVIDED else link_execution_preference,
        link_weight = base.link_weight if link_weight == _NOT_PROVIDED else link_weight,
        link_ordering = base.link_ordering if link_ordering == _NOT_PROVIDED else link_ordering,
        enable_distributed_thinlto = base.enable_distributed_thinlto if enable_distributed_thinlto == _NOT_PROVIDED else enable_distributed_thinlto,
        category_suffix = base.category_suffix if category_suffix == _NOT_PROVIDED else category_suffix,
        identifier = base.identifier if identifier == _NOT_PROVIDED else identifier,
        strip = base.strip if strip == _NOT_PROVIDED else strip,
        strip_args_factory = base.strip_args_factory if strip_args_factory == _NOT_PROVIDED else strip_args_factory,
        import_library = base.import_library if import_library == _NOT_PROVIDED else import_library,
        allow_cache_upload = base.allow_cache_upload if allow_cache_upload == _NOT_PROVIDED else allow_cache_upload,
        __private_use_link_options_function_to_construct = None,
    )
