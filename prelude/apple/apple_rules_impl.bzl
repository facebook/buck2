# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

load("@prelude//:attrs_validators.bzl", "validation_common")
load(
    "@prelude//:validation_deps.bzl",
    "VALIDATION_DEPS_ATTR_NAME",
    "VALIDATION_DEPS_ATTR_TYPE",
)
load("@prelude//apple:apple_common.bzl", "apple_common")
# @oss-disable[end= ]: load("@prelude//apple/meta_only:meta_only_rules.bzl", "meta_only_apple_rule_attributes", "meta_only_apple_rule_implementations")
load("@prelude//apple/swift:swift_incremental_support.bzl", "SwiftCompilationMode")
load("@prelude//cxx:headers.bzl", "CPrecompiledHeaderInfo", "HeaderMode")
load("@prelude//cxx:link_groups_types.bzl", "LINK_GROUP_MAP_ATTR")
load("@prelude//linking:execution_preference.bzl", "link_execution_preference_attr")
load("@prelude//linking:link_info.bzl", "LinkOrdering")
load("@prelude//linking:types.bzl", "Linkage")
load(":apple_library.bzl", "AppleSharedLibraryMachOFileType")
load(
    ":apple_rules_impl_utility.bzl",
    "APPLE_ARCHIVE_OBJECTS_LOCALLY_OVERRIDE_ATTR_NAME",
    "apple_bundle_extra_attrs",
    "apple_dsymutil_attrs",
    "get_apple_toolchain_attr",
    "get_apple_xctoolchain_attr",
    "get_apple_xctoolchain_bundle_id_attr",
    "get_enable_library_evolution",
    "get_skip_swift_incremental_outputs_attrs",
    "get_swift_incremental_file_hashing_attrs",
)

implemented_rules = {
# @oss-disable[end= ]: } | meta_only_apple_rule_implementations()
} # @oss-enable

_APPLE_TOOLCHAIN_ATTR = get_apple_toolchain_attr()

def _apple_library_extra_attrs():
    attribs = {
        "dist_thin_lto_codegen_flags": attrs.list(attrs.arg(), default = []),
        "enable_distributed_thinlto": attrs.bool(default = select({
            "DEFAULT": False,
            "config//build_mode/constraints:distributed-thin-lto-enabled": True,
        })),
        "enable_library_evolution": attrs.option(attrs.bool(), default = None),
        "header_mode": attrs.option(attrs.enum(HeaderMode.values()), default = None),
        "link_execution_preference": link_execution_preference_attr(),
        "link_group_map": LINK_GROUP_MAP_ATTR,
        "link_ordering": attrs.option(attrs.enum(LinkOrdering.values()), default = None),
        "precompiled_header": attrs.option(attrs.dep(providers = [CPrecompiledHeaderInfo]), default = None),
        "preferred_linkage": attrs.enum(Linkage.values(), default = "any"),
        "propagated_target_sdk_version": attrs.option(attrs.string(), default = None),
        # Mach-O file type for binary when the target is built as a shared library.
        "shared_library_macho_file_type": attrs.enum(AppleSharedLibraryMachOFileType.values(), default = "dylib"),
        "stripped": attrs.option(attrs.bool(), default = None),
        "supports_header_symlink_subtarget": attrs.bool(default = False),
        "supports_shlib_interfaces": attrs.bool(default = True),
        "swift_compilation_mode": attrs.enum(SwiftCompilationMode.values(), default = "wmo"),
        "swift_package_name": attrs.option(attrs.string(), default = None),
        "use_archive": attrs.option(attrs.bool(), default = None),
        "_apple_toolchain": _APPLE_TOOLCHAIN_ATTR,
        "_apple_xctoolchain": get_apple_xctoolchain_attr(),
        "_apple_xctoolchain_bundle_id": get_apple_xctoolchain_bundle_id_attr(),
        "_enable_library_evolution": get_enable_library_evolution(),
        "_stripped_default": attrs.bool(default = False),
        "_swift_enable_testing": attrs.bool(default = select({
            "DEFAULT": False,
            "config//features/apple:swift_enable_testing_enabled": True,
        })),
        APPLE_ARCHIVE_OBJECTS_LOCALLY_OVERRIDE_ATTR_NAME: attrs.option(attrs.bool(), default = None),
        VALIDATION_DEPS_ATTR_NAME: VALIDATION_DEPS_ATTR_TYPE,
    } | validation_common.attrs_validators_arg()
    attribs.update(apple_common.apple_tools_arg())
    attribs.update(apple_dsymutil_attrs())
    attribs.update(get_swift_incremental_file_hashing_attrs())
    attribs.update(get_skip_swift_incremental_outputs_attrs())
    return attribs

extra_attributes = {
    "apple_bundle": apple_bundle_extra_attrs(),
    "apple_library": _apple_library_extra_attrs(),
# @oss-disable[end= ]: } | meta_only_apple_rule_attributes()
} # @oss-enable
