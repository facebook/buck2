# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

load("@prelude//apple:apple_toolchain_types.bzl", "AppleToolchainInfo")
load("@prelude//apple/user:apple_selective_debugging.bzl", "AppleSelectiveDebuggingInfo", "filter_debug_info")
load("@prelude//cxx:debug.bzl", "maybe_external_debug_info", "project_external_debug_info")
load(
    "@prelude//ide_integrations:xcode.bzl",
    "XCODE_DATA_SUB_TARGET",
    "generate_xcode_data",
)
load("@prelude//utils:utils.bzl", "expect", "flatten", "is_any")
load(":apple_bundle_destination.bzl", "AppleBundleDestination")
load(":apple_bundle_part.bzl", "AppleBundlePart", "assemble_bundle", "bundle_output", "get_apple_bundle_part_relative_destination_path")
load(":apple_bundle_resources.bzl", "get_apple_bundle_resource_part_list", "get_is_watch_bundle")
load(":apple_bundle_types.bzl", "AppleBundleInfo", "AppleBundleLinkerMapInfo", "AppleBundleResourceInfo")
load(":apple_bundle_utility.bzl", "get_bundle_min_target_version", "get_product_name")
load(":apple_dsym.bzl", "AppleBundleDebuggableInfo", "AppleDebuggableInfo", "DEBUGINFO_SUBTARGET", "DSYM_INFO_SUBTARGET", "DSYM_SUBTARGET", "get_apple_dsym", "get_apple_dsym_info")
load(":apple_sdk.bzl", "get_apple_sdk_name")
load(":xcode.bzl", "apple_xcode_data_add_xctoolchain")

INSTALL_DATA_SUB_TARGET = "install-data"
_INSTALL_DATA_FILE_NAME = "install_apple_data.json"

_PLIST = "plist"

_XCTOOLCHAIN_SUB_TARGET = "xctoolchain"

AppleBundlePartListConstructorParams = record(
    # The binaries/executables, required to create a bundle
    binaries = field([AppleBundlePart.type]),
)

AppleBundlePartListOutput = record(
    # The parts to be copied into an Apple bundle, *including* binaries
    parts = field([AppleBundlePart.type]),
    # Part that holds the info.plist
    info_plist_part = field(AppleBundlePart.type),
)

AppleBundleBinaryOutput = record(
    binary = field("artifact"),
    debuggable_info = field([AppleDebuggableInfo.type, None], None),
    # In the case of watchkit, the `ctx.attrs.binary`'s not set, and we need to create a stub binary.
    is_watchkit_stub_binary = field(bool.type, False),
)

def _get_binary(ctx: "context") -> AppleBundleBinaryOutput.type:
    # No binary means we are building watchOS bundle. In v1 bundle binary is present, but its sources are empty.
    if ctx.attrs.binary == None:
        return AppleBundleBinaryOutput(
            binary = _get_watch_kit_stub_artifact(ctx),
            is_watchkit_stub_binary = True,
        )

    binary_dep = ctx.attrs.binary
    if len(binary_dep[DefaultInfo].default_outputs) != 1:
        fail("Expected single output artifact. Make sure the implementation of rule from `binary` attribute is correct.")

    return _maybe_scrub_binary(ctx, binary_dep)

def _maybe_scrub_binary(ctx, binary_dep: "dependency") -> AppleBundleBinaryOutput.type:
    binary = binary_dep[DefaultInfo].default_outputs[0]
    debuggable_info = binary_dep.get(AppleDebuggableInfo)
    if ctx.attrs.selective_debugging == None:
        return AppleBundleBinaryOutput(binary = binary, debuggable_info = debuggable_info)

    selective_debugging_info = ctx.attrs.selective_debugging[AppleSelectiveDebuggingInfo]
    binary = selective_debugging_info.scrub_binary(ctx, binary)

    if not debuggable_info:
        return AppleBundleBinaryOutput(binary = binary)

    # If we have debuggable info for this binary, create the scrubed dsym for the binary
    # and filter debug info.
    external_debug_info = debuggable_info.external_debug_info
    dsym_artifact = _get_scrubbed_binary_dsym(ctx, binary, external_debug_info)

    all_debug_info = external_debug_info.traverse()
    filtered_debug_info = filter_debug_info(all_debug_info, selective_debugging_info)
    filtered_external_debug_info = maybe_external_debug_info(
        actions = ctx.actions,
        label = ctx.label,
        artifacts = filtered_debug_info,
    )
    debuggable_info = AppleDebuggableInfo(dsyms = [dsym_artifact], external_debug_info = filtered_external_debug_info)

    return AppleBundleBinaryOutput(binary = binary, debuggable_info = debuggable_info)

def _get_scrubbed_binary_dsym(ctx, binary: "artifact", external_debug_info: "transitive_set") -> "artifact":
    external_debug_info_args = project_external_debug_info(
        actions = ctx.actions,
        label = ctx.label,
        infos = [external_debug_info],
    )
    dsym_artifact = get_apple_dsym(
        ctx = ctx,
        executable = binary,
        external_debug_info = external_debug_info_args,
        action_identifier = binary.short_path,
    )
    return dsym_artifact

def _get_binary_bundle_parts(ctx: "context", binary_output: AppleBundleBinaryOutput.type) -> ([AppleBundlePart.type], AppleBundlePart.type):
    """Returns a tuple of all binary bundle parts and the primary bundle binary."""
    result = []

    if binary_output.is_watchkit_stub_binary:
        # If we're using a stub binary from watchkit, we also need to add extra part for stub.
        result.append(AppleBundlePart(source = binary_output.binary, destination = AppleBundleDestination("watchkitstub"), new_name = "WK"))
    primary_binary_part = AppleBundlePart(source = binary_output.binary, destination = AppleBundleDestination("executables"), new_name = get_product_name(ctx))
    result.append(primary_binary_part)
    return result, primary_binary_part

def _get_watch_kit_stub_artifact(ctx: "context") -> "artifact":
    expect(ctx.attrs.binary == None, "Stub is useful only when binary is not set which means watchOS bundle is built.")
    stub_binary = ctx.attrs._apple_toolchain[AppleToolchainInfo].watch_kit_stub_binary
    if stub_binary == None:
        fail("Expected Watch Kit stub binary to be provided when bundle binary is not set.")
    return stub_binary

def _apple_bundle_run_validity_checks(ctx: "context"):
    if ctx.attrs.extension == None:
        fail("`extension` attribute is required")

def _get_debuggable_deps(ctx: "context", binary_output: AppleBundleBinaryOutput.type) -> AppleBundleDebuggableInfo.type:
    # `label` captures configuration as well, so it's safe to use for comparison purposes
    binary_label = getattr(ctx.attrs.binary, "label", None)
    deps_debuggable_infos = filter(
        None,
        # It's allowed for `ctx.attrs.binary` to appear in `ctx.attrs.deps` as well,
        # in this case, do not duplicate the debugging info for the binary coming from two paths.
        [dep.get(AppleDebuggableInfo) for dep in ctx.attrs.deps if dep.label != binary_label],
    )

    # We don't care to process the watchkit stub binary.
    binary_debuggable_info = None
    if not binary_output.is_watchkit_stub_binary:
        binary_debuggable_info = binary_output.debuggable_info

    return AppleBundleDebuggableInfo(
        binary_info = binary_debuggable_info,
        dep_infos = deps_debuggable_infos,
        all_infos = deps_debuggable_infos + ([binary_debuggable_info] if binary_debuggable_info else []),
    )

def get_apple_bundle_part_list(ctx: "context", params: AppleBundlePartListConstructorParams.type) -> AppleBundlePartListOutput.type:
    resource_part_list = None
    if hasattr(ctx.attrs, "_resource_bundle") and ctx.attrs._resource_bundle != None:
        resource_info = ctx.attrs._resource_bundle[AppleBundleResourceInfo]
        if resource_info != None:
            resource_part_list = resource_info.resource_output

    if resource_part_list == None:
        resource_part_list = get_apple_bundle_resource_part_list(ctx)

    return AppleBundlePartListOutput(
        parts = resource_part_list.resource_parts + params.binaries,
        info_plist_part = resource_part_list.info_plist_part,
    )

def apple_bundle_impl(ctx: "context") -> ["provider"]:
    _apple_bundle_run_validity_checks(ctx)

    binary_outputs = _get_binary(ctx)
    all_binary_parts, primary_binary_part = _get_binary_bundle_parts(ctx, binary_outputs)
    apple_bundle_part_list_output = get_apple_bundle_part_list(ctx, AppleBundlePartListConstructorParams(binaries = all_binary_parts))

    sub_targets = {}

    linker_maps_directory, linker_map_info = _linker_maps_data(ctx)
    sub_targets["linker-maps"] = [DefaultInfo(default_output = linker_maps_directory)]

    bundle_debuggable_info = _get_debuggable_deps(ctx, binary_outputs)

    binary_dsym_artifacts = getattr(bundle_debuggable_info.binary_info, "dsyms", [])
    dep_dsym_artifacts = flatten([info.dsyms for info in bundle_debuggable_info.dep_infos])
    dsym_artifacts = binary_dsym_artifacts + dep_dsym_artifacts
    if dsym_artifacts:
        sub_targets[DSYM_SUBTARGET] = [DefaultInfo(default_outputs = dsym_artifacts)]

    external_debug_info = maybe_external_debug_info(
        actions = ctx.actions,
        label = ctx.label,
        children = [info.external_debug_info for info in bundle_debuggable_info.all_infos],
    )
    sub_targets[DEBUGINFO_SUBTARGET] = [
        DefaultInfo(
            other_outputs = project_external_debug_info(
                actions = ctx.actions,
                label = ctx.label,
                infos = [external_debug_info],
            ),
        ),
    ]

    dsym_info = get_apple_dsym_info(ctx, binary_dsyms = binary_dsym_artifacts, dep_dsyms = dep_dsym_artifacts)
    sub_targets[DSYM_INFO_SUBTARGET] = [
        DefaultInfo(default_output = dsym_info, other_outputs = dsym_artifacts),
    ]

    bundle = bundle_output(ctx)

    assemble_bundle(ctx, bundle, apple_bundle_part_list_output.parts, apple_bundle_part_list_output.info_plist_part)

    sub_targets[_PLIST] = [DefaultInfo(default_output = apple_bundle_part_list_output.info_plist_part.source)]

    sub_targets[_XCTOOLCHAIN_SUB_TARGET] = ctx.attrs._apple_xctoolchain.providers

    # Define the xcode data sub target
    xcode_data_default_info, xcode_data_info = generate_xcode_data(ctx, "apple_bundle", bundle, _xcode_populate_attributes, processed_info_plist = apple_bundle_part_list_output.info_plist_part.source)
    sub_targets[XCODE_DATA_SUB_TARGET] = xcode_data_default_info
    install_data = generate_install_data(ctx)

    primary_binary_rel_path = get_apple_bundle_part_relative_destination_path(ctx, primary_binary_part)
    primary_binary_path = cmd_args([bundle, primary_binary_rel_path], delimiter = "/")
    run_cmd = cmd_args(primary_binary_path).hidden(bundle)

    return [
        DefaultInfo(default_output = bundle, sub_targets = sub_targets),
        AppleBundleInfo(
            bundle = bundle,
            binary_name = get_product_name(ctx),
            is_watchos = get_is_watch_bundle(ctx),
            contains_watchapp = is_any(lambda part: part.destination == AppleBundleDestination("watchapp"), apple_bundle_part_list_output.parts),
            skip_copying_swift_stdlib = ctx.attrs.skip_copying_swift_stdlib,
        ),
        AppleDebuggableInfo(dsyms = dsym_artifacts, external_debug_info = external_debug_info),
        InstallInfo(
            installer = ctx.attrs._apple_toolchain[AppleToolchainInfo].installer,
            files = {
                "app_bundle": bundle,
                "options": install_data,
            },
        ),
        RunInfo(args = run_cmd),
        linker_map_info,
        xcode_data_info,
    ]

def _xcode_populate_attributes(ctx, processed_info_plist: "artifact") -> {str.type: ""}:
    data = {
        "deployment_version": get_bundle_min_target_version(ctx),
        "info_plist": ctx.attrs.info_plist,
        "processed_info_plist": processed_info_plist,
        "product_name": get_product_name(ctx),
        "sdk": get_apple_sdk_name(ctx),
    }

    apple_xcode_data_add_xctoolchain(ctx, data)
    return data

def _linker_maps_data(ctx: "context") -> ("artifact", AppleBundleLinkerMapInfo.type):
    deps_with_binary = ctx.attrs.deps + ([ctx.attrs.binary] if ctx.attrs.binary != None else [])
    deps_linker_map_infos = filter(
        None,
        [dep.get(AppleBundleLinkerMapInfo) for dep in deps_with_binary],
    )
    deps_linker_maps = flatten([info.linker_maps for info in deps_linker_map_infos])
    all_maps = {map.basename: map for map in deps_linker_maps}
    directory = ctx.actions.copied_dir(
        "LinkMap",
        all_maps,
    )
    provider = AppleBundleLinkerMapInfo(linker_maps = all_maps.values())
    return (directory, provider)

def generate_install_data(
        ctx: "context",
        populate_rule_specific_attributes_func: ["function", None] = None,
        **kwargs) -> "artifact":
    data = {
        "fullyQualifiedName": ctx.label,
        ## TODO(T110665037): populate full path similar to bundle_spec.json
        "info_plist": ctx.attrs.info_plist,
        "use_idb": "true",
        ## TODO(T110665037): read from .buckconfig
        # We require the user to have run `xcode-select` and `/var/db/xcode_select_link` to symlink
        # to the selected Xcode. e.g: `/Applications/Xcode_14.2.app/Contents/Developer`
        "xcode_developer_path": "/var/db/xcode_select_link",
    }

    if populate_rule_specific_attributes_func:
        data.update(populate_rule_specific_attributes_func(ctx, **kwargs))

    return ctx.actions.write_json(_INSTALL_DATA_FILE_NAME, data)
