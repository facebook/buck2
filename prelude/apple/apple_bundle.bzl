# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

load(
    "@prelude//:artifact_tset.bzl",
    "ArtifactTSet",  # @unused Used as a type
    "make_artifact_tset",
    "project_artifacts",
)
load("@prelude//:paths.bzl", "paths")
load("@prelude//:validation_deps.bzl", "get_validation_deps_outputs")
load("@prelude//apple:apple_toolchain_types.bzl", "AppleToolchainInfo", "AppleToolsInfo")
load("@prelude//apple:apple_xctest_frameworks_utility.bzl", "get_xctest_frameworks_bundle_parts")
load("@prelude//apple:debug.bzl", "AppleSelectiveDebuggableMetadata")
# @oss-disable[end= ]: load("@prelude//apple/meta_only:linker_outputs.bzl", "subtargets_for_apple_bundle_extra_outputs")
load("@prelude//apple/user:apple_selected_debug_path_file.bzl", "SELECTED_DEBUG_PATH_FILE_NAME")
load("@prelude//apple/user:apple_selective_debugging.bzl", "AppleSelectiveDebuggingInfo")
load("@prelude//apple/validation:debug_artifacts.bzl", "get_debug_artifacts_validators")
load(
    "@prelude//cxx:index_store.bzl",
    "IndexStoreInfo",  # @unused Used as a type
    "create_index_store_subtargets_and_provider",
)
load(
    "@prelude//ide_integrations/xcode:data.bzl",
    "XCODE_DATA_SUB_TARGET",
    "XcodeDataInfoKeys",
    "generate_xcode_data",
)
load(
    "@prelude//linking:execution_preference.bzl",
    "LinkExecutionPreference",
    "LinkExecutionPreferenceInfo",
)
load(
    "@prelude//linking:link_info.bzl",
    "LinkCommandDebugOutputInfo",  # @unused Used as a type
    "UnstrippedLinkOutputInfo",
    "make_link_command_debug_output_json_info",
)
load("@prelude//utils:arglike.bzl", "ArgLike")
load("@prelude//utils:lazy.bzl", "lazy")
load(
    "@prelude//utils:utils.bzl",
    "flatten",
)
load(":apple_bundle_destination.bzl", "AppleBundleDestination")
load(":apple_bundle_part.bzl", "AppleBundlePart", "SwiftStdlibArguments", "assemble_bundle", "bundle_output", "get_apple_bundle_part_relative_destination_path", "get_bundle_dir_name")
load(":apple_bundle_resources.bzl", "get_apple_bundle_resource_part_list")
load(
    ":apple_bundle_types.bzl",
    "AppleBinaryExtraOutputsInfo",
    "AppleBundleBinaryOutput",
    "AppleBundleExtraOutputsInfo",
    "AppleBundleInfo",
    "AppleBundleLinkerMapInfo",
    "AppleBundleResourceInfo",
    "AppleBundleType",
    "AppleBundleTypeDefault",
)
load(":apple_bundle_utility.bzl", "get_bundle_min_target_version", "get_default_binary_dep", "get_flattened_binary_deps", "get_product_name")
load(":apple_code_signing_types.bzl", "CodeSignConfiguration", "get_code_signing_configuration_attr_value")
load(":apple_dsym.bzl", "DSYM_INFO_SUBTARGET", "DSYM_SUBTARGET", "EXTENDED_DSYM_INFO_SUBTARGET", "get_apple_dsym", "get_apple_dsym_ext", "get_apple_dsym_info_json")
load(":apple_sdk.bzl", "get_apple_sdk_name")
load(
    ":apple_sdk_metadata.bzl",
    "MacOSXCatalystSdkMetadata",
    "MacOSXSdkMetadata",
)
load(":apple_universal_binaries.bzl", "create_universal_binary")
load(
    ":debug.bzl",
    "AggregatedAppleDebugInfo",
    "AppleDebuggableInfo",
    "get_aggregated_debug_info",
)
load(":xcode.bzl", "apple_xcode_data_add_xctoolchain")

_INSTALL_DATA_FILE_NAME = "install_apple_data.json"

_PLIST = "plist"

_XCTOOLCHAIN_SUB_TARGET = "xctoolchain"

AppleBundleDebuggableInfo = record(
    binary_info = field(AppleDebuggableInfo),
    # Debugable info of all bundle deps
    dep_infos = field(list[AppleDebuggableInfo]),
    # Concat of `binary_info` and `dep_infos`
    all_infos = field(list[AppleDebuggableInfo]),
)

AppleBundlePartListConstructorParams = record(
    # The binaries/executables, required to create a bundle
    binaries = field(list[AppleBundlePart]),
)

AppleBundlePartListOutput = record(
    # The parts to be copied into an Apple bundle, *including* binaries
    parts = field(list[AppleBundlePart]),
    # Part that holds the info.plist
    info_plist_part = field(AppleBundlePart),
)

_AppleBundleBinaryParts = record(
    all_parts = field(list[AppleBundlePart]),
    primary_part = field(AppleBundlePart),
    sub_targets = field(dict[str, list[DefaultInfo]]),
)

def _get_binary(ctx: AnalysisContext) -> AppleBundleBinaryOutput:
    binary_deps = get_flattened_binary_deps(ctx.attrs.binary)
    if len(binary_deps) > 1:
        if ctx.attrs.selective_debugging != None:
            fail("Selective debugging is not supported for universal binaries.")
        return create_universal_binary(
            ctx = ctx,
            binary_deps = ctx.attrs.binary,
            binary_name = "{}-UniversalBinary".format(get_product_name(ctx)),
            dsym_bundle_name = _get_bundle_dsym_name(ctx),
            split_arch_dsym = ctx.attrs.split_arch_dsym,
        )
    else:
        binary_dep = binary_deps[0]
        if len(binary_dep[DefaultInfo].default_outputs) != 1:
            fail("Expected single output artifact. Make sure the implementation of rule from `binary` attribute is correct.")

        return _maybe_scrub_binary(ctx, binary_dep)

def _get_bundle_dsym_name(ctx: AnalysisContext) -> str:
    return paths.replace_extension(get_bundle_dir_name(ctx), ".dSYM")

def _scrub_binary(ctx, binary: Artifact, binary_execution_preference_info: None | LinkExecutionPreferenceInfo, focused_targets_labels: list[Label] = [], identifier: None | str = None) -> Artifact:
    # If fast adhoc code signing is enabled, we need to resign the binary as it won't be signed later.
    code_signing_configuration = get_code_signing_configuration_attr_value(ctx)
    if code_signing_configuration == CodeSignConfiguration("fast-adhoc"):
        apple_tools = ctx.attrs._apple_tools[AppleToolsInfo]
        adhoc_codesign_tool = apple_tools.adhoc_codesign_tool
    else:
        adhoc_codesign_tool = None

    selective_debugging_info = ctx.attrs.selective_debugging[AppleSelectiveDebuggingInfo]
    preference = binary_execution_preference_info.preference if binary_execution_preference_info else LinkExecutionPreference("any")
    return selective_debugging_info.scrub_binary(ctx, binary, preference, adhoc_codesign_tool, focused_targets_labels, identifier = identifier)

def _maybe_scrub_binary(ctx, binary_dep: Dependency) -> AppleBundleBinaryOutput:
    binary = binary_dep[DefaultInfo].default_outputs[0]
    unstripped_binary = binary_dep.get(UnstrippedLinkOutputInfo).artifact if binary_dep.get(UnstrippedLinkOutputInfo) != None else None
    debuggable_info = binary_dep.get(AppleDebuggableInfo)
    if ctx.attrs.selective_debugging == None:
        return AppleBundleBinaryOutput(binary = binary, unstripped_binary = unstripped_binary, debuggable_info = debuggable_info)

    if debuggable_info:
        if debuggable_info.selective_metadata:
            fail("Binary cannot contain selective metadata, as it only gets scrubbed when embedded in a bundle")

        # If we have debuggable info for this binary, create the scrubed dsym for the binary and filter debug info.
        debug_info_tset = debuggable_info.debug_info_tset

        # The traversal is intentionally designed to be topological, allowing us to skip
        # portions of the debug info that are not transitive in relation to the focused targets.
        all_debug_info = debug_info_tset._tset.traverse(ordering = "topological")
        selective_debugging_info = ctx.attrs.selective_debugging[AppleSelectiveDebuggingInfo]
        filtered_debug_info = selective_debugging_info.filter(ctx, all_debug_info)

        filtered_external_debug_info = make_artifact_tset(
            actions = ctx.actions,
            label = ctx.label,
            infos = filtered_debug_info.infos,
        )

        binary = _scrub_binary(ctx, binary, binary_dep.get(LinkExecutionPreferenceInfo), filtered_debug_info.swift_modules_labels)
        dsym_artifact = _get_scrubbed_binary_dsym(ctx, binary, debug_info_tset)

        filtered_map = {}
        for selected_target_info in filtered_debug_info.selected_target_infos:
            filtered_map.setdefault(selected_target_info.label, []).extend(selected_target_info.artifacts)

        debuggable_info = AppleDebuggableInfo(
            dsyms = [dsym_artifact],
            debug_info_tset = filtered_external_debug_info,
            filtered_map = filtered_map,
            selective_metadata = [
                AppleSelectiveDebuggableMetadata(
                    dsym = dsym_artifact,
                    metadata = filtered_debug_info.metadata,
                ),
            ],
        )
        return AppleBundleBinaryOutput(binary = binary, unstripped_binary = unstripped_binary, debuggable_info = debuggable_info)
    else:
        binary = _scrub_binary(ctx, binary, binary_dep.get(LinkExecutionPreferenceInfo))
        return AppleBundleBinaryOutput(binary = binary, unstripped_binary = unstripped_binary)

def _get_scrubbed_binary_dsym(ctx, binary: Artifact, debug_info_tset: ArtifactTSet) -> Artifact:
    debug_info = project_artifacts(
        actions = ctx.actions,
        tsets = [debug_info_tset],
    )
    dsym_artifact = get_apple_dsym(
        ctx = ctx,
        executable = binary,
        debug_info = debug_info,
        action_identifier = binary.short_path,
    )
    return dsym_artifact

def _get_binary_bundle_parts(ctx: AnalysisContext, binary_output: AppleBundleBinaryOutput, aggregated_debug_info: AggregatedAppleDebugInfo) -> _AppleBundleBinaryParts:
    """Returns a tuple of all binary bundle parts and the primary bundle binary."""
    result = []

    primary_binary_part = AppleBundlePart(source = binary_output.binary, destination = AppleBundleDestination("executables"), new_name = get_product_name(ctx))
    result.append(primary_binary_part)

    selected_debug_target_part = _get_selected_debug_targets_part(ctx, aggregated_debug_info)
    if selected_debug_target_part:
        result.append(selected_debug_target_part)

    sub_targets = {
        "selected-debug-paths": [DefaultInfo(default_output = selected_debug_target_part.source if selected_debug_target_part else None)],
    }

    return _AppleBundleBinaryParts(
        all_parts = result,
        primary_part = primary_binary_part,
        sub_targets = sub_targets,
    )

def _get_dsym_input_binary_arg(ctx: AnalysisContext, binary_output: AppleBundleBinaryOutput, primary_binary_path_arg: cmd_args) -> cmd_args:
    # We've already scrubbed the default binary, we only want to scrub the unstripped one if present.
    unstripped_binary = binary_output.unstripped_binary
    if unstripped_binary:
        if ctx.attrs.selective_debugging != None:
            unstripped_binary = _scrub_binary(ctx, unstripped_binary, get_default_binary_dep(ctx.attrs.binary).get(LinkExecutionPreferenceInfo), identifier = "unstripped")
        renamed_unstripped_binary = ctx.actions.copy_file(get_product_name(ctx), unstripped_binary)
        return cmd_args(renamed_unstripped_binary)
    else:
        return primary_binary_path_arg

def _apple_bundle_run_validity_checks(ctx: AnalysisContext):
    if ctx.attrs.extension == None:
        fail("`extension` attribute is required")

def _get_deps_selective_metadata(deps_debuggable_infos: list[AppleDebuggableInfo]) -> list[AppleSelectiveDebuggableMetadata]:
    all_metadatas = []
    for debuggable_info in deps_debuggable_infos:
        all_metadatas.extend(debuggable_info.selective_metadata)
    return all_metadatas

def _get_deps_debuggable_infos(ctx: AnalysisContext) -> list[AppleDebuggableInfo]:
    binary_labels = filter(None, [getattr(binary_dep, "label", None) for binary_dep in get_flattened_binary_deps(ctx.attrs.binary)])
    deps_debuggable_infos = filter(
        None,
        # It's allowed for `ctx.attrs.binary` to appear in `ctx.attrs.deps` as well,
        # in this case, do not duplicate the debugging info for the binary coming from two paths.
        [dep.get(AppleDebuggableInfo) for dep in ctx.attrs.deps if dep.label not in binary_labels],
    )
    return deps_debuggable_infos

def _get_bundle_binary_dsym_artifacts(ctx: AnalysisContext, binary_output: AppleBundleBinaryOutput, executable_arg: ArgLike) -> list[Artifact]:
    if not ctx.attrs.split_arch_dsym:
        # Calling `dsymutil` on the correctly named binary in the _final bundle_ to yield dsym files
        # with naming convention compatible with Meta infra.
        binary_debuggable_info = binary_output.debuggable_info
        bundle_binary_dsym_artifact = get_apple_dsym_ext(
            ctx = ctx,
            executable = executable_arg,
            debug_info = project_artifacts(
                actions = ctx.actions,
                tsets = [binary_debuggable_info.debug_info_tset] if binary_debuggable_info else [],
            ),
            action_identifier = get_bundle_dir_name(ctx),
            output_path = _get_bundle_dsym_name(ctx),
        )
        return [bundle_binary_dsym_artifact]
    else:
        return binary_output.debuggable_info.dsyms

def _get_all_agg_debug_info(ctx: AnalysisContext, binary_output: AppleBundleBinaryOutput, deps_debuggable_infos: list[AppleDebuggableInfo]) -> AggregatedAppleDebugInfo:
    all_debug_infos = deps_debuggable_infos + ([binary_output.debuggable_info] if binary_output.debuggable_info else [])
    return get_aggregated_debug_info(ctx, all_debug_infos)

def _maybe_scrub_selected_debug_paths_file(ctx: AnalysisContext, package_names: list[str]) -> Artifact:
    if not ctx.attrs.selective_debugging:
        return ctx.actions.write(SELECTED_DEBUG_PATH_FILE_NAME, sorted(set(package_names)))

    selective_debugging_info = ctx.attrs.selective_debugging[AppleSelectiveDebuggingInfo]
    return selective_debugging_info.scrub_selected_debug_paths_file(ctx, package_names, SELECTED_DEBUG_PATH_FILE_NAME)

def _get_selected_debug_targets_part(ctx: AnalysisContext, agg_debug_info: AggregatedAppleDebugInfo) -> [AppleBundlePart, None]:
    # Only app bundle need this, and this file is searched by FBReport at the bundle root
    if ctx.attrs.extension == "app" and agg_debug_info.debug_info.filtered_map:
        package_names = [label.package for label in agg_debug_info.debug_info.filtered_map.keys()]
        output = _maybe_scrub_selected_debug_paths_file(ctx, package_names)
        return AppleBundlePart(source = output, destination = AppleBundleDestination("bundleroot"), new_name = SELECTED_DEBUG_PATH_FILE_NAME)
    else:
        return None

def get_apple_bundle_part_list(ctx: AnalysisContext, params: AppleBundlePartListConstructorParams) -> AppleBundlePartListOutput:
    resource_part_list = None
    if hasattr(ctx.attrs, "_resource_bundle") and ctx.attrs._resource_bundle != None:
        resource_info = ctx.attrs._resource_bundle[AppleBundleResourceInfo]
        if resource_info != None:
            resource_part_list = resource_info.resource_output

    if resource_part_list == None:
        resource_part_list = get_apple_bundle_resource_part_list(ctx)

    xctest_frameworks_parts = []
    if getattr(ctx.attrs, "embed_xctest_frameworks", False):
        if getattr(ctx.attrs, "extension", "") == "app":
            # XCTest frameworks should only be enabled for the top-level app,
            # not for any other bundles in the dep graph
            xctest_frameworks_parts = get_xctest_frameworks_bundle_parts(
                ctx,
                # It's not possible to pass information down the graph whether
                # the `apple_test()` rdep needs Swift support, so just assume
                # it does, in the future, Obj-C only test targets would be rare.
                swift_support_needed = True,
            )

    return AppleBundlePartListOutput(
        parts = resource_part_list.resource_parts + params.binaries + xctest_frameworks_parts,
        info_plist_part = resource_part_list.info_plist_part,
    )

def _infer_apple_bundle_type(ctx: AnalysisContext) -> AppleBundleType:
    if ctx.attrs.bundle_type != None:
        return AppleBundleType(ctx.attrs.bundle_type)

    return AppleBundleTypeDefault

def apple_bundle_impl(ctx: AnalysisContext) -> list[Provider]:
    _apple_bundle_run_validity_checks(ctx)

    binary_outputs = _get_binary(ctx)

    deps_debuggable_infos = _get_deps_debuggable_infos(ctx)
    aggregated_debug_info = _get_all_agg_debug_info(ctx, binary_outputs, deps_debuggable_infos)

    binary_parts = _get_binary_bundle_parts(ctx, binary_outputs, aggregated_debug_info)
    apple_bundle_part_list_output = get_apple_bundle_part_list(ctx, AppleBundlePartListConstructorParams(binaries = binary_parts.all_parts))

    bundle = bundle_output(ctx)

    primary_binary_rel_path = get_apple_bundle_part_relative_destination_path(ctx, binary_parts.primary_part)

    validation_deps_outputs = get_validation_deps_outputs(ctx)

    incremental_bundling_override = None
    sdk_name = get_apple_sdk_name(ctx)
    if sdk_name == MacOSXSdkMetadata.name or sdk_name == MacOSXCatalystSdkMetadata.name:
        incremental_bundling_override = False

    bundle_result = assemble_bundle(
        ctx,
        bundle,
        apple_bundle_part_list_output.parts,
        apple_bundle_part_list_output.info_plist_part,
        SwiftStdlibArguments(primary_binary_rel_path = primary_binary_rel_path),
        validation_deps_outputs,
        incremental_bundling_override = incremental_bundling_override,
    )
    sub_targets = bundle_result.sub_targets
    sub_targets.update(aggregated_debug_info.sub_targets)
    sub_targets.update(binary_parts.sub_targets)

    primary_binary_path = cmd_args([bundle, primary_binary_rel_path], delimiter = "/")
    primary_binary_path_arg = cmd_args(primary_binary_path, hidden = bundle)

    linker_maps_directory, linker_map_info = _linker_maps_data(ctx)
    sub_targets["linker-maps"] = [DefaultInfo(default_output = linker_maps_directory)]

    link_cmd_debug_file, link_cmd_debug_info = _link_command_debug_data(ctx)
    sub_targets["linker.command"] = [DefaultInfo(default_outputs = filter(None, [link_cmd_debug_file]))]

    # dsyms
    dsym_input_binary_arg = _get_dsym_input_binary_arg(ctx, binary_outputs, primary_binary_path_arg)
    binary_dsym_artifacts = _get_bundle_binary_dsym_artifacts(ctx, binary_outputs, dsym_input_binary_arg)
    dep_dsym_artifacts = flatten([info.dsyms for info in deps_debuggable_infos])

    dsym_artifacts = binary_dsym_artifacts + dep_dsym_artifacts
    if dsym_artifacts:
        sub_targets[DSYM_SUBTARGET] = [DefaultInfo(default_outputs = dsym_artifacts)]

    dsym_json_info = get_apple_dsym_info_json(binary_dsym_artifacts, dep_dsym_artifacts)
    dsym_info = ctx.actions.write_json("dsym-info.json", dsym_json_info.json_object, pretty = True)
    sub_targets[DSYM_INFO_SUBTARGET] = [
        DefaultInfo(default_output = dsym_info, other_outputs = dsym_json_info.outputs),
    ]

    deps_selective_metadata = _get_deps_selective_metadata(deps_debuggable_infos)
    binary_selective_metadata = []
    if binary_outputs.debuggable_info and binary_outputs.debuggable_info.selective_metadata:
        if len(binary_outputs.debuggable_info.selective_metadata) > 1:
            fail("Binary cannot have multiple selective metadata")

        # `AppleSelectiveDebuggableMetadata` for the binary is computed here because
        # the dSYMs for the bundle get regenerated (via call to `_get_bundle_binary_dsym_artifacts()`).
        # To ensure we have the correct dSYM path, metadata needs to be created here, as otherwise
        # the map will contain the value of the dSYM for the standalone binary, not for the binary
        # as part of the bundle.
        binary_selective_metadata = [AppleSelectiveDebuggableMetadata(dsym = binary_dsym, metadata = binary_outputs.debuggable_info.selective_metadata[0].metadata) for binary_dsym in binary_dsym_artifacts]
    all_selective_metadata = binary_selective_metadata + deps_selective_metadata

    extended_dsym_json_info = get_apple_dsym_info_json(binary_dsym_artifacts, dep_dsym_artifacts, all_selective_metadata)
    extended_dsym_info = ctx.actions.write_json("extended-dsym-info.json", extended_dsym_json_info.json_object, pretty = True)
    sub_targets[EXTENDED_DSYM_INFO_SUBTARGET] = [
        DefaultInfo(default_output = extended_dsym_info, other_outputs = extended_dsym_json_info.outputs),
    ]

    sub_targets[_PLIST] = [DefaultInfo(default_output = apple_bundle_part_list_output.info_plist_part.source)]

    sub_targets[_XCTOOLCHAIN_SUB_TARGET] = ctx.attrs._apple_xctoolchain.providers

    # Define the xcode data sub target
    plist_bundle_relative_path = get_apple_bundle_part_relative_destination_path(ctx, apple_bundle_part_list_output.info_plist_part)
    xcode_data_default_info, xcode_data_info = generate_xcode_data(ctx, "apple_bundle", bundle, _xcode_populate_attributes, processed_info_plist = apple_bundle_part_list_output.info_plist_part.source, info_plist_relative_path = plist_bundle_relative_path)
    sub_targets[XCODE_DATA_SUB_TARGET] = xcode_data_default_info

    install_data = generate_install_data(ctx, plist_bundle_relative_path)

    # Collect extra bundle outputs
    extra_output_provider = _extra_output_provider(ctx)
    # @oss-disable[end= ]: extra_output_subtargets = subtargets_for_apple_bundle_extra_outputs(ctx, extra_output_provider)
    # @oss-disable[end= ]: sub_targets.update(extra_output_subtargets)

    # index store
    index_store_subtargets, index_store_info = _index_store_data(ctx)
    sub_targets.update(index_store_subtargets)

    bundle_and_dsym_info_json = {
        "bundle": bundle,
        "dsym": dsym_json_info.json_object,
    }
    bundle_and_dsym_info = ctx.actions.write_json("bundle-and-dsym-info.json", bundle_and_dsym_info_json)
    sub_targets["bundle-and-dsym-info"] = [
        DefaultInfo(
            default_output = bundle_and_dsym_info,
            other_outputs = [bundle] + dsym_json_info.outputs,
        ),
    ]

    (validation_providers, validation_subtargets) = _get_debug_validators_subtargets_and_providers(
        ctx,
        aggregated_debug_info.debug_info.debug_info_tset,
    )
    sub_targets.update(validation_subtargets)

    return [
        DefaultInfo(default_output = bundle, sub_targets = sub_targets),
        AppleBundleInfo(
            bundle = bundle,
            bundle_type = _infer_apple_bundle_type(ctx),
            binary_name = get_product_name(ctx),
            contains_watchapp = lazy.is_any(lambda part: part.destination == AppleBundleDestination("watchapp"), apple_bundle_part_list_output.parts),
            skip_copying_swift_stdlib = ctx.attrs.skip_copying_swift_stdlib,
        ),
        AppleDebuggableInfo(
            dsyms = dsym_artifacts,
            debug_info_tset = aggregated_debug_info.debug_info.debug_info_tset,
            filtered_map = aggregated_debug_info.debug_info.filtered_map,
            selective_metadata = all_selective_metadata,
        ),
        InstallInfo(
            installer = ctx.attrs._apple_toolchain[AppleToolchainInfo].installer,
            files = {
                "app_bundle": bundle,
                "options": install_data,
            },
        ),
        RunInfo(args = primary_binary_path_arg),
        linker_map_info,
        xcode_data_info,
        extra_output_provider,
        link_cmd_debug_info,
        index_store_info,
    ] + bundle_result.providers + validation_providers

def _xcode_populate_attributes(ctx, processed_info_plist: Artifact, info_plist_relative_path: str) -> dict[str, typing.Any]:
    data = {
        XcodeDataInfoKeys.BUNDLE_TYPE: _infer_apple_bundle_type(ctx),
        XcodeDataInfoKeys.DEPLOYMENT_VERSION: get_bundle_min_target_version(ctx, get_default_binary_dep(ctx.attrs.binary)),
        XcodeDataInfoKeys.INFO_PLIST: ctx.attrs.info_plist,
        XcodeDataInfoKeys.PROCESSED_INFO_PLIST: processed_info_plist,
        XcodeDataInfoKeys.INFO_PLIST_RELATIVE_PATH: info_plist_relative_path,
        XcodeDataInfoKeys.PRODUCT_NAME: get_product_name(ctx),
        XcodeDataInfoKeys.SDK: get_apple_sdk_name(ctx),
        XcodeDataInfoKeys.APP_EXTENSION_DEPENDENCIES: _app_extension_deps(ctx),
    }

    apple_xcode_data_add_xctoolchain(ctx, data)
    return data

def _get_debug_validators_subtargets_and_providers(ctx, artifacts: ArtifactTSet) -> (list[Provider], dict[str, list[Provider]]):
    name_to_debug_validator_artifact = get_debug_artifacts_validators(ctx, artifacts)
    if not name_to_debug_validator_artifact:
        return ([], {})

    return (
        [
            ValidationInfo(
                validations = [
                    ValidationSpec(
                        name = name,
                        validation_result = artifact,
                    )
                    for name, artifact in name_to_debug_validator_artifact.items()
                ],
            ),
        ],
        {
            "debug-artifacts-validators": [
                DefaultInfo(
                    default_outputs = name_to_debug_validator_artifact.values(),
                    sub_targets = {
                        name: [DefaultInfo(default_output = artifact)]
                        for name, artifact in name_to_debug_validator_artifact.items()
                    },
                ),
            ],
        },
    )

def _linker_maps_data(ctx: AnalysisContext) -> (Artifact, AppleBundleLinkerMapInfo):
    deps_with_binary = ctx.attrs.deps + get_flattened_binary_deps(ctx.attrs.binary)
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

def _link_command_debug_data(ctx: AnalysisContext) -> (Artifact, LinkCommandDebugOutputInfo):
    deps_with_binary = ctx.attrs.deps + get_flattened_binary_deps(ctx.attrs.binary)
    debug_output_infos = filter(
        None,
        [dep.get(LinkCommandDebugOutputInfo) for dep in deps_with_binary],
    )
    all_debug_infos = flatten([debug_info.debug_outputs for debug_info in debug_output_infos])
    link_cmd_debug_output_file = make_link_command_debug_output_json_info(ctx, all_debug_infos)
    return link_cmd_debug_output_file, LinkCommandDebugOutputInfo(debug_outputs = all_debug_infos)

def _index_store_data(ctx: AnalysisContext) -> (dict[str, list[Provider]], IndexStoreInfo):
    deps_with_binary = ctx.attrs.deps + get_flattened_binary_deps(ctx.attrs.binary)
    index_store_subtargets, index_store_info = create_index_store_subtargets_and_provider(ctx, [], [], deps_with_binary)
    return index_store_subtargets, index_store_info

def _extra_output_provider(ctx: AnalysisContext) -> AppleBundleExtraOutputsInfo:
    # Collect the sub_targets for this bundle's binary that are extra_linker_outputs.
    extra_outputs = []
    for binary_dep in get_flattened_binary_deps(ctx.attrs.binary):
        linker_outputs = ctx.attrs._apple_toolchain[AppleToolchainInfo].extra_linker_outputs
        binary_outputs = {k: v[DefaultInfo].default_outputs for k, v in binary_dep[DefaultInfo].sub_targets.items() if k in linker_outputs}
        extra_outputs.append(AppleBinaryExtraOutputsInfo(
            name = get_product_name(ctx),
            default_output = binary_dep[DefaultInfo].default_outputs[0],
            extra_outputs = binary_outputs,
        ))

    # Collect the transitive extra bundle outputs from the deps.
    for dep in ctx.attrs.deps:
        if AppleBundleExtraOutputsInfo in dep:
            extra_outputs.extend(dep[AppleBundleExtraOutputsInfo].extra_outputs)

    return AppleBundleExtraOutputsInfo(extra_outputs = extra_outputs)

def _app_extension_deps(ctx: AnalysisContext) -> list[str]:
    app_extension_deps = []
    for dep in ctx.attrs.deps:
        apple_bundle_info = dep.get(AppleBundleInfo)
        if apple_bundle_info and paths.split_extension(apple_bundle_info.bundle.short_path)[1] == ".appex":
            app_extension_deps.append(str(dep.label.raw_target()))
    return app_extension_deps

def generate_install_data(
        ctx: AnalysisContext,
        plist_path: str,
        populate_rule_specific_attributes_func: [typing.Callable, None] = None,
        **kwargs) -> Artifact:
    data = {
        "fullyQualifiedName": ctx.label,
        "info_plist": plist_path,
        "platform_name": get_apple_sdk_name(ctx),
        "use_idb": "true",
        ## TODO(T110665037): read from .buckconfig
        # We require the user to have run `xcode-select` and `/var/db/xcode_select_link` to symlink
        # to the selected Xcode. e.g: `/Applications/Xcode_14.2.app/Contents/Developer`
        "xcode_developer_path": "/var/db/xcode_select_link",
    }

    if populate_rule_specific_attributes_func:
        data.update(populate_rule_specific_attributes_func(ctx, **kwargs))

    return ctx.actions.write_json(_INSTALL_DATA_FILE_NAME, data)
