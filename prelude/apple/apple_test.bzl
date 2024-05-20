# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

load(
    "@prelude//:artifact_tset.bzl",
    "project_artifacts",
)
load("@prelude//:paths.bzl", "paths")
load("@prelude//apple:apple_library.bzl", "AppleLibraryAdditionalParams", "apple_library_rule_constructor_params_and_swift_providers")
load("@prelude//apple:apple_toolchain_types.bzl", "AppleToolchainInfo")
# @oss-disable: load("@prelude//apple/meta_only:apple_test_re_capabilities.bzl", "ios_test_re_capabilities", "macos_test_re_capabilities") 
# @oss-disable: load("@prelude//apple/meta_only:apple_test_re_use_case.bzl", "apple_test_re_use_case") 
load("@prelude//apple/swift:swift_compilation.bzl", "get_swift_anonymous_targets", "uses_explicit_modules")
load(
    "@prelude//cxx:argsfiles.bzl",
    "CompileArgsfile",  # @unused Used as a type
)
load("@prelude//cxx:cxx_library.bzl", "cxx_library_parameterized")
load(
    "@prelude//cxx:cxx_sources.bzl",
    "CxxSrcWithFlags",  # @unused Used as a type
)
load("@prelude//cxx:cxx_types.bzl", "CxxRuleProviderParams", "CxxRuleSubTargetParams")
load(
    "@prelude//cxx:linker.bzl",
    "SharedLibraryFlagOverrides",
)
load(
    "@prelude//utils:dicts.bzl",
    "flatten_x",
)
load("@prelude//utils:expect.bzl", "expect")
load(":apple_bundle.bzl", "AppleBundlePartListConstructorParams", "get_apple_bundle_part_list")
load(":apple_bundle_destination.bzl", "AppleBundleDestination", "bundle_relative_path_for_destination")
load(":apple_bundle_part.bzl", "AppleBundlePart", "SwiftStdlibArguments", "assemble_bundle", "bundle_output", "get_apple_bundle_part_relative_destination_path", "get_bundle_dir_name")
load(":apple_bundle_types.bzl", "AppleBundleInfo")
load(":apple_bundle_utility.bzl", "get_product_name")
load(":apple_dsym.bzl", "DSYM_SUBTARGET", "DWARF_AND_DSYM_SUBTARGET", "get_apple_dsym")
load(":apple_entitlements.bzl", "entitlements_link_flags")
load(":apple_sdk.bzl", "get_apple_sdk_name")
load(
    ":apple_sdk_metadata.bzl",
    "MacOSXSdkMetadata",
)
load(":debug.bzl", "AppleDebuggableInfo")
load(":xcode.bzl", "apple_populate_xcode_attributes")
load(":xctest_swift_support.bzl", "XCTestSwiftSupportInfo")

def apple_test_impl(ctx: AnalysisContext) -> [list[Provider], Promise]:
    def get_apple_test_providers(deps_providers) -> list[Provider]:
        xctest_bundle = bundle_output(ctx)

        test_host_app_bundle = _get_test_host_app_bundle(ctx)
        test_host_app_binary = _get_test_host_app_binary(ctx, test_host_app_bundle)
        ui_test_target_app_bundle = _get_ui_test_target_app_bundle(ctx)

        objc_bridging_header_flags = [
            # Disable bridging header -> PCH compilation to mitigate an issue in Xcode 13 beta.
            "-disable-bridging-pch",
            "-import-objc-header",
            cmd_args(ctx.attrs.bridging_header),
        ] if ctx.attrs.bridging_header else []

        shared_library_flags = ["-bundle"]

        # Embedding entitlements (if present) means that we can skip adhoc codesigning
        # any xctests altogether, provided the test dylib is adhoc signed
        shared_library_flags += entitlements_link_flags(ctx)

        # The linker will include adhoc signature for ARM64 by default, lets
        # ensure we always have an adhoc signature regardless of arch/linker logic.
        shared_library_flags += ["-Wl,-adhoc_codesign"]

        constructor_params = apple_library_rule_constructor_params_and_swift_providers(
            ctx,
            AppleLibraryAdditionalParams(
                rule_type = "apple_test",
                extra_exported_link_flags = _get_xctest_framework_linker_flags(ctx) + _get_bundle_loader_flags(test_host_app_binary),
                extra_swift_compiler_flags = _get_xctest_framework_search_paths_flags(ctx) + objc_bridging_header_flags,
                shared_library_flags = SharedLibraryFlagOverrides(
                    # When `-bundle` is used we can't use the `-install_name` args, thus we keep this field empty.
                    shared_library_name_linker_flags_format = [],
                    # When building Apple tests, we want to link with `-bundle` instead of `-shared` to allow
                    # linking against the bundle loader.
                    shared_library_flags = shared_library_flags,
                ),
                generate_sub_targets = CxxRuleSubTargetParams(
                    compilation_database = True,
                    headers = False,
                    link_group_map = False,
                ),
                generate_providers = CxxRuleProviderParams(
                    compilation_database = True,
                    default = False,
                    linkable_graph = False,
                    link_style_outputs = True,
                    merged_native_link_info = False,
                    omnibus_root = False,
                    preprocessors = False,
                    resources = False,
                    shared_libraries = False,
                    template_placeholders = False,
                ),
                populate_xcode_attributes_func = lambda local_ctx, **kwargs: _xcode_populate_attributes(ctx = local_ctx, xctest_bundle = xctest_bundle, test_host_app_binary = test_host_app_binary, **kwargs),
                # We want to statically link the transitive dep graph of the apple_test()
                # which we can achieve by forcing link group linking with
                # an empty mapping (i.e., default mapping).
                force_link_group_linking = True,
            ),
            deps_providers,
            is_test_target = True,
        )

        cxx_library_output = cxx_library_parameterized(ctx, constructor_params)
        test_binary_output = ctx.actions.declare_output(get_product_name(ctx))

        # Rename in order to generate dSYM with correct binary name (dsymutil doesn't provide a way to control binary name in output dSYM bundle).
        test_binary = ctx.actions.copy_file(test_binary_output, cxx_library_output.default_output.default)

        binary_part = AppleBundlePart(source = test_binary, destination = AppleBundleDestination("executables"), new_name = ctx.attrs.name)
        part_list_output = get_apple_bundle_part_list(ctx, AppleBundlePartListConstructorParams(binaries = [binary_part]))

        xctest_swift_support_needed = None
        debug_info = None
        for p in cxx_library_output.providers:
            if isinstance(p, XCTestSwiftSupportInfo):
                xctest_swift_support_needed = p.support_needed
            if isinstance(p, AppleDebuggableInfo):
                debug_info = project_artifacts(ctx.actions, [p.debug_info_tset])
        expect(xctest_swift_support_needed != None, "Expected `XCTestSwiftSupportInfo` provider to be present")
        expect(debug_info != None, "Expected `AppleDebuggableInfo` provider to be present")

        bundle_parts = part_list_output.parts + _get_xctest_framework(ctx, xctest_swift_support_needed)

        for sanitizer_runtime_dylib in cxx_library_output.sanitizer_runtime_files:
            frameworks_destination = AppleBundleDestination("frameworks")
            bundle_parts.append(
                AppleBundlePart(
                    source = sanitizer_runtime_dylib,
                    destination = frameworks_destination,
                    codesign_on_copy = True,
                ),
            )

        primary_binary_rel_path = get_apple_bundle_part_relative_destination_path(ctx, binary_part)
        swift_stdlib_args = SwiftStdlibArguments(primary_binary_rel_path = primary_binary_rel_path)

        bundle_result = assemble_bundle(
            ctx,
            xctest_bundle,
            bundle_parts,
            part_list_output.info_plist_part,
            swift_stdlib_args,
            # Adhoc signing can be skipped because the test executable is adhoc signed
            # + includes any entitlements if present.
            skip_adhoc_signing = True,
        )
        sub_targets = bundle_result.sub_targets
        sub_targets.update(cxx_library_output.sub_targets)

        dsym_artifact = get_apple_dsym(
            ctx = ctx,
            executable = test_binary,
            debug_info = debug_info,
            action_identifier = "generate_apple_test_dsym",
            output_path_override = get_bundle_dir_name(ctx) + ".dSYM",
        )
        sub_targets[DSYM_SUBTARGET] = [DefaultInfo(default_output = dsym_artifact)]

        # If the test has a test host and a ui test target, add the subtargets to build the app bundles.
        sub_targets["test-host"] = [DefaultInfo(default_output = test_host_app_bundle)] if test_host_app_bundle else [DefaultInfo()]
        sub_targets["ui-test-target"] = [DefaultInfo(default_output = ui_test_target_app_bundle)] if ui_test_target_app_bundle else [DefaultInfo()]

        sub_targets[DWARF_AND_DSYM_SUBTARGET] = [
            DefaultInfo(default_output = xctest_bundle, other_outputs = [dsym_artifact]),
            _get_test_info(ctx, xctest_bundle, test_host_app_bundle, dsym_artifact, ui_test_target_app_bundle),
        ]

        return [
            DefaultInfo(default_output = xctest_bundle, sub_targets = sub_targets),
            _get_test_info(ctx, xctest_bundle, test_host_app_bundle, ui_test_target_app_bundle = ui_test_target_app_bundle),
            cxx_library_output.xcode_data_info,
            cxx_library_output.cxx_compilationdb_info,
        ] + bundle_result.providers

    if uses_explicit_modules(ctx):
        return get_swift_anonymous_targets(ctx, get_apple_test_providers)
    else:
        return get_apple_test_providers([])

def _get_test_info(ctx: AnalysisContext, xctest_bundle: Artifact, test_host_app_bundle: Artifact | None, dsym_artifact: Artifact | None = None, ui_test_target_app_bundle: Artifact | None = None) -> Provider:
    # When interacting with Tpx, we just pass our various inputs via env vars,
    # since Tpx basiclaly wants structured output for this.

    xctest_bundle = cmd_args(xctest_bundle).hidden(dsym_artifact) if dsym_artifact else xctest_bundle
    env = {"XCTEST_BUNDLE": xctest_bundle}

    if test_host_app_bundle == None:
        tpx_label = "tpx:apple_test:buck2:logicTest"
    else:
        env["HOST_APP_BUNDLE"] = test_host_app_bundle
        tpx_label = "tpx:apple_test:buck2:appTest"

    if ui_test_target_app_bundle != None:
        env["TARGET_APP_BUNDLE"] = ui_test_target_app_bundle
        tpx_label = "tpx:apple_test:buck2:uiTest"

    labels = ctx.attrs.labels + [tpx_label]
    labels.append(tpx_label)

    sdk_name = get_apple_sdk_name(ctx)
    if sdk_name == MacOSXSdkMetadata.name:
        # @oss-disable: remote_execution_properties = macos_test_re_capabilities() 
        remote_execution_properties = None # @oss-enable

    else:
        # @oss-disable: requires_ios_booted_simulator = ctx.attrs.test_host_app != None or ctx.attrs.ui_test_target_app != None 
        # @oss-disable: remote_execution_properties = ios_test_re_capabilities(use_unbooted_simulator = not requires_ios_booted_simulator) 
        remote_execution_properties = None # @oss-enable

    # @oss-disable: remote_execution_use_case = apple_test_re_use_case(macos_test = sdk_name == MacOSXSdkMetadata.name) 

    remote_execution_use_case = None # @oss-enable
    local_enabled = remote_execution_use_case == None
    remote_enabled = remote_execution_use_case != None

    return ExternalRunnerTestInfo(
        type = "custom",  # We inherit a label via the macro layer that overrides this.
        command = ["false"],  # Tpx makes up its own args, we just pass params via the env.
        env = flatten_x([ctx.attrs.env or {}, env]),
        labels = labels,
        use_project_relative_paths = True,
        run_from_project_root = True,
        contacts = ctx.attrs.contacts,
        executor_overrides = {
            "ios-simulator": CommandExecutorConfig(
                local_enabled = local_enabled,
                remote_enabled = remote_enabled,
                remote_execution_properties = remote_execution_properties,
                remote_execution_use_case = remote_execution_use_case,
            ),
            "static-listing": CommandExecutorConfig(local_enabled = True, remote_enabled = False),
        },
        local_resources = {
            "ios_booted_simulator": ctx.attrs._ios_booted_simulator.label,
            "ios_unbooted_simulator": ctx.attrs._ios_unbooted_simulator.label,
            "macos_idb_companion": ctx.attrs._macos_idb_companion.label,
        },
    )

def _get_test_host_app_bundle(ctx: AnalysisContext) -> Artifact | None:
    """ Get the bundle for the test host app, if one exists for this test. """
    if ctx.attrs.test_host_app:
        # Copy the test host app bundle into test's output directory
        original_bundle = ctx.attrs.test_host_app[AppleBundleInfo].bundle
        test_host_app_bundle = ctx.actions.declare_output(original_bundle.basename)
        ctx.actions.copy_file(test_host_app_bundle, original_bundle)
        return test_host_app_bundle

    return None

def _get_test_host_app_binary(ctx: AnalysisContext, test_host_app_bundle: Artifact | None) -> [cmd_args, None]:
    """ Reference to the binary with the test host app bundle, if one exists for this test. Captures the bundle as an artifact in the cmd_args. """
    if ctx.attrs.test_host_app == None:
        return None

    parts = [test_host_app_bundle]
    rel_path = bundle_relative_path_for_destination(AppleBundleDestination("executables"), get_apple_sdk_name(ctx), ctx.attrs.extension, False)
    if len(rel_path) > 0:
        parts.append(rel_path)
    parts.append(ctx.attrs.test_host_app[AppleBundleInfo].binary_name)
    return cmd_args(parts, delimiter = "/")

def _get_ui_test_target_app_bundle(ctx: AnalysisContext) -> Artifact | None:
    """ Get the bundle for the ui test target app, if one exists for this test. """
    if ctx.attrs.ui_test_target_app:
        # Copy the ui test target app bundle into test's output directory
        original_bundle = ctx.attrs.ui_test_target_app[AppleBundleInfo].bundle
        ui_test_target_app_bundle = ctx.actions.declare_output(original_bundle.basename)
        ctx.actions.copy_file(ui_test_target_app_bundle, original_bundle)
        return ui_test_target_app_bundle

    return None

def _get_bundle_loader_flags(binary: [cmd_args, None]) -> list[typing.Any]:
    if binary:
        # During linking we need to link the test shared lib against the test host binary. The
        # test host binary doesn't need to be embedded in an `apple_bundle`.
        return ["-bundle_loader", binary]

    return []

def _xcode_populate_attributes(
        ctx: AnalysisContext,
        srcs: list[CxxSrcWithFlags],
        argsfiles: dict[str, CompileArgsfile],
        xctest_bundle: Artifact,
        test_host_app_binary: [cmd_args, None],
        **_kwargs) -> dict[str, typing.Any]:
    data = apple_populate_xcode_attributes(ctx = ctx, srcs = srcs, argsfiles = argsfiles, product_name = ctx.attrs.name)
    data["output"] = xctest_bundle
    if test_host_app_binary:
        data["test_host_app_binary"] = test_host_app_binary
    return data

def _get_xctest_framework_search_paths(ctx: AnalysisContext) -> (cmd_args, cmd_args):
    toolchain = ctx.attrs._apple_toolchain[AppleToolchainInfo]
    xctest_swiftmodule_search_path = cmd_args([toolchain.platform_path, "Developer/usr/lib"], delimiter = "/")
    xctest_framework_search_path = cmd_args([toolchain.platform_path, "Developer/Library/Frameworks"], delimiter = "/")
    return (xctest_swiftmodule_search_path, xctest_framework_search_path)

def _get_xctest_framework_search_paths_flags(ctx: AnalysisContext) -> list[[cmd_args, str]]:
    xctest_swiftmodule_search_path, xctest_framework_search_path = _get_xctest_framework_search_paths(ctx)
    return [
        "-I",
        xctest_swiftmodule_search_path,
        "-F",
        xctest_framework_search_path,
    ]

def _get_xctest_framework_linker_flags(ctx: AnalysisContext) -> list[[cmd_args, str]]:
    xctest_swiftmodule_search_path, xctest_framework_search_path = _get_xctest_framework_search_paths(ctx)
    return [
        "-L",
        xctest_swiftmodule_search_path,
        "-F",
        xctest_framework_search_path,
    ]

def _get_xctest_framework(ctx: AnalysisContext, swift_support_needed: bool) -> list[AppleBundlePart]:
    swift_support = [
        _get_object_from_platform_path(ctx, "Developer/usr/lib/libXCTestSwiftSupport.dylib"),
    ] if swift_support_needed else []
    return [
        _get_object_from_platform_path(ctx, "Developer/Library/Frameworks/XCTest.framework"),
        _get_object_from_platform_path(ctx, "Developer/Library/PrivateFrameworks/XCTAutomationSupport.framework"),
        _get_object_from_platform_path(ctx, "Developer/Library/PrivateFrameworks/XCTestCore.framework"),
        _get_object_from_platform_path(ctx, "Developer/Library/PrivateFrameworks/XCTestSupport.framework"),
        _get_object_from_platform_path(ctx, "Developer/Library/PrivateFrameworks/XCUIAutomation.framework"),
        _get_object_from_platform_path(ctx, "Developer/Library/PrivateFrameworks/XCUnit.framework"),
        _get_object_from_platform_path(ctx, "Developer/usr/lib/libXCTestBundleInject.dylib"),
    ] + swift_support

def _get_object_from_platform_path(ctx: AnalysisContext, platform_relative_path: str) -> AppleBundlePart:
    toolchain = ctx.attrs._apple_toolchain[AppleToolchainInfo]
    copied_framework = ctx.actions.declare_output(paths.basename(platform_relative_path))

    # We have to copy because:
    # 1) Platform path might be a string (e.g. for Xcode toolchains)
    # 2) It's not possible to project artifact which is not produced by different target (and platform path is a separate target for distributed toolchains).
    ctx.actions.run(["cp", "-PR", cmd_args(toolchain.platform_path, platform_relative_path, delimiter = "/"), copied_framework.as_output()], category = "extract_framework", identifier = platform_relative_path)

    return AppleBundlePart(source = copied_framework, destination = AppleBundleDestination("frameworks"), codesign_on_copy = True)
