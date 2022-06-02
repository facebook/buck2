load("@fbcode//buck2/prelude/apple:apple_dsym.bzl", "AppleDebuggableInfo", "DSYM_SUBTARGET", "get_apple_dsym")
load("@fbcode//buck2/prelude/apple:apple_stripping.bzl", "apple_strip_args")
load("@fbcode//buck2/prelude/apple:swift_compilation.bzl", "compile_swift", "get_swift_dependency_infos")
load("@fbcode//buck2/prelude/cxx:cxx.bzl", "get_srcs_with_flags")
load("@fbcode//buck2/prelude/cxx:cxx_library.bzl", "cxx_library_parameterized")
load("@fbcode//buck2/prelude/cxx:cxx_types.bzl", "CxxRuleConstructorParams", "CxxRuleProviderParams", "CxxRuleSubTargetParams")
load("@fbcode//buck2/prelude/cxx:headers.bzl", "cxx_attr_exported_headers")
load(
    "@fbcode//buck2/prelude/cxx:linker.bzl",
    "SharedLibraryFlagOverrides",
)
load(
    "@fbcode//buck2/prelude/cxx:preprocessor.bzl",
    "CPreprocessor",
)
load("@fbcode//buck2/prelude/linking:link_info.bzl", "LinkStyle")
load(":apple_bundle_types.bzl", "AppleMinDeploymentVersionInfo")
load(":apple_frameworks.bzl", "get_apple_frameworks_linker_flags", "get_framework_search_path_flags")
load(":apple_modular_utility.bzl", "MODULE_CACHE_PATH")
load(":apple_target_sdk_version.bzl", "get_min_deployment_version_for_node", "get_min_deployment_version_target_linker_flags", "get_min_deployment_version_target_preprocessor_flags")
load(":apple_utility.bzl", "get_apple_cxx_headers_layout", "get_module_name")
load(":modulemap.bzl", "preprocessor_info_for_modulemap")
load(":resource_groups.bzl", "create_resource_graph")
load(":xcode.bzl", "apple_populate_xcode_attributes")

AppleLibraryAdditionalParams = record(
    # Name of the top level rule utilizing the apple_library rule.
    rule_type = str.type,
    # Extra flags to be passed to the linker.
    extra_exported_link_flags = field(["_arglike"], []),
    # Linker flags that tell the linker to create shared libraries, overriding the default shared library flags.
    # e.g. when building Apple tests, we want to link with `-bundle` instead of `-shared` to allow
    # linking against the bundle loader.
    shared_library_flags = field([SharedLibraryFlagOverrides.type, None], None),
    # Function to use for setting Xcode attributes for the Xcode data sub target.
    populate_xcode_attributes_func = field("function", apple_populate_xcode_attributes),
    # Define which sub targets to generate.
    generate_sub_targets = field(CxxRuleSubTargetParams.type, CxxRuleSubTargetParams()),
    # Define which providers to generate.
    generate_providers = field(CxxRuleProviderParams.type, CxxRuleProviderParams()),
)

def apple_library_impl(ctx: "context") -> ["provider"]:
    constructor_params, swift_providers = apple_library_rule_constructor_params_and_swift_providers(ctx, AppleLibraryAdditionalParams(rule_type = "apple_library"))

    resource_graph = create_resource_graph(
        root = ctx.label,
        labels = ctx.attr.labels,
        deps = ctx.attr.deps,
        exported_deps = ctx.attr.exported_deps,
    )

    output = cxx_library_parameterized(ctx, constructor_params)
    providers = output.providers + [resource_graph] + swift_providers
    return providers

def apple_library_rule_constructor_params_and_swift_providers(ctx: "context", params: AppleLibraryAdditionalParams.type) -> (CxxRuleConstructorParams.type, ["provider"]):
    extra_exported_link_flags = get_apple_frameworks_linker_flags(ctx) + params.extra_exported_link_flags
    cxx_srcs, swift_srcs = _filter_swift_srcs(ctx)

    # First create a modulemap if necessary. This is required for importing
    # ObjC code in Swift so must be done before Swift compilation.
    exported_hdrs = cxx_attr_exported_headers(ctx, get_apple_cxx_headers_layout(ctx))
    if (ctx.attr.modular or swift_srcs) and exported_hdrs:
        modulemap_pre = preprocessor_info_for_modulemap(ctx, "exported", exported_hdrs, None)
    else:
        modulemap_pre = None

    swift_compile = compile_swift(ctx, swift_srcs, exported_hdrs, modulemap_pre)
    swift_object_files = swift_compile.object_files if swift_compile else []

    swift_pre = CPreprocessor()
    if swift_compile:
        # If we have Swift we export the extended modulemap that includes
        # the ObjC exported headers and the -Swift.h header.
        exported_pre = swift_compile.exported_pre

        # We also include the -Swift.h header to this libraries preprocessor
        # info, so that we can import it unprefixed in this module.
        swift_pre = swift_compile.pre
    elif modulemap_pre:
        # Otherwise if this library is modular we export a modulemap of
        # the ObjC exported headers.
        exported_pre = modulemap_pre
    else:
        exported_pre = None

    # Returns SwiftInfo and SwiftPcmInfo(compiled pcm of exported headers)
    swift_providers = swift_compile.providers if swift_compile else get_swift_dependency_infos(ctx, exported_pre, None)

    modular_pre = CPreprocessor(
        uses_modules = ctx.attr.uses_modules,
        modular_args = [
            "-fcxx-modules",
            "-fmodules",
            "-fmodule-name=" + get_module_name(ctx),
            "-fmodules-cache-path=" + MODULE_CACHE_PATH,
        ],
    )

    framework_search_path_pre = CPreprocessor(
        args = [cmd_args(get_framework_search_path_flags(ctx))],
    )

    return CxxRuleConstructorParams(
        rule_type = params.rule_type,
        headers_layout = get_apple_cxx_headers_layout(ctx),
        extra_exported_link_flags = extra_exported_link_flags,
        extra_link_flags = get_min_deployment_version_target_linker_flags(ctx),
        extra_link_input = swift_object_files,
        extra_preprocessors = get_min_deployment_version_target_preprocessor_flags(ctx) + [framework_search_path_pre, swift_pre, modular_pre],
        extra_exported_preprocessors = filter(None, [exported_pre]),
        srcs = cxx_srcs,
        additional_srcs = swift_srcs,
        link_style_sub_targets_and_providers_factory = _get_shared_link_style_sub_targets_and_providers,
        shared_library_flags = params.shared_library_flags,
        # apple_library's 'stripped' arg only applies to shared subtargets, or,
        # targets with 'preferred_linkage = "shared"'
        strip_executable = ctx.attr.stripped,
        strip_args_factory = apple_strip_args,
        # when building Apple shared libraries we want to always use link group's linking logic,
        # which provides the desired behavior of linking Apple shared libraries.
        force_link_group_linking = True,
        cxx_populate_xcode_attributes_func = params.populate_xcode_attributes_func,
        generate_sub_targets = params.generate_sub_targets,
        generate_providers = params.generate_providers,
    ), swift_providers

def _filter_swift_srcs(ctx: "context") -> (["CxxSrcWithFlags"], ["CxxSrcWithFlags"]):
    cxx_srcs = []
    swift_srcs = []
    for s in get_srcs_with_flags(ctx):
        if s.file.extension == ".swift":
            swift_srcs.append(s)
        else:
            cxx_srcs.append(s)

    return cxx_srcs, swift_srcs

def _get_shared_link_style_sub_targets_and_providers(
        link_style: LinkStyle.type,
        ctx: "context",
        executable: "artifact",
        object_files: ["artifact"]) -> ({str.type: ["provider"]}, ["provider"]):
    if link_style != LinkStyle("shared"):
        return ({}, [])

    min_version = get_min_deployment_version_for_node(ctx)
    min_version_providers = [AppleMinDeploymentVersionInfo(version = min_version)] if min_version != None else []

    dsym_artifact = get_apple_dsym(
        ctx = ctx,
        executable = executable,
        object_files = object_files,
        action_identifier = executable.short_path,
    )
    return ({
        DSYM_SUBTARGET: [DefaultInfo(default_outputs = [dsym_artifact])],
    }, [AppleDebuggableInfo(dsyms = [dsym_artifact])] + min_version_providers)
