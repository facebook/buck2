load("@fbcode//buck2/prelude/apple:apple_stripping.bzl", "apple_strip_args")
load("@fbcode//buck2/prelude/cxx:cxx.bzl", "get_srcs_with_flags")
load("@fbcode//buck2/prelude/cxx:cxx_executable.bzl", "cxx_executable")
load("@fbcode//buck2/prelude/cxx:cxx_library_utility.bzl", "cxx_attr_deps", "cxx_attr_exported_deps")
load("@fbcode//buck2/prelude/cxx:cxx_types.bzl", "CxxRuleConstructorParams")
load(
    "@fbcode//buck2/prelude/cxx:preprocessor.bzl",
    "CPreprocessor",
)
load("@fbcode//buck2/prelude/linking:link_info.bzl", "extract_default_filelist_from_link_args")
load("@fbcode//buck2/prelude/utils:utils.bzl", "flatten")
load(":apple_bundle_types.bzl", "AppleMinDeploymentVersionInfo")
load(":apple_code_signing_types.bzl", "AppleEntitlementsInfo")
load(":apple_dsym.bzl", "AppleDebuggableInfo", "DSYM_SUBTARGET", "get_apple_dsym")
load(":apple_frameworks.bzl", "get_framework_search_path_flags")
load(":apple_target_sdk_version.bzl", "get_min_deployment_version_for_node", "get_min_deployment_version_target_linker_flags", "get_min_deployment_version_target_preprocessor_flags")
load(":apple_utility.bzl", "get_apple_cxx_headers_layout")
load(":resource_groups.bzl", "create_resource_graph")
load(":xcode.bzl", "apple_populate_xcode_attributes")

def apple_binary_impl(ctx: "context") -> ["provider"]:
    extra_link_flags = get_min_deployment_version_target_linker_flags(ctx) + _entitlements_link_flags(ctx)
    framework_search_path_pre = CPreprocessor(
        args = [cmd_args(get_framework_search_path_flags(ctx))],
    )
    constructor_params = CxxRuleConstructorParams(
        rule_type = "apple_binary",
        headers_layout = get_apple_cxx_headers_layout(ctx),
        extra_exported_link_flags = extra_link_flags,
        srcs = get_srcs_with_flags(ctx),
        extra_preprocessors = get_min_deployment_version_target_preprocessor_flags(ctx) + [framework_search_path_pre],
        strip_executable = ctx.attrs.stripped,
        strip_args_factory = apple_strip_args,
        cxx_populate_xcode_attributes_func = apple_populate_xcode_attributes,
    )
    (cxx_output, _comp_db_info) = cxx_executable(ctx, constructor_params)

    dsym_artifact = get_apple_dsym(
        ctx = ctx,
        executable = cxx_output.binary,
        object_files = flatten(
            [extract_default_filelist_from_link_args(link_arg) for link_arg in cxx_output.link_args],
        ),
        action_identifier = cxx_output.binary.short_path,
    )
    cxx_output.sub_targets[DSYM_SUBTARGET] = [DefaultInfo(default_outputs = [dsym_artifact])]

    min_version = get_min_deployment_version_for_node(ctx)
    min_version_providers = [AppleMinDeploymentVersionInfo(version = min_version)] if min_version != None else []

    resource_graph = create_resource_graph(
        root = ctx.label,
        labels = ctx.attrs.labels,
        deps = cxx_attr_deps(ctx),
        exported_deps = cxx_attr_exported_deps(ctx),
    )

    return [
        DefaultInfo(default_outputs = [cxx_output.binary], sub_targets = cxx_output.sub_targets),
        RunInfo(args = cmd_args(cxx_output.binary).hidden(cxx_output.runtime_files)),
        AppleEntitlementsInfo(entitlements_file = ctx.attrs.entitlements_file),
        AppleDebuggableInfo(dsyms = [dsym_artifact]),
    ] + [resource_graph] + min_version_providers

def _entitlements_link_flags(ctx: "context") -> [""]:
    return [
        "-Xlinker",
        "-sectcreate",
        "-Xlinker",
        "__TEXT",
        "-Xlinker",
        "__entitlements",
        "-Xlinker",
        ctx.attrs.entitlements_file,
    ] if ctx.attrs.entitlements_file else []
