load("@fbcode//buck2/prelude/apple:apple_library.bzl", "AppleLibraryAdditionalParams", "apple_library_rule_constructor_params_and_swift_providers")
load(
    "@fbcode//buck2/prelude/cxx:compile.bzl",
    "CxxExtension",  # @unused Used as a type
    "CxxSrcWithFlags",  # @unused Used as a type
)
load("@fbcode//buck2/prelude/cxx:cxx_library.bzl", "cxx_library_parameterized")
load("@fbcode//buck2/prelude/cxx:cxx_types.bzl", "CxxRuleProviderParams", "CxxRuleSubTargetParams")
load(
    "@fbcode//buck2/prelude/cxx:linker.bzl",
    "SharedLibraryFlagOverrides",
)
load(":apple_bundle.bzl", "AppleBundlePartListConstructorParams", "get_apple_bundle_part_list")
load(":apple_bundle_destination.bzl", "AppleBundleDestination")
load(":apple_bundle_part.bzl", "AppleBundlePart", "assemble_bundle", "bundle_output")
load(":apple_bundle_types.bzl", "AppleBundleInfo")
load(":xcode.bzl", "apple_populate_xcode_attributes")

def apple_test_impl(ctx: "context") -> ["provider"]:
    xctest_bundle = bundle_output(ctx)

    test_host_app_bundle = _get_test_host_app_bundle(ctx)
    test_host_app_binary = _get_test_host_app_binary(ctx, test_host_app_bundle)

    constructor_params, _ = apple_library_rule_constructor_params_and_swift_providers(
        ctx,
        AppleLibraryAdditionalParams(
            rule_type = "apple_test",
            extra_link_flags = _get_bundle_loader_flags(test_host_app_binary),
            shared_library_flags = SharedLibraryFlagOverrides(
                # When `-bundle` is used we can't use the `-install_name` args, thus we keep this field empty.
                shared_library_name_linker_flags_format = [],
                # When building Apple tests, we want to link with `-bundle` instead of `-shared` to allow
                # linking against the bundle loader.
                shared_library_flags = ["-bundle"],
            ),
            generate_sub_targets = CxxRuleSubTargetParams(
                compilation_database = False,
                headers = False,
                link_group_map = False,
                link_style_outputs = False,
            ),
            generate_providers = CxxRuleProviderParams(
                compilation_database = False,
                default = False,
                linkable_graph = False,
                link_style_outputs = False,
                merged_native_link_info = False,
                omnibus_root = False,
                preprocessors = False,
                resources = False,
                shared_libraries = False,
                template_placeholders = False,
            ),
            populate_xcode_attributes_func = lambda local_ctx, **kwargs: _xcode_populate_attributes(ctx = local_ctx, xctest_bundle = xctest_bundle, test_host_app_binary = test_host_app_binary, **kwargs),
        ),
    )
    cxx_library_output = cxx_library_parameterized(ctx, constructor_params)

    binary_part = AppleBundlePart(source = cxx_library_output.default_output.default, destination = AppleBundleDestination("executables"), new_name = ctx.attr.name)
    part_list_output = get_apple_bundle_part_list(ctx, AppleBundlePartListConstructorParams(binaries = [binary_part]))
    assemble_bundle(ctx, xctest_bundle, part_list_output.parts, part_list_output.info_plist_part)

    xctest = ctx.attr._fbxctest[RunInfo]
    args = [cmd_args([
        xctest,
    ])]

    sub_targets = cxx_library_output.sub_targets

    # If the test has a test host, add a subtarget to build the test host app bundle.
    sub_targets["test-host"] = [DefaultInfo(default_outputs = [test_host_app_bundle])] if test_host_app_bundle else [DefaultInfo()]

    return [
        DefaultInfo(default_outputs = [xctest_bundle], sub_targets = sub_targets),
        # TODO(T112699634): Change the type to apple_test and match up the
        # arguments to the one that Buck1 sets in the external runner spec.
        ExternalRunnerTestInfo(
            type = "custom",
            command = args,
            env = {},
            labels = ctx.attr.labels,
            contacts = ctx.attr.contacts,
            use_templated_api = False,
        ),
    ]

def _get_test_host_app_bundle(ctx: "context") -> ["artifact", None]:
    """ Get the bundle for the test host app, if one exists for this test. """
    if ctx.attr.test_host_app:
        # Copy the test host app bundle into test's output directory
        original_bundle = ctx.attr.test_host_app[AppleBundleInfo].bundle
        test_host_app_bundle = ctx.actions.declare_output(original_bundle.basename)
        ctx.actions.copy(original_bundle, test_host_app_bundle)
        return test_host_app_bundle

    return None

def _get_test_host_app_binary(ctx: "context", test_host_app_bundle: ["artifact", None]) -> ["cmd_args", None]:
    """ Reference to the binary with the test host app bundle, if one exists for this test. Captures the bundle as an artifact in the cmd_args. """
    if ctx.attr.test_host_app:
        return cmd_args([test_host_app_bundle, ctx.attr.test_host_app[AppleBundleInfo].binary_name], joined = True, delimiter = "/")

    return None

def _get_bundle_loader_flags(binary: ["cmd_args", None]) -> [""]:
    if binary:
        # During linking we need to link the test shared lib against the test host binary. The
        # test host binary doesn't need to be embedded in an `apple_bundle`.
        return ["-bundle_loader", binary]

    return []

def _xcode_populate_attributes(
        ctx,
        srcs: [CxxSrcWithFlags.type],
        argsfiles_by_ext: {CxxExtension.type: "artifact"},
        xctest_bundle: "artifact",
        test_host_app_binary: ["cmd_args", None],
        **_kwargs) -> {str.type: ""}:
    data = apple_populate_xcode_attributes(ctx = ctx, srcs = srcs, argsfiles_by_ext = argsfiles_by_ext, product_name = ctx.attr.name)
    data["output"] = xctest_bundle
    if test_host_app_binary:
        data["test_host_app_binary"] = test_host_app_binary
    return data
