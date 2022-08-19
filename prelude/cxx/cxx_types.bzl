load(
    ":compile.bzl",
    "CxxSrcWithFlags",  # @unused Used as a type
)
load(
    ":headers.bzl",
    "CxxHeadersLayout",
)
load(
    ":linker.bzl",
    "SharedLibraryFlagOverrides",
)
load(
    ":preprocessor.bzl",
    "CPreprocessor",
)
load(
    ":xcode.bzl",
    "cxx_populate_xcode_attributes",
)

# Parameters to control which sub targets to define when processing Cxx rules.
# By default, generates all subtargets.
CxxRuleSubTargetParams = record(
    argsfiles = field(bool.type, True),
    compilation_database = field(bool.type, True),
    headers = field(bool.type, True),
    link_group_map = field(bool.type, True),
    link_style_outputs = field(bool.type, True),
    xcode_data = field(bool.type, True),
)

# Parameters to control which providers to define when processing Cxx rules.
# By default, generates all providers.
CxxRuleProviderParams = record(
    compilation_database = field(bool.type, True),
    default = field(bool.type, True),
    java_packaging_info = field(bool.type, True),
    android_packageable_info = field(bool.type, True),
    linkable_graph = field(bool.type, True),
    link_style_outputs = field(bool.type, True),
    merged_native_link_info = field(bool.type, True),
    omnibus_root = field(bool.type, True),
    preprocessors = field(bool.type, True),
    resources = field(bool.type, True),
    shared_libraries = field(bool.type, True),
    template_placeholders = field(bool.type, True),
    preprocessor_for_tests = field(bool.type, True),
)

# Parameters to handle non-Clang sources, e.g Swift on Apple's platforms.
CxxRuleAdditionalParams = record(
    srcs = field([CxxSrcWithFlags.type], []),
)

# Parameters that allows to configure/extend generic implementation of C++ rules.
# Apple-specific rules (such as `apple_binary` and `apple_library`) and regular C++
# rules (such as `cxx_binary` and `cxx_library`) have too much in common, though
# some aspects of behavior (like layout of headers affecting inclusion of those
# or additional linking flags to support usage of platform frameworks) of are
# different and need to be specified. The following record holds the data which
# is needed to specialize user-facing rule from generic implementation.
CxxRuleConstructorParams = record(
    # We need to build an empty shared library for rust_python_extensions so that they can link against the rust shared object
    build_empty_so = field(bool.type, False),
    # Name of the top level rule utilizing the cxx rule.
    rule_type = str.type,
    # If the rule is a test.
    is_test = field(bool.type, False),
    # Header layout to use importing headers.
    headers_layout = CxxHeadersLayout.type,
    # Additional information used to preprocess every unit of translation in the rule
    extra_preprocessors = field([CPreprocessor.type], []),
    # Additional preprocessor info to export to other rules
    extra_exported_preprocessors = field([CPreprocessor.type], []),
    # Additional information used to link every object produced by the rule,
    # flags are _both_ exported and used to link the target itself.
    extra_exported_link_flags = field([""], []),
    # Additional flags used _only_ when linking the target itself.
    # These flags are _not_ propagated up the dep tree.
    extra_link_flags = field([""], []),
    # Additional artifacts to be linked together with the cxx compilation output
    extra_link_input = field(["artifact"], []),
    # The source files to compile as part of this rule. This list can be generated
    # from ctx.attrs with the `get_srcs_with_flags` function.
    srcs = field([CxxSrcWithFlags.type]),
    additional = field(CxxRuleAdditionalParams.type, CxxRuleAdditionalParams()),
    # A function which enables the caller to inject subtargets into the link_style provider
    # as well as create custom providers based on the link styles.
    link_style_sub_targets_and_providers_factory = field("function", lambda _link_style, _context, _executable, _object_files: ({}, [])),
    # Optinal postprocessor used to post postprocess the artifacts
    link_postprocessor = field(["cmd_args", None], None),
    # Linker flags that tell the linker to create shared libraries, overriding the default shared library flags.
    # e.g. when building Apple tests, we want to link with `-bundle` instead of `-shared` to allow
    # linking against the bundle loader.
    shared_library_flags = field([SharedLibraryFlagOverrides.type, None], None),
    # If passed to cxx_library_parameterized, this field will be used to determine
    # a shared subtarget's default output should be stripped.
    #
    # If passed to cxx_executable, this field will be used to determine
    # a shared subtarget's default output should be stripped.
    strip_executable = field(bool.type, False),
    strip_args_factory = field("function", lambda _: cmd_args()),
    # Wether to embed the library name as the SONAME.
    use_soname = field(bool.type, True),
    # Use link group's linking logic regardless whether a link group map's present.
    force_link_group_linking = field(bool.type, False),
    # Function to use for setting Xcode attributes for the Xcode data sub target.
    cxx_populate_xcode_attributes_func = field("function", cxx_populate_xcode_attributes),
    # Define which sub targets to generate.
    generate_sub_targets = field(CxxRuleSubTargetParams.type, CxxRuleSubTargetParams()),
    # Define which providers to generate.
    generate_providers = field(CxxRuleProviderParams.type, CxxRuleProviderParams()),
)
