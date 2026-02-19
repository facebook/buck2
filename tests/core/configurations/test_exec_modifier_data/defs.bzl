"""Custom rules for testing exec platform modifiers."""

def get_labels():
    """Returns labels based on build_mode and compiler constraints."""
    return select({
        "//cfg:build_mode[debug]": ["build_mode:debug"],
        "//cfg:build_mode[none]": ["build_mode:none"],
        "//cfg:build_mode[release]": ["build_mode:release"],
    }) + select({
        "//cfg:compiler[clang]": ["compiler:clang"],
        "//cfg:compiler[gcc]": ["compiler:gcc"],
        "//cfg:compiler[none]": ["compiler:none"],
    }) + select({
        "//cfg:os[linux]": ["os:linux"],
        "//cfg:os[macos]": ["os:macos"],
        "//cfg:os[none]": ["os:none"],
    }) + select({
        "//cfg:cpu[arm64]": ["cpu:arm64"],
        "//cfg:cpu[none]": ["cpu:none"],
        "//cfg:cpu[x86_64]": ["cpu:x86_64"],
    })

def get_buckconfig_backed_label():
    return select({
        "//cfg:buckconfig_backed[enabled]": "buckconfig_backed:enabled",
        "//cfg:buckconfig_backed[none]": "buckconfig_backed:none",
    })

def _dummy(ctx):
    _ignore = ctx
    return [
        DefaultInfo(),
    ]

# Rule that has dependencies and does nothing. Useful for query-like tests.
dummy = rule(
    impl = _dummy,
    attrs = {
        "buckconfig_backed_label": attrs.string(default = ""),
        "configured_deps": attrs.list(attrs.configured_dep(), default = []),
        "deps": attrs.list(attrs.dep(), default = []),
        "exec_deps": attrs.list(attrs.exec_dep(), default = []),
        "labels": attrs.list(attrs.string(), default = []),
        "srcs": attrs.list(attrs.source(), default = []),
        "toolchain_deps": attrs.list(attrs.toolchain_dep(), default = []),
    },
)

def labeled_dummy(name, **kwargs):
    """
    A wrapper around dummy that automatically sets labels based on constraints.

    Args:
        name: The name of the target
        **kwargs: Additional arguments passed to dummy()
    """

    # Set labels if not already provided
    if "labels" not in kwargs:
        kwargs["labels"] = get_labels()

    if "buckconfig_backed_label" not in kwargs:
        kwargs["buckconfig_backed_label"] = get_buckconfig_backed_label()

    # Set default_target_platform if not provided
    if "default_target_platform" not in kwargs:
        kwargs["default_target_platform"] = "//cfg:debug_platform"

    dummy(
        name = name,
        **kwargs
    )

def labeled_tool(name, **kwargs):
    """
    A wrapper around dummy specifically for build tools used as exec_deps.
    Automatically sets labels based on constraints.
    """

    # Set labels if not already provided
    if "labels" not in kwargs:
        kwargs["labels"] = get_labels()

    dummy(
        name = name,
        **kwargs
    )

def _toolchain_impl(ctx):
    _ignore = ctx
    return [
        DefaultInfo(),
    ]

# Toolchain rule - marked with is_toolchain_rule = True
toolchain = rule(
    impl = _toolchain_impl,
    attrs = {
        "additional_tools": attrs.list(attrs.exec_dep(), default = []),  # Additional tools for testing multiple exec deps
        "labels": attrs.list(attrs.string(), default = []),
        "tool": attrs.option(attrs.exec_dep(), default = None),  # The actual tool that runs
    },
    is_toolchain_rule = True,
)

def labeled_toolchain(name, **kwargs):
    """
    A wrapper around toolchain that automatically sets labels.
    """

    # Set labels if not already provided
    if "labels" not in kwargs:
        kwargs["labels"] = get_labels()

    toolchain(
        name = name,
        **kwargs
    )
