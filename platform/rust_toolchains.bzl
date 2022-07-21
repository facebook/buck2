load(
    "@fbcode//buck2/platform:utils.bzl",
    "attr_info",
    "binary_attr",
    "bool_attr",
    "flags_attr",
    "read_list_with_comma_as_delimiter",
    "string_attr",
)
load("@fbcode//buck2/prelude/rust:rust_toolchain.bzl", "RustPlatformInfo", "RustToolchainInfo")

DEFAULT_RUSTC_ACTION = "fbcode//buck2/prelude/rust/tools:rustc_action"
DEFAULT_FAILURE_FILTER_ACTION = "fbcode//buck2/prelude/rust/tools:failure_filter_action"

# Lints are comma-separated in config
_lint_attr = attr_info(reader = read_list_with_comma_as_delimiter, attr_type = attrs.list(attrs.arg()))

# TODO(scottcao): Add all attributes from rust and rust#... buckconfig.
_buckconfig_rust_toolchain_attrs = {
    "allow_lints": (_lint_attr, []),
    "clippy_driver": (binary_attr, None),
    "compiler": (binary_attr, None),
    "default_edition": (string_attr, None),
    "deny_lints": (_lint_attr, []),
    "extern_html_root_url_prefix": (string_attr, None),
    "failure_filter": (bool_attr, False),
    "pipelined": (bool_attr, False),
    "report_unused_deps": (bool_attr, False),
    "rustc_binary_flags": (flags_attr, []),
    "rustc_check_flags": (flags_attr, []),
    "rustc_flags": (flags_attr, None),
    "rustc_test_flags": (flags_attr, []),
    "rustdoc": (binary_attr, None),
    "rustdoc_flags": (flags_attr, None),
    "warn_lints": (_lint_attr, []),
}

def config_backed_rust_toolchain(flavor, **kwargs):
    sections = ["rust#" + flavor, "rust"]
    for (key, (info, default)) in _buckconfig_rust_toolchain_attrs.items():
        if key in kwargs:
            continue
        val = None
        for section in sections:
            val = info.reader(section, key)
            if val != None:
                break
        if val == None:
            val = default
        if val != None:
            kwargs[key] = val

    _config_backed_rust_toolchain_rule(
        name = "rust-" + flavor,
        **kwargs
    )

def _config_backed_rust_toolchain_rule_impl(ctx):
    return [
        DefaultInfo(),
        RustToolchainInfo(
            allow_lints = ctx.attrs.allow_lints,
            clippy_driver = ctx.attrs.clippy_driver[RunInfo],
            compiler = ctx.attrs.compiler[RunInfo],
            deny_lints = ctx.attrs.deny_lints,
            extern_html_root_url_prefix = ctx.attrs.extern_html_root_url_prefix,
            failure_filter = ctx.attrs.failure_filter,
            failure_filter_action = ctx.attrs.failure_filter_action,
            pipelined = ctx.attrs.pipelined,
            report_unused_deps = ctx.attrs.report_unused_deps,
            rustc_action = ctx.attrs.rustc_action,
            rustc_binary_flags = ctx.attrs.rustc_binary_flags,
            rustc_check_flags = ctx.attrs.rustc_check_flags,
            rustc_flags = ctx.attrs.rustc_flags,
            default_edition = ctx.attrs.default_edition,
            rustc_test_flags = ctx.attrs.rustc_test_flags,
            rustdoc = ctx.attrs.rustdoc[RunInfo],
            rustdoc_flags = ctx.attrs.rustdoc_flags,
            warn_lints = ctx.attrs.warn_lints,
        ),
        RustPlatformInfo(
            name = ctx.attrs.name,
        ),
    ]

_config_backed_rust_toolchain_rule = rule(
    impl = _config_backed_rust_toolchain_rule_impl,
    attrs = {
        "allow_lints": attrs.list(attrs.arg()),
        "clippy_driver": attrs.dep(default = "fbsource//xplat/rust/toolchain/current:clippy-driver"),
        "compiler": attrs.dep(default = "fbsource//xplat/rust/toolchain/current:rustc"),
        "default_edition": attrs.string(),
        "deny_lints": attrs.list(attrs.arg()),
        "extern_html_root_url_prefix": attrs.string(default = ""),
        "failure_filter": attrs.bool(),
        "failure_filter_action": attrs.dep(providers = [RunInfo], default = DEFAULT_FAILURE_FILTER_ACTION),
        "pipelined": attrs.bool(),
        "report_unused_deps": attrs.bool(),
        "rustc_action": attrs.dep(providers = [RunInfo], default = DEFAULT_RUSTC_ACTION),
        "rustc_binary_flags": attrs.list(attrs.arg()),
        "rustc_check_flags": attrs.list(attrs.arg()),
        "rustc_flags": attrs.list(attrs.arg()),
        "rustc_test_flags": attrs.list(attrs.arg()),
        "rustdoc": attrs.dep(default = "fbsource//xplat/rust/toolchain/current:rustdoc"),
        "rustdoc_flags": attrs.list(attrs.arg()),
        "warn_lints": attrs.list(attrs.arg()),
    },
)
