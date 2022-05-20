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
_lint_attr = attr_info(reader = read_list_with_comma_as_delimiter, attr_type = attr.list(attr.arg()))

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
    "save_analysis": (bool_attr, True),
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
            allow_lints = ctx.attr.allow_lints,
            clippy_driver = ctx.attr.clippy_driver[RunInfo],
            compiler = ctx.attr.compiler[RunInfo],
            deny_lints = ctx.attr.deny_lints,
            extern_html_root_url_prefix = ctx.attr.extern_html_root_url_prefix,
            failure_filter = ctx.attr.failure_filter,
            failure_filter_action = ctx.attr.failure_filter_action,
            pipelined = ctx.attr.pipelined,
            report_unused_deps = ctx.attr.report_unused_deps,
            rustc_action = ctx.attr.rustc_action,
            rustc_binary_flags = ctx.attr.rustc_binary_flags,
            rustc_check_flags = ctx.attr.rustc_check_flags,
            rustc_flags = ctx.attr.rustc_flags,
            default_edition = ctx.attr.default_edition,
            rustc_test_flags = ctx.attr.rustc_test_flags,
            rustdoc = ctx.attr.rustdoc[RunInfo],
            save_analysis = ctx.attr.save_analysis,
            warn_lints = ctx.attr.warn_lints,
        ),
        RustPlatformInfo(
            name = ctx.attr.name,
        ),
    ]

_config_backed_rust_toolchain_rule = rule(
    implementation = _config_backed_rust_toolchain_rule_impl,
    attrs = {
        "allow_lints": attr.list(attr.arg()),
        "clippy_driver": attr.dep(default = "fbsource//xplat/rust/toolchain/current:clippy-driver"),
        "compiler": attr.dep(default = "fbsource//xplat/rust/toolchain/current:rustc"),
        "default_edition": attr.string(),
        "deny_lints": attr.list(attr.arg()),
        "extern_html_root_url_prefix": attr.string(default = ""),
        "failure_filter": attr.bool(),
        "failure_filter_action": attr.dep(providers = [RunInfo], default = DEFAULT_FAILURE_FILTER_ACTION),
        "pipelined": attr.bool(),
        "report_unused_deps": attr.bool(),
        "rustc_action": attr.dep(providers = [RunInfo], default = DEFAULT_RUSTC_ACTION),
        "rustc_binary_flags": attr.list(attr.arg()),
        "rustc_check_flags": attr.list(attr.arg()),
        "rustc_flags": attr.list(attr.arg()),
        "rustc_test_flags": attr.list(attr.arg()),
        "rustdoc": attr.dep(default = "fbsource//xplat/rust/toolchain/current:rustdoc"),
        "save_analysis": attr.bool(),
        "warn_lints": attr.list(attr.arg()),
    },
)
