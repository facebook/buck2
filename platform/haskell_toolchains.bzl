load("@fbcode//buck2/platform:utils.bzl", "bool_attr", "flags_attr", "int_attr", "string_attr")
load("@fbcode//buck2/prelude/haskell:haskell.bzl", "HaskellPlatformInfo", "HaskellToolchainInfo")

# Got these attributes from
# 'fbcode/tools/buckconfigs/fbcode/modes/dev-sand.bcfg'
# ([haskell#platform009-clang])
_buckconfig_haskell_toolchain_attrs = {
    "archive_contents": (string_attr, ""),
    "cache_links": (bool_attr, True),
    "compiler": (string_attr, ""),
    "compiler_flags": (flags_attr, None),
    "compiler_major_version": (int_attr, None),
    "ghci_binutils_path": (string_attr, ""),
    "ghci_cc_path": (string_attr, ""),
    "ghci_cpp_path": (string_attr, ""),
    "ghci_cxx_path": (string_attr, ""),
    "ghci_ghc_path": (string_attr, ""),
    "ghci_iserv_path": (string_attr, ""),
    "ghci_iserv_prof_path": (string_attr, ""),
    "ghci_iserv_template": (string_attr, ""),
    "ghci_lib_path": (string_attr, ""),
    "ghci_packager": (string_attr, ""),
    "ghci_script_template": (string_attr, ""),
    "haddock": (string_attr, ""),
    "ide_script_template": (string_attr, ""),
    "linker": (string_attr, ""),
    "linker_flags": (flags_attr, None),
    "package_name_prefix": (string_attr, ""),
    "packager": (string_attr, ""),
    "support_expose_package": (bool_attr, False),
    "use_argsfile": (bool_attr, False),
}

# The current config value for compiler_flags contain -package-db links which point at untracked local files.
# We could replace them by $(location) macros, but in fact the package databases they point at
# are unnecessary as these database are referred to by the .db field of prebuilt Haskell libraries,
# which is the right place to get these values from.
def _remove_package_db(xs: [str.type]) -> [str.type]:
    res = []
    skip_next = False
    for x in xs:
        if x == "-package-db":
            skip_next = True
        elif skip_next:
            skip_next = False
        else:
            res.append(x)
    return res

def config_backed_haskell_toolchain(flavor, **kwargs):
    sections = ["haskell#" + flavor, "haskell"]
    for (key, (info, default)) in _buckconfig_haskell_toolchain_attrs.items():
        if key in kwargs:
            continue
        val = None
        for section in sections:
            val = info.reader(section, key)
            if val != None:
                break
        if val == None:
            val = default
        if key == "compiler_flags":
            val = _remove_package_db(val)
        elif key == "packager":
            # The buckconfig packager points at a file, using ./third-party-buck.
            # That's wrong for two reasons: 1) relative path to the wrong location;
            # 2) package boundary violation. Replace it with what it should be.
            # FIXME: Should we regenerate the buckconfig?
            val = "fbcode//third-party-buck/platform009/build/ghc:bin/ghc-pkg"
        kwargs[key] = val

    _config_backed_haskell_toolchain_rule(
        name = "haskell-" + flavor,
        **kwargs
    )

def _config_backed_haskell_toolchain_rule_impl(ctx):
    return [
        DefaultInfo(),
        HaskellToolchainInfo(
            compiler = ctx.attrs.compiler,
            compiler_flags = ctx.attrs.compiler_flags,
            linker = ctx.attrs.linker,
            linker_flags = ctx.attrs.linker_flags,
            haddock = ctx.attrs.haddock,
            compiler_major_version = ctx.attrs.compiler_major_version,
            package_name_prefix = ctx.attrs.package_name_prefix,
            packager = ctx.attrs.packager,
            use_argsfile = ctx.attrs.use_argsfile,
            support_expose_package = ctx.attrs.support_expose_package,
            archive_contents = ctx.attrs.archive_contents,
            ghci_script_template = ctx.attrs.ghci_script_template,
            ghci_iserv_template = ctx.attrs.ghci_iserv_template,
            ide_script_template = ctx.attrs.ide_script_template,
            ghci_binutils_path = ctx.attrs.ghci_binutils_path,
            ghci_lib_path = ctx.attrs.ghci_lib_path,
            ghci_ghc_path = ctx.attrs.ghci_ghc_path,
            ghci_iserv_path = ctx.attrs.ghci_iserv_path,
            ghci_iserv_prof_path = ctx.attrs.ghci_iserv_prof_path,
            ghci_cxx_path = ctx.attrs.ghci_cxx_path,
            ghci_cc_path = ctx.attrs.ghci_cc_path,
            ghci_cpp_path = ctx.attrs.ghci_cpp_path,
            ghci_packager = ctx.attrs.ghci_packager,
            cache_links = ctx.attrs.cache_links,
        ),
        HaskellPlatformInfo(
            name = ctx.attrs.name,
        ),
    ]

_config_backed_haskell_toolchain_rule = rule(
    impl = _config_backed_haskell_toolchain_rule_impl,
    attrs = {
        "archive_contents": attrs.arg(),
        "cache_links": attrs.bool(),
        "compiler": attrs.dep(providers = [RunInfo]),
        "compiler_flags": attrs.list(attrs.arg()),
        "compiler_major_version": attrs.int(),
        "ghci_binutils_path": attrs.string(),
        "ghci_cc_path": attrs.string(),
        "ghci_cpp_path": attrs.string(),
        "ghci_cxx_path": attrs.string(),
        "ghci_ghc_path": attrs.string(),
        "ghci_iserv_path": attrs.string(),
        "ghci_iserv_prof_path": attrs.string(),
        "ghci_iserv_template": attrs.string(),
        "ghci_lib_path": attrs.string(),
        "ghci_packager": attrs.string(),
        "ghci_script_template": attrs.string(),
        "haddock": attrs.dep(providers = [RunInfo]),
        "ide_script_template": attrs.string(),
        "linker": attrs.dep(providers = [RunInfo]),
        "linker_flags": attrs.list(attrs.arg()),
        "package_name_prefix": attrs.string(),
        "packager": attrs.source(),
        "support_expose_package": attrs.bool(),
        "use_argsfile": attrs.bool(),
    },
)
