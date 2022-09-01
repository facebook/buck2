# Rules for mapping requirements to options

load(
    "@prelude//linking:link_info.bzl",
    "LinkStyle",
    "Linkage",  # @unused Used as a type
)
load("@prelude//utils:utils.bzl", "expect")

# --crate-type=
# Excludes `lib` because we want to explicitly choose the library flavour
CrateType = enum(
    # Binary
    "bin",
    # Rust linkage
    "rlib",
    "dylib",
    "proc-macro",
    # Native linkage
    "cdylib",
    "staticlib",
)

# Crate type is intended for consumption by Rust code
def crate_type_rust_linkage(crate_type: CrateType.type) -> bool.type:
    return crate_type.value in ("rlib", "dylib", "proc-macro")

# Crate type is intended for native linkage (eg C++)
def crate_type_native_linkage(crate_type: CrateType.type) -> bool.type:
    return crate_type.value in ("cdylib", "staticlib")

# Crate type which invokes the linker
def crate_type_linked(crate_type: CrateType.type) -> bool.type:
    return crate_type.value in ("bin", "dylib", "proc-macro", "cdylib")

# Crate type which should include transitive deps
def crate_type_transitive_deps(crate_type: CrateType.type) -> bool.type:
    return crate_type.value in ("rlib", "dylib", "staticlib")  # not sure about staticlib

# Crate type which should always need codegen
def crate_type_codegen(crate_type: CrateType.type) -> bool.type:
    return crate_type_linked(crate_type) or crate_type_native_linkage(crate_type)

# -Crelocation-model= from --print relocation-models
RelocModel = enum(
    # Common
    "static",
    "pic",
    # Various obscure types
    "dynamic-no-pic",
    "ropi",
    "rwpi",
    "ropi-rwpi",
    "default",
)

# --emit=
Emit = enum(
    "asm",
    "llvm-bc",
    "llvm-ir",
    "obj",
    "metadata",
    "link",
    "dep-info",
    "mir",
    "save-analysis",  # pseudo emit alias for metadata + -Zsave-analysis
    "expand",  # pseudo emit alias for -Zunpretty=expanded
)

# Emitting this artifact generates code
def emit_needs_codegen(emit: Emit.type) -> bool.type:
    return emit.value in ("asm", "llvm-bc", "llvm-ir", "obj", "link", "mir")

BuildParams = record(
    crate_type = field(CrateType.type),
    reloc_model = field(RelocModel.type),
    dep_link_style = field(LinkStyle.type),  # what link_style to use for dependencies
    # XXX This needs to be OS-specific
    prefix = field(str.type),
    suffix = field(str.type),
)

RustcFlags = record(
    crate_type = field(CrateType.type),
    reloc_model = field(RelocModel.type),
    dep_link_style = field(LinkStyle.type),
)

# Filenames used for various emitted forms
# `None` for a prefix or suffix means use the build_param version
_EMIT_PREFIX_SUFFIX = {
    Emit("asm"): ("", ".s"),
    Emit("llvm-bc"): ("", ".bc"),
    Emit("llvm-ir"): ("", ".ll"),
    Emit("obj"): ("", ".o"),
    Emit("metadata"): ("lib", ".rmeta"),  # even binaries get called 'libfoo.rmeta'
    Emit("link"): (None, None),  # crate type and reloc model dependent
    Emit("dep-info"): ("", ".d"),
    Emit("mir"): (None, ".mir"),
    Emit("expand"): (None, ".rs"),
    Emit("save-analysis"): (None, ".json"),
}

# Return the filename for a particular emitted artifact type
def output_filename(cratename: str.type, emit: Emit.type, buildparams: BuildParams.type, extra: [str.type, None] = None) -> str.type:
    epfx, esfx = _EMIT_PREFIX_SUFFIX[emit]
    prefix = epfx if epfx != None else buildparams.prefix
    suffix = esfx if esfx != None else buildparams.suffix
    return prefix + cratename + (extra or "") + suffix

# Rule type - 'binary' also covers 'test'
RuleType = enum("binary", "library")

# What language we're generating artifacts to be linked with
LinkageLang = enum("rust", "c++")

_BINARY_SHARED = 0
_BINARY_PIE = 1
_BINARY_NON_PIE = 2
_NATIVE_LINKABLE_SHARED_OBJECT = 3
_RUST_DYLIB_SHARED = 4
_RUST_PROC_MACRO = 5
_RUST_STATIC_PIC_LIBRARY = 6
_RUST_STATIC_NON_PIC_LIBRARY = 7
_NATIVE_LINKABLE_STATIC_PIC = 8
_NATIVE_LINKABLE_STATIC_NON_PIC = 9

_BUILD_PARAMS = {
    _BINARY_SHARED: (RustcFlags(crate_type = CrateType("bin"), reloc_model = RelocModel("pic"), dep_link_style = LinkStyle("shared")), lambda _: ("", "")),
    _BINARY_PIE: (RustcFlags(crate_type = CrateType("bin"), reloc_model = RelocModel("pic"), dep_link_style = LinkStyle("static_pic")), lambda _: ("", "")),
    _BINARY_NON_PIE: (RustcFlags(crate_type = CrateType("bin"), reloc_model = RelocModel("static"), dep_link_style = LinkStyle("static")), lambda _: ("", "")),
    _NATIVE_LINKABLE_SHARED_OBJECT: (RustcFlags(crate_type = CrateType("cdylib"), reloc_model = RelocModel("pic"), dep_link_style = LinkStyle("shared")), lambda platform: {
        "darwin": ("", ".dylib"),
        "gnu": ("lib", ".so"),
        "windows": ("", ".dll"),
    }[platform]),
    _RUST_DYLIB_SHARED: (RustcFlags(crate_type = CrateType("dylib"), reloc_model = RelocModel("pic"), dep_link_style = LinkStyle("shared")), lambda platform: {
        "darwin": ("lib", ".dylib"),
        "gnu": ("lib", ".so"),
        "windows": ("", ".dll"),
    }[platform]),
    _RUST_PROC_MACRO: (RustcFlags(crate_type = CrateType("proc-macro"), reloc_model = RelocModel("pic"), dep_link_style = LinkStyle("static_pic")), lambda platform: {
        "darwin": ("lib", ".dylib"),
        "gnu": ("lib", ".so"),
        "windows": ("", ".dll"),
    }[platform]),
    _RUST_STATIC_PIC_LIBRARY: (RustcFlags(crate_type = CrateType("rlib"), reloc_model = RelocModel("pic"), dep_link_style = LinkStyle("static_pic")), lambda _: ("lib", ".rlib")),
    _RUST_STATIC_NON_PIC_LIBRARY: (RustcFlags(crate_type = CrateType("rlib"), reloc_model = RelocModel("static"), dep_link_style = LinkStyle("static")), lambda _: ("lib", ".rlib")),
    _NATIVE_LINKABLE_STATIC_PIC: (RustcFlags(crate_type = CrateType("staticlib"), reloc_model = RelocModel("pic"), dep_link_style = LinkStyle("static_pic")), lambda _: ("lib", "_pic.a")),
    _NATIVE_LINKABLE_STATIC_NON_PIC: (RustcFlags(crate_type = CrateType("staticlib"), reloc_model = RelocModel("static"), dep_link_style = LinkStyle("static")), lambda _: ("lib", ".a")),
}

_INPUTS = {
    # Binary, shared
    ("binary", False, "shared", "any", "rust"): _BINARY_SHARED,
    ("binary", False, "shared", "shared", "rust"): _BINARY_SHARED,
    ("binary", False, "shared", "static", "rust"): _BINARY_SHARED,
    # Binary, PIE
    ("binary", False, "static_pic", "any", "rust"): _BINARY_PIE,
    ("binary", False, "static_pic", "shared", "rust"): _BINARY_PIE,
    ("binary", False, "static_pic", "static", "rust"): _BINARY_PIE,
    # Binary, non-PIE
    ("binary", False, "static", "any", "rust"): _BINARY_NON_PIE,
    ("binary", False, "static", "shared", "rust"): _BINARY_NON_PIE,
    ("binary", False, "static", "static", "rust"): _BINARY_NON_PIE,
    # Native linkable shared object
    ("library", False, "shared", "any", "c++"): _NATIVE_LINKABLE_SHARED_OBJECT,
    ("library", False, "shared", "shared", "c++"): _NATIVE_LINKABLE_SHARED_OBJECT,
    ("library", False, "static", "shared", "c++"): _NATIVE_LINKABLE_SHARED_OBJECT,
    ("library", False, "static_pic", "shared", "c++"): _NATIVE_LINKABLE_SHARED_OBJECT,
    # Rust dylib shared object
    ("library", False, "shared", "any", "rust"): _RUST_DYLIB_SHARED,
    ("library", False, "shared", "shared", "rust"): _RUST_DYLIB_SHARED,
    ("library", False, "static", "shared", "rust"): _RUST_DYLIB_SHARED,
    ("library", False, "static_pic", "shared", "rust"): _RUST_DYLIB_SHARED,
    # Rust proc-macro
    ("library", True, "shared", "any", "rust"): _RUST_PROC_MACRO,
    ("library", True, "shared", "shared", "rust"): _RUST_PROC_MACRO,
    ("library", True, "shared", "static", "rust"): _RUST_PROC_MACRO,
    ("library", True, "static", "any", "rust"): _RUST_PROC_MACRO,
    ("library", True, "static", "shared", "rust"): _RUST_PROC_MACRO,
    ("library", True, "static", "static", "rust"): _RUST_PROC_MACRO,
    ("library", True, "static_pic", "any", "rust"): _RUST_PROC_MACRO,
    ("library", True, "static_pic", "shared", "rust"): _RUST_PROC_MACRO,
    ("library", True, "static_pic", "static", "rust"): _RUST_PROC_MACRO,
    # Rust static_pic library
    ("library", False, "shared", "static", "rust"): _RUST_STATIC_PIC_LIBRARY,
    ("library", False, "static_pic", "any", "rust"): _RUST_STATIC_PIC_LIBRARY,
    ("library", False, "static_pic", "static", "rust"): _RUST_STATIC_PIC_LIBRARY,
    # Rust static (non-pic) library
    ("library", False, "static", "any", "rust"): _RUST_STATIC_NON_PIC_LIBRARY,
    ("library", False, "static", "static", "rust"): _RUST_STATIC_NON_PIC_LIBRARY,
    # Native linkable static_pic
    ("library", False, "shared", "static", "c++"): _NATIVE_LINKABLE_STATIC_PIC,
    ("library", False, "static_pic", "any", "c++"): _NATIVE_LINKABLE_STATIC_PIC,
    ("library", False, "static_pic", "static", "c++"): _NATIVE_LINKABLE_STATIC_PIC,
    # Native linkable static non-pic
    ("library", False, "static", "any", "c++"): _NATIVE_LINKABLE_STATIC_NON_PIC,
    ("library", False, "static", "static", "c++"): _NATIVE_LINKABLE_STATIC_NON_PIC,
}

# Compute crate type, relocation model and name mapping given what rule we're building,
# whether its a proc-macro, linkage information and language.
def build_params(
        rule: RuleType.type,
        proc_macro: bool.type,
        link_style: LinkStyle.type,
        preferred_linkage: Linkage.type,
        lang: LinkageLang.type,
        linker_type: str.type) -> BuildParams.type:
    input = (rule.value, proc_macro, link_style.value, preferred_linkage.value, lang.value)

    expect(
        input in _INPUTS,
        "missing case for rule_type={} proc_macro={} link_style={} preferred_linkage={} lang={}",
        rule,
        proc_macro,
        link_style,
        preferred_linkage,
        lang,
    )

    build_kind_key = _INPUTS[input]
    (flags, platform_to_affix) = _BUILD_PARAMS[build_kind_key]
    (prefix, suffix) = platform_to_affix(linker_type)

    return BuildParams(
        crate_type = flags.crate_type,
        reloc_model = flags.reloc_model,
        dep_link_style = flags.dep_link_style,
        prefix = prefix,
        suffix = suffix,
    )
