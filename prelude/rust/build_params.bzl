# Rules for mapping requirements to options

load(
    "@fbcode//buck2/prelude/linking:link_info.bzl",
    "LinkStyle",
    "Linkage",  # @unused Used as a type
)
load("@fbcode//buck2/prelude/utils:utils.bzl", "expect")

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

_BUILD_PARAMS = [
    BuildParams(crate_type = CrateType("bin"), reloc_model = RelocModel("pic"), dep_link_style = LinkStyle("shared"), prefix = "", suffix = ""),
    BuildParams(crate_type = CrateType("bin"), reloc_model = RelocModel("pic"), dep_link_style = LinkStyle("static_pic"), prefix = "", suffix = ""),
    BuildParams(crate_type = CrateType("bin"), reloc_model = RelocModel("static"), dep_link_style = LinkStyle("static"), prefix = "", suffix = ""),
    BuildParams(crate_type = CrateType("cdylib"), reloc_model = RelocModel("pic"), dep_link_style = LinkStyle("shared"), prefix = "lib", suffix = ".so"),
    BuildParams(crate_type = CrateType("dylib"), reloc_model = RelocModel("pic"), dep_link_style = LinkStyle("shared"), prefix = "lib", suffix = ".so"),
    BuildParams(crate_type = CrateType("proc-macro"), reloc_model = RelocModel("pic"), dep_link_style = LinkStyle("static_pic"), prefix = "lib", suffix = ".so"),
    BuildParams(crate_type = CrateType("rlib"), reloc_model = RelocModel("pic"), dep_link_style = LinkStyle("static_pic"), prefix = "lib", suffix = ".rlib"),
    BuildParams(crate_type = CrateType("rlib"), reloc_model = RelocModel("static"), dep_link_style = LinkStyle("static"), prefix = "lib", suffix = ".rlib"),
    BuildParams(crate_type = CrateType("staticlib"), reloc_model = RelocModel("pic"), dep_link_style = LinkStyle("static_pic"), prefix = "lib", suffix = "_pic.a"),
    BuildParams(crate_type = CrateType("staticlib"), reloc_model = RelocModel("static"), dep_link_style = LinkStyle("static"), prefix = "lib", suffix = ".a"),
]

_INPUTS = {
    # Binary, shared
    ("binary", False, "shared", "any", "rust"): _BUILD_PARAMS[0],
    ("binary", False, "shared", "shared", "rust"): _BUILD_PARAMS[0],
    ("binary", False, "shared", "static", "rust"): _BUILD_PARAMS[0],
    # Binary, PIE
    ("binary", False, "static_pic", "any", "rust"): _BUILD_PARAMS[1],
    ("binary", False, "static_pic", "shared", "rust"): _BUILD_PARAMS[1],
    ("binary", False, "static_pic", "static", "rust"): _BUILD_PARAMS[1],
    # Binary, non-PIE
    ("binary", False, "static", "any", "rust"): _BUILD_PARAMS[2],
    ("binary", False, "static", "shared", "rust"): _BUILD_PARAMS[2],
    ("binary", False, "static", "static", "rust"): _BUILD_PARAMS[2],
    # Native linkable shared object
    ("library", False, "shared", "any", "c++"): _BUILD_PARAMS[3],
    ("library", False, "shared", "shared", "c++"): _BUILD_PARAMS[3],
    ("library", False, "static", "shared", "c++"): _BUILD_PARAMS[3],
    ("library", False, "static_pic", "shared", "c++"): _BUILD_PARAMS[3],
    # Rust dylib shared object
    ("library", False, "shared", "any", "rust"): _BUILD_PARAMS[4],
    ("library", False, "shared", "shared", "rust"): _BUILD_PARAMS[4],
    ("library", False, "static", "shared", "rust"): _BUILD_PARAMS[4],
    ("library", False, "static_pic", "shared", "rust"): _BUILD_PARAMS[4],
    # Rust proc-macro
    ("library", True, "shared", "any", "rust"): _BUILD_PARAMS[5],
    ("library", True, "shared", "shared", "rust"): _BUILD_PARAMS[5],
    ("library", True, "shared", "static", "rust"): _BUILD_PARAMS[5],
    ("library", True, "static", "any", "rust"): _BUILD_PARAMS[5],
    ("library", True, "static", "shared", "rust"): _BUILD_PARAMS[5],
    ("library", True, "static", "static", "rust"): _BUILD_PARAMS[5],
    ("library", True, "static_pic", "any", "rust"): _BUILD_PARAMS[5],
    ("library", True, "static_pic", "shared", "rust"): _BUILD_PARAMS[5],
    ("library", True, "static_pic", "static", "rust"): _BUILD_PARAMS[5],
    # Rust static_pic library
    ("library", False, "shared", "static", "rust"): _BUILD_PARAMS[6],
    ("library", False, "static_pic", "any", "rust"): _BUILD_PARAMS[6],
    ("library", False, "static_pic", "static", "rust"): _BUILD_PARAMS[6],
    # Rust static (non-pic) library
    ("library", False, "static", "any", "rust"): _BUILD_PARAMS[7],
    ("library", False, "static", "static", "rust"): _BUILD_PARAMS[7],
    # Native linkable static_pic
    ("library", False, "shared", "static", "c++"): _BUILD_PARAMS[8],
    ("library", False, "static_pic", "any", "c++"): _BUILD_PARAMS[8],
    ("library", False, "static_pic", "static", "c++"): _BUILD_PARAMS[8],
    # Native linkable static non-pic
    ("library", False, "static", "any", "c++"): _BUILD_PARAMS[9],
    ("library", False, "static", "static", "c++"): _BUILD_PARAMS[9],
}

# Compute crate type, relocation model and name mapping given what rule we're building,
# whether its a proc-macro, linkage information and language.
def build_params(
        rule: RuleType.type,
        proc_macro: bool.type,
        link_style: LinkStyle.type,
        preferred_linkage: Linkage.type,
        lang: LinkageLang.type) -> BuildParams.type:
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

    return _INPUTS[input]
