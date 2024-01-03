# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

# Rules for mapping requirements to options

load(
    "@prelude//linking:link_info.bzl",
    "LibOutputStyle",
    "LinkStrategy",
)
load("@prelude//os_lookup:defs.bzl", "OsLookup")
load("@prelude//utils:expect.bzl", "expect")

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
def crate_type_rust_linkage(crate_type: CrateType) -> bool:
    return crate_type.value in ("rlib", "dylib", "proc-macro")

# Crate type is intended for native linkage (eg C++)
def crate_type_native_linkage(crate_type: CrateType) -> bool:
    return crate_type.value in ("cdylib", "staticlib")

# Crate type which invokes the linker
def crate_type_linked(crate_type: CrateType) -> bool:
    return crate_type.value in ("bin", "dylib", "proc-macro", "cdylib")

# Crate type which should always need codegen
def crate_type_codegen(crate_type: CrateType) -> bool:
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
    "expand",  # pseudo emit alias for -Zunpretty=expanded
)

# Emitting this artifact generates code
def emit_needs_codegen(emit: Emit) -> bool:
    return emit.value in ("asm", "llvm-bc", "llvm-ir", "obj", "link", "mir")

# Represents a way of invoking rustc to produce an artifact. These values are computed from
# information such as the rule type, linkstyle, crate type, etc.
BuildParams = record(
    crate_type = field(CrateType),
    reloc_model = field(RelocModel),
    dep_link_strategy = field(LinkStrategy),
    # A prefix and suffix to use for the name of the produced artifact. Note that although we store
    # these in this type, they are in principle computable from the remaining fields and the OS.
    # Keeping them here just turns out to be a little more convenient.
    prefix = field(str),
    suffix = field(str),
)

RustcFlags = record(
    crate_type = field(CrateType),
    platform_to_affix = field(typing.Callable),
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
}

# Return the filename for a particular emitted artifact type
def output_filename(cratename: str, emit: Emit, buildparams: BuildParams, extra: [str, None] = None) -> str:
    epfx, esfx = _EMIT_PREFIX_SUFFIX[emit]
    prefix = epfx if epfx != None else buildparams.prefix
    suffix = esfx if esfx != None else buildparams.suffix
    return prefix + cratename + (extra or "") + suffix

# Rule type - 'binary' also covers 'test'
RuleType = enum("binary", "library")

# Controls how we build our rust libraries, largely dependent on whether rustc
# or buck is driving the final linking and whether we are linking the artifact
# into other rust targets.
#
# Rust: In this mode, we build rust libraries as rlibs. This is the primary
# approach for building rust targets when the final link step is driven by
# rustc (e.g. rust_binary, rust_unittest, etc).
#
# Native: In this mode, we build rust libraries as staticlibs, where rustc
# will bundle all of this target's rust dependencies into a single library
# artifact. This approach is the most standardized way to build rust libraries
# for linkage in non-rust code.
#
# NOTE: This approach does not scale well. It's possible to end up with
# non-rust target A depending on two rust targets B and C, which can cause
# duplicate symbols if B and C share common rust dependencies.
#
# Native Unbundled: In this mode, we revert back to building as rlibs. This
# approach mitigates the duplicate symbol downside of the "Native" approach.
# However, this option is not formally supported by rustc, and depends on an
# implementation detail of rlibs (they're effectively .a archives and can be
# linked with other native code using the CXX linker).
#
# See https://github.com/rust-lang/rust/issues/73632 for more details on
# stabilizing this approach.

LinkageLang = enum(
    "rust",
    "native",
    "native-unbundled",
)

_BINARY = 0
_NATIVE_LINKABLE_SHARED_OBJECT = 3
_RUST_DYLIB_SHARED = 4
_RUST_PROC_MACRO = 5
_RUST_STATIC_PIC_LIBRARY = 6
_RUST_STATIC_NON_PIC_LIBRARY = 7
_NATIVE_LINKABLE_STATIC_PIC = 8
_NATIVE_LINKABLE_STATIC_NON_PIC = 9

def _executable_prefix_suffix(linker_type: str, target_os_type: OsLookup) -> (str, str):
    return {
        "darwin": ("", ""),
        "gnu": ("", ".exe") if target_os_type.platform == "windows" else ("", ""),
        "wasm": ("", ".wasm"),
        "windows": ("", ".exe"),
    }[linker_type]

def _library_prefix_suffix(linker_type: str, target_os_type: OsLookup) -> (str, str):
    return {
        "darwin": ("lib", ".dylib"),
        "gnu": ("", ".dll") if target_os_type.platform == "windows" else ("lib", ".so"),
        "wasm": ("", ".wasm"),
        "windows": ("", ".dll"),
    }[linker_type]

_BUILD_PARAMS = {
    _BINARY: RustcFlags(
        crate_type = CrateType("bin"),
        platform_to_affix = _executable_prefix_suffix,
    ),
    _NATIVE_LINKABLE_SHARED_OBJECT: RustcFlags(
        crate_type = CrateType("cdylib"),
        platform_to_affix = _library_prefix_suffix,
    ),
    _RUST_DYLIB_SHARED: RustcFlags(
        crate_type = CrateType("dylib"),
        platform_to_affix = _library_prefix_suffix,
    ),
    _RUST_PROC_MACRO: RustcFlags(
        crate_type = CrateType("proc-macro"),
        platform_to_affix = _library_prefix_suffix,
    ),
    # FIXME(JakobDegen): Add a comment explaining why `.a`s need reloc-strategy
    # dependent names while `.rlib`s don't.
    _RUST_STATIC_PIC_LIBRARY: RustcFlags(
        crate_type = CrateType("rlib"),
        platform_to_affix = lambda _l, _t: ("lib", ".rlib"),
    ),
    _RUST_STATIC_NON_PIC_LIBRARY: RustcFlags(
        crate_type = CrateType("rlib"),
        platform_to_affix = lambda _l, _t: ("lib", ".rlib"),
    ),
    _NATIVE_LINKABLE_STATIC_PIC: RustcFlags(
        crate_type = CrateType("staticlib"),
        platform_to_affix = lambda _l, _t: ("lib", "_pic.a"),
    ),
    _NATIVE_LINKABLE_STATIC_NON_PIC: RustcFlags(
        crate_type = CrateType("staticlib"),
        platform_to_affix = lambda _l, _t: ("lib", ".a"),
    ),
}

_INPUTS = {
    # Binary
    ("binary", False, None, "rust"): _BINARY,
    # Native linkable shared object
    ("library", False, "shared_lib", "native"): _NATIVE_LINKABLE_SHARED_OBJECT,
    # Native unbundled linkable shared object
    ("library", False, "shared_lib", "native-unbundled"): _RUST_DYLIB_SHARED,
    # Rust dylib shared object
    ("library", False, "shared_lib", "rust"): _RUST_DYLIB_SHARED,
    # Rust proc-macro
    ("library", True, "archive", "rust"): _RUST_PROC_MACRO,
    ("library", True, "pic_archive", "rust"): _RUST_PROC_MACRO,
    ("library", True, "shared_lib", "rust"): _RUST_PROC_MACRO,
    # Rust static_pic library
    ("library", False, "pic_archive", "rust"): _RUST_STATIC_PIC_LIBRARY,
    # Rust static (non-pic) library
    ("library", False, "archive", "rust"): _RUST_STATIC_NON_PIC_LIBRARY,
    # Native linkable static_pic
    ("library", False, "pic_archive", "native"): _NATIVE_LINKABLE_STATIC_PIC,
    # Native linkable static non-pic
    ("library", False, "archive", "native"): _NATIVE_LINKABLE_STATIC_NON_PIC,
    # Native Unbundled static_pic library
    ("library", False, "pic_archive", "native-unbundled"): _RUST_STATIC_PIC_LIBRARY,
    # Native Unbundled static (non-pic) library
    ("library", False, "archive", "native-unbundled"): _RUST_STATIC_NON_PIC_LIBRARY,
}

# Check types of _INPUTS, writing these out as types is too verbose, but let's make sure we don't have any typos.
[
    (RuleType(rule_type), LibOutputStyle(lib_output_style) if lib_output_style else None, LinkageLang(linkage_lang))
    for (rule_type, _, lib_output_style, linkage_lang), _ in _INPUTS.items()
]

def _get_reloc_model(link_strategy: LinkStrategy, target_os_type: OsLookup) -> RelocModel:
    if target_os_type.platform == "windows":
        return RelocModel("pic")
    if link_strategy == LinkStrategy("static"):
        return RelocModel("static")
    return RelocModel("pic")

# Compute crate type, relocation model and name mapping given what rule we're building, whether its
# a proc-macro, linkage information and language.
#
# Binaries should pass the link strategy and not the lib output style, while libraries should do the
# opposite.
#
# The linking information that's passed here is different from what one might expect in the C++
# rules. There's a good reason for that, so let's go over it. First, let's recap how C++ handles
# this, as of December 2023 (I say "recap" but I don't think this is actually documented anywhere):
#
#  1. C++ libraries can be built in three different ways: Archives, pic archives, and shared
#     libraries. Which one of these is used for a given link strategy is determined by the preferred
#     linkage using `linking/link_info.bzl:get_lib_output_style`.
#  2. When a C++ library is built as a shared library, the link strategy used for its dependencies
#     is determined by the link style attribute on the C++ library.
#  3. When a C++ library is built as an archive (either kind), there's no need to know a link
#     strategy for the dependencies. None of the per-link-strategy providers of the dependencies
#     need to be accessed.
#
# There are two relevant ways in which Rust differs:
#
#  1. There are more ways of building Rust libraries than are represented by `LibOutputStyle`. The
#     Rust analogue is the `BuildParams` type, which implicitly holds a `LibOutputStyle` as well as
#     a bunch of additional information - this is why `LibOutputStyle` is relatively rarely used
#     directly in the Rust rules.
#  2. Rust does not have the property in point three above, ie building a Rust library into an
#     archive does require knowing per-link-strategy properties of the dependencies. This is
#     fundamental in cases without native unbundled deps - with native unbundled deps it may be
#     fixable, but that's not super clear.
def build_params(
        rule: RuleType,
        proc_macro: bool,
        link_strategy: LinkStrategy | None,
        lib_output_style: LibOutputStyle | None,
        lang: LinkageLang,
        linker_type: str,
        target_os_type: OsLookup) -> BuildParams:
    if rule == RuleType("binary"):
        expect(link_strategy != None)
        expect(lib_output_style == None)
    else:
        expect(link_strategy == None)
        expect(lib_output_style != None)

    # FIXME(JakobDegen): We deal with Rust needing to know the link strategy
    # even for building archives by using a default link strategy specifically
    # for those cases. I've gone through the code and checked all the places
    # where the link strategy is used to determine that this won't do anything
    # too bad, but it would be nice to enforce that more strictly or not have
    # this at all.
    def default_link_strategy_for_output_style(output_style: LibOutputStyle) -> LinkStrategy:
        if output_style == LibOutputStyle("archive"):
            return LinkStrategy("static")
        if output_style == LibOutputStyle("pic_archive"):
            return LinkStrategy("static_pic")

        # Rust does not have the `link_style` attribute on libraries in the same
        # way that C++ does - if it did, this is what it would affect.
        return LinkStrategy("shared")

    if not link_strategy:
        if proc_macro:
            # FIXME(JakobDegen): It's not really clear what we should do about
            # proc macros. The principled thing is probably to treat them sort
            # of like a normal library, except that they always have preferred
            # linkage shared? Preserve existing behavior for now
            link_strategy = LinkStrategy("static_pic")
        else:
            link_strategy = default_link_strategy_for_output_style(lib_output_style)

    if rule == RuleType("binary") and proc_macro:
        # It's complicated: this is a rustdoc test for a procedural macro crate.
        # We need deps built as if this were a binary, while passing crate-type
        # proc_macro to the rustdoc invocation.
        crate_type = CrateType("proc-macro")
        proc_macro = False
    else:
        crate_type = None

    input = (rule.value, proc_macro, lib_output_style.value if lib_output_style else None, lang.value)

    expect(
        input in _INPUTS,
        "missing case for rule_type={} proc_macro={} lib_output_style={} lang={}",
        rule,
        proc_macro,
        lib_output_style,
        lang,
    )

    flags = _BUILD_PARAMS[_INPUTS[input]]
    reloc_model = _get_reloc_model(link_strategy, target_os_type)
    prefix, suffix = flags.platform_to_affix(linker_type, target_os_type)

    return BuildParams(
        crate_type = crate_type or flags.crate_type,
        reloc_model = reloc_model,
        dep_link_strategy = link_strategy,
        prefix = prefix,
        suffix = suffix,
    )
