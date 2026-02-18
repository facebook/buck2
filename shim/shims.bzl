# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

load("@bazel_skylib//lib:paths.bzl", "paths")
load("@prelude//utils:buckconfig.bzl", "read_bool")
load("@prelude//utils:selects.bzl", "selects")
# @lint-ignore-every FBCODEBZLADDLOADS

load("@prelude//utils:type_defs.bzl", "is_dict", "is_list", "is_select", "is_tuple")
load("@shim//build_defs:auto_headers.bzl", "AutoHeaders", "get_auto_headers")
load("@shim//build_defs/lib:oss.bzl", "translate_target")

prelude = native

_C_SOURCE_EXTS = (
    ".c",
)

_CPP_SOURCE_EXTS = (
    ".cc",
    ".cpp",
)

_SOURCE_EXTS = _C_SOURCE_EXTS + _CPP_SOURCE_EXTS

# These header suffixes are used to logically group C/C++ source (e.g.
# `foo/Bar.cpp`) with headers with the following suffixes (e.g. `foo/Bar.h` and
# `foo/Bar-inl.tcc`), such that the source provides all implementation for
# methods/classes declared in the headers.
#
# This is important for a couple reasons:
# 1) Automatic dependencies: Tooling can use this property to automatically
#    manage TARGETS dependencies by extracting `#include` references in sources
#    and looking up the rules which "provide" them.
# 2) Modules: This logical group can be combined into a standalone C/C++ module
#    (when such support is available).
_HEADER_SUFFIXES = (
    ".h",
    ".hpp",
    ".tcc",
    "-inl.h",
    "-inl.hpp",
    "-inl.tcc",
    "-defs.h",
    "-defs.hpp",
    "-defs.tcc",
)

CPP_UNITTEST_DEPS = [
    "shim//third-party/googletest:cpp_unittest_main",
]
CPP_FOLLY_UNITTEST_DEPS = [
    "gh_facebook_folly//folly/test/common:test_main_lib",
    "gh_facebook_folly//folly/ext/buck2:test_ext",
]

def _get_headers_from_sources(srcs):
    """
    Return the headers likely associated with the given sources

    Args:
        srcs: A list of strings representing files or build targets

    Returns:
        A list of header files corresponding to the list of sources. These files are
        validated to exist based on glob()
    """
    split_srcs = [
        paths.split_extension(src_filename)
        for src_filename in [_get_src_filename(src) for src in srcs]
        if "//" not in src_filename and not src_filename.startswith(":")
    ]

    # For e.g. foo.cpp grab a glob on foo.h, foo-inl.h, etc
    headers = [
        base + header_ext
        for base, ext in split_srcs
        if ext in _SOURCE_EXTS
        for header_ext in _HEADER_SUFFIXES
    ]

    # Avoid a warning for an empty glob pattern if there are no headers.
    return glob(headers) if headers else []

def _get_src_filename(src):
    """
    Return filename from a potentilly tuple value entry in srcs attribute
    """

    if is_tuple(src):
        s, _ = src
        return s
    return src

def _update_headers_with_src_headers(src_headers, out_headers):
    """
    Helper function to update raw headers with headers from srcs
    """
    src_headers = list(src_headers.difference(out_headers))

    # Looks simple, right? But if a header is explicitly added in, say, a
    # dictionary mapping, we want to make sure to keep the original mapping
    # and drop the F -> F mapping
    if is_list(out_headers):
        out_headers.extend(sorted(src_headers))
    else:
        # Let it throw AttributeError if update() can't be found neither
        out_headers.update({k: k for k in src_headers})
    return out_headers

def prebuilt_cpp_library(
        name,
        headers = None,
        linker_flags = None,
        private_linker_flags = None,
        **kwargs):
    prelude.prebuilt_cxx_library(
        name = name,
        exported_headers = headers,
        exported_linker_flags = linker_flags,
        linker_flags = private_linker_flags,
        **kwargs
    )

def cpp_library(
        name,
        deps = [],
        srcs = [],
        external_deps = [],
        exported_deps = [],
        exported_external_deps = [],
        undefined_symbols = None,
        visibility = ["PUBLIC"],
        auto_headers = None,
        modular_headers = None,
        arch_compiler_flags = None,
        labels = None,
        linker_flags = None,
        private_linker_flags = None,
        exported_linker_flags = None,
        headers = None,
        private_headers = None,
        propagated_pp_flags = (),
        feature = None,
        preferred_linkage = None,
        **kwargs):
    base_path = native.package_name()
    oss_depends_on_folly = read_bool("oss_depends_on", "folly", False)
    header_base_path = base_path
    if oss_depends_on_folly and header_base_path.startswith("folly"):
        header_base_path = header_base_path.replace("folly/", "", 1)

    _unused = (undefined_symbols, modular_headers, arch_compiler_flags, labels, propagated_pp_flags, feature, preferred_linkage)  # @unused
    if headers == None:
        headers = []
    if labels != None and "oss_dependency" in labels:
        if oss_depends_on_folly:
            headers = [item.replace("//:", "//folly:") if item == "//:folly-config.h" else item for item in headers]
    if is_select(srcs) and auto_headers == AutoHeaders.SOURCES:
        # Validate `srcs` and `auto_headers` before the config check
        fail(
            "//{}:{}: `select` srcs cannot support AutoHeaders.SOURCES".format(base_path, name),
        )
    auto_headers = get_auto_headers(auto_headers)
    if auto_headers == AutoHeaders.SOURCES and not is_select(srcs):
        src_headers = set(_get_headers_from_sources(srcs))
        if private_headers:
            src_headers = src_headers.difference(set(private_headers))

        headers = selects.apply(
            headers,
            partial(_update_headers_with_src_headers, src_headers),
        )
    if not is_select(linker_flags):
        linker_flags = linker_flags or []
        linker_flags = list(linker_flags)
        if exported_linker_flags != None:
            linker_flags += exported_linker_flags
    prelude.cxx_library(
        name = name,
        srcs = srcs,
        deps = _fix_deps(deps + external_deps_to_targets(external_deps)),
        exported_deps = _fix_deps(exported_deps + external_deps_to_targets(exported_external_deps)),
        visibility = visibility,
        preferred_linkage = "static",
        exported_headers = headers,
        headers = private_headers,
        exported_linker_flags = linker_flags,
        linker_flags = private_linker_flags,
        header_namespace = header_base_path,
        **kwargs
    )

def cpp_unittest(
        name,
        deps = [],
        external_deps = [],
        visibility = ["PUBLIC"],
        supports_static_listing = None,
        allocator = None,
        owner = None,
        labels = None,
        emails = None,
        extract_helper_lib = None,
        compiler_specific_flags = None,
        default_strip_mode = None,
        resources = {},
        test_main = None,
        versions = None,
        **kwargs):
    _unused = (supports_static_listing, allocator, owner, labels, emails, extract_helper_lib, compiler_specific_flags, default_strip_mode, versions)  # @unused
    if test_main != None:
        deps = deps + [test_main]
    elif read_bool("oss", "folly_cxx_tests", True):
        deps = deps + CPP_FOLLY_UNITTEST_DEPS
    else:
        deps = deps + CPP_UNITTEST_DEPS

    prelude.cxx_test(
        name = name,
        deps = _fix_deps(deps + external_deps_to_targets(external_deps)),
        visibility = visibility,
        resources = _fix_resources(resources),
        **kwargs
    )

def cpp_binary(
        name,
        deps = [],
        external_deps = [],
        visibility = ["PUBLIC"],
        dlopen_enabled = None,
        compiler_specific_flags = None,
        os_linker_flags = None,
        allocator = None,
        modules = None,
        **kwargs):
    _unused = (dlopen_enabled, compiler_specific_flags, os_linker_flags, allocator, modules)  # @unused
    prelude.cxx_binary(
        name = name,
        deps = _fix_deps(deps + external_deps_to_targets(external_deps)),
        visibility = visibility,
        **kwargs
    )

def java_binary(
        name,
        jar_style = None,
        runtime = None,
        *args,
        **kwargs):
    _unused = (jar_style, runtime)  # @unused
    return prelude.java_binary(
        name = name,
        *args,
        **kwargs
    )

def rust_library(
        name,
        edition = None,
        rustc_flags = [],
        deps = [],
        named_deps = None,
        test_deps = None,
        test_env = None,
        autocargo = None,
        unittests = None,
        mapped_srcs = {},
        cpp_deps = None,
        cxx_bridge = None,
        visibility = ["PUBLIC"],
        **kwargs):
    _unused = (test_deps, test_env, named_deps, autocargo, unittests, visibility, cpp_deps, cxx_bridge)  # @unused
    deps = _fix_deps(deps)
    mapped_srcs = _maybe_select_map(mapped_srcs, _fix_mapped_srcs)

    # Reset visibility because internal and external paths are different.
    visibility = ["PUBLIC"]

    prelude.rust_library(
        name = name,
        edition = edition or _default_rust_edition(),
        rustc_flags = rustc_flags + [_CFG_BUCK_BUILD],
        deps = deps,
        visibility = visibility,
        mapped_srcs = mapped_srcs,
        **kwargs
    )

def rust_binary(
        name,
        edition = None,
        rustc_flags = [],
        deps = [],
        autocargo = None,
        unittests = None,
        allocator = None,
        default_strip_mode = None,
        visibility = ["PUBLIC"],
        **kwargs):
    _unused = (unittests, allocator, default_strip_mode, autocargo)  # @unused
    deps = _fix_deps(deps)

    # @lint-ignore BUCKLINT: avoid "Direct usage of native rules is not allowed."
    prelude.rust_binary(
        name = name,
        edition = edition or _default_rust_edition(),
        rustc_flags = rustc_flags + [_CFG_BUCK_BUILD],
        deps = deps,
        visibility = visibility,
        **kwargs
    )

def rust_unittest(
        name,
        edition = None,
        rustc_flags = [],
        deps = [],
        visibility = ["PUBLIC"],
        **kwargs):
    deps = _fix_deps(deps)

    prelude.rust_test(
        name = name,
        edition = edition or _default_rust_edition(),
        rustc_flags = rustc_flags + [_CFG_BUCK_BUILD],
        deps = deps,
        visibility = visibility,
        **kwargs
    )

def rust_protobuf_library(
        name,
        srcs,
        build_script,
        protos = None,  # Pass a list of files. They'll be placed in the cwd. Prefer using proto_srcs.
        deps = None,
        test_deps = None,
        doctests = True,
        build_env = None,
        proto_srcs = None,
        crate_name = None):  # Use a proto_srcs() target, path is exposed as BUCK_PROTO_SRCS.
    _rust_protobuf_library(
        name,
        srcs,
        build_script,
        "buck2_protoc_dev",
        "prost",
        "prost-types",
        "tonic",
        protos,
        deps,
        test_deps,
        doctests,
        build_env,
        proto_srcs,
        crate_name,
    )

def rust_protobuf_library_prost_0134(
        name,
        srcs,
        build_script,
        protos = None,  # Pass a list of files. They'll be placed in the cwd. Prefer using proto_srcs.
        deps = None,
        test_deps = None,
        doctests = True,
        build_env = None,
        proto_srcs = None,
        crate_name = None):
    # Use a proto_srcs() target, path is exposed as BUCK_PROTO_SRCS.
    _rust_protobuf_library(
        name,
        srcs,
        build_script,
        "buck2_protoc_dev-tonic-0-12-3",
        "prost-0-13-4",
        "prost-types-0-13-4",
        "tonic-0-12-3",
        protos,
        deps,
        test_deps,
        doctests,
        build_env,
        proto_srcs,
        crate_name,
    )

def _rust_protobuf_library(
        name,
        srcs,
        build_script,
        buck2_protoc_dev,
        versioned_prost_target,
        versioned_prost_types_target,
        versioned_tonic_target,
        protos,  # Pass a list of files. They'll be placed in the cwd. Prefer using proto_srcs.
        deps,
        test_deps,
        doctests,
        build_env,
        proto_srcs,
        crate_name):  # Use a proto_srcs() target, path is exposed as BUCK_PROTO_SRCS.
    build_name = name + "-build"
    proto_name = name + "-proto"

    deps = (deps or []) + [
        "fbsource//third-party/rust:" + versioned_prost_target,
        "fbsource//third-party/rust:" + versioned_prost_types_target,
        "fbsource//third-party/rust:" + versioned_tonic_target,
    ]

    rust_binary(
        name = build_name,
        srcs = [build_script],
        crate_root = build_script,
        deps = [
            "fbsource//third-party/rust:" + versioned_tonic_target,
            "//buck2/app/buck2_protoc_dev:" + buck2_protoc_dev,
        ],
    )

    build_env = build_env or {}
    build_env.update(
        {
            "PROTOC": "$(exe shim//third-party/proto:protoc)",
            "PROTOC_INCLUDE": "$(location shim//third-party/proto:google_protobuf)",
        },
    )
    if proto_srcs:
        build_env["BUCK_PROTO_SRCS"] = "$(location {})".format(proto_srcs)

    prelude.genrule(
        name = proto_name,
        srcs = (protos or []) + [
            "shim//third-party/proto:google_protobuf",
        ],
        out = ".",
        cmd = "$(exe :" + build_name + ")",
        env = build_env,
    )

    rust_library(
        name = name,
        srcs = srcs,
        doctests = doctests,
        env = {
            # This is where prost looks for generated .rs files
            "OUT_DIR": "$(location :{})".format(proto_name),
        },
        test_deps = test_deps,
        deps = deps,
        crate = crate_name or name,
    )

ProtoSrcsInfo = provider(fields = ["srcs"])

def _proto_srcs_impl(ctx):
    srcs = {src.basename: src for src in ctx.attrs.srcs}
    for dep in ctx.attrs.deps:
        for src in dep[ProtoSrcsInfo].srcs:
            if src.basename in srcs:
                fail("Duplicate src:", src.basename)
            srcs[src.basename] = src
    out = ctx.actions.copied_dir(ctx.attrs.name, srcs)
    return [DefaultInfo(default_output = out), ProtoSrcsInfo(srcs = srcs.values())]

proto_srcs = rule(
    impl = _proto_srcs_impl,
    attrs = {
        "deps": attrs.list(attrs.dep(), default = []),
        "srcs": attrs.list(attrs.source(), default = []),
    },
)

def ocaml_binary(
        name,
        deps = [],
        visibility = ["PUBLIC"],
        **kwargs):
    deps = _fix_deps(deps)

    prelude.ocaml_binary(
        name = name,
        deps = deps,
        visibility = visibility,
        **kwargs
    )

_CFG_BUCK_BUILD = "--cfg=buck_build"

def _maybe_select_map(v, mapper):
    if is_select(v):
        return select_map(v, mapper)
    return mapper(v)

def _fix_mapped_srcs(xs: dict[str, str]):
    # For reasons, this is source -> file path, which is the opposite of what
    # it should be.
    return {translate_target(k): v for (k, v) in xs.items()}

def _fix_deps(xs):
    if is_select(xs):
        return select_map(xs, lambda child_targets: _fix_deps(child_targets))
    return map(translate_target, xs)

def _fix_resources(resources):
    if is_list(resources):
        return [translate_target(r) for r in resources]

    if is_dict(resources):
        return {k: translate_target(v) for k, v in resources.items()}

    fail("Unexpected type {} for resources".format(type(resources)))

def _default_rust_edition():
    package = native.package_name()

    # Parse buckconfig entries in the following form:
    #
    #     [rust]
    #     default_edition = 2024
    #     default_edition:buck2 = 2021
    #     default_edition:buck2/dice = 2024
    #
    if package:
        split = package.split("/")
        for i in range(len(split)):
            parent_directory = "/".join(split[:len(split) - i])
            edition = read_config("rust", "default_edition:" + parent_directory)
            if edition != None:
                return edition

    return read_config("rust", "default_edition")

def thrift_library(
        name,
        thrift_srcs,
        languages,
        deps = [],
        py_base_module = None,
        rust_deps = [],
        thrift_rust_options = [],
        **kwargs):
    for l in languages:
        if False:
            pass
        else:
            print("FIXME(buck2-shims-meta): unsupported thrift language: {}".format(l))

# Do a nasty conversion of e.g. ("supercaml", None, "ocaml-dev") to
# 'fbcode//third-party-buck/platform010/build/supercaml:ocaml-dev'
# (which will then get mapped to `shim//third-party/ocaml:ocaml-dev`).
def external_dep_to_target(t):
    if type(t) == type(()):
        return "fbcode//third-party-buck/platform010/build/{}:{}".format(t[0], t[2])
    else:
        return "fbcode//third-party-buck/platform010/build/{}:{}".format(t, t)

def external_deps_to_targets(ts):
    return [external_dep_to_target(t) for t in ts]
