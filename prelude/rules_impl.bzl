# Android
load("@fbcode//buck2/prelude/android:android.bzl", _android_extra_attributes = "extra_attributes", _android_implemented_rules = "implemented_rules")

# Apple
load("@fbcode//buck2/prelude/apple:apple_rules_impl.bzl", _apple_extra_attributes = "extra_attributes", _apple_implemented_rules = "implemented_rules")
load("@fbcode//buck2/prelude/apple:apple_toolchain_setup.bzl", _get_apple_cxx_select_map = "get_apple_cxx_select_map")

# Configuration
load("@fbcode//buck2/prelude/configurations:rules.bzl", _config_implemented_rules = "implemented_rules")
load("@fbcode//buck2/prelude/cxx:cxx.bzl", "cxx_binary_impl", "cxx_library_impl", "cxx_precompiled_header_impl", "cxx_test_impl", "prebuilt_cxx_library_impl")
load("@fbcode//buck2/prelude/cxx:cxx_toolchain.bzl", "cxx_toolchain_impl")
load("@fbcode//buck2/prelude/cxx:cxx_toolchain_types.bzl", "CxxPlatformInfo", "CxxToolchainInfo", "DistLtoToolsInfo")

# C++
load("@fbcode//buck2/prelude/cxx:headers.bzl", "CPrecompiledHeaderInfo")
load("@fbcode//buck2/prelude/cxx:prebuilt_cxx_library_group.bzl", "prebuilt_cxx_library_group_impl")

# Go
load("@fbcode//buck2/prelude/go:go_binary.bzl", "go_binary_impl")
load("@fbcode//buck2/prelude/go:go_library.bzl", "go_library_impl")
load("@fbcode//buck2/prelude/go:go_test.bzl", "go_test_impl")
load("@fbcode//buck2/prelude/go:toolchain.bzl", "GoToolchainInfo")

# Haskell
load("@fbcode//buck2/prelude/haskell:haskell.bzl", "HaskellPlatformInfo", "HaskellToolchainInfo", "haskell_binary_impl", "haskell_library_impl", "haskell_prebuilt_library_impl")

# Http archive
load("@fbcode//buck2/prelude/http_archive:http_archive.bzl", "http_archive_impl")

# Java
load("@fbcode//buck2/prelude/java:java.bzl", _java_extra_attributes = "extra_attributes", _java_implemented_rules = "implemented_rules")

# JavaScript
load("@fbcode//buck2/prelude/js:js.bzl", _js_extra_attributes = "extra_attributes", _js_implemented_rules = "implemented_rules")

# Kotlin
load("@fbcode//buck2/prelude/kotlin:kotlin.bzl", _kotlin_extra_attributes = "extra_attributes", _kotlin_implemented_rules = "implemented_rules")

# OCaml
load("@fbcode//buck2/prelude/ocaml:ocaml.bzl", "ocaml_binary_impl", "ocaml_library_impl", "ocaml_object_impl", "prebuilt_ocaml_library_impl")
load("@fbcode//buck2/prelude/ocaml:providers.bzl", "OCamlPlatformInfo", "OCamlToolchainInfo")

# Python
load("@fbcode//buck2/prelude/python:cxx_python_extension.bzl", "cxx_python_extension_impl")
load("@fbcode//buck2/prelude/python:prebuilt_python_library.bzl", "prebuilt_python_library_impl")
load("@fbcode//buck2/prelude/python:python_binary.bzl", "python_binary_impl")
load("@fbcode//buck2/prelude/python:python_library.bzl", "python_library_impl")
load("@fbcode//buck2/prelude/python:python_test.bzl", "python_test_impl")
load("@fbcode//buck2/prelude/python:toolchain.bzl", "PythonPlatformInfo", "PythonToolchainInfo")

# Python Bootstrap
load("@fbcode//buck2/prelude/python_bootstrap:python_bootstrap.bzl", "PythonBootstrapSources", "PythonBootstrapToolchainInfo", "python_bootstrap_binary_impl", "python_bootstrap_library_impl")

# Rust
load("@fbcode//buck2/prelude/rust:rust_binary.bzl", "rust_binary_impl", "rust_test_impl")
load("@fbcode//buck2/prelude/rust:rust_library.bzl", "prebuilt_rust_library_impl", "rust_library_impl")
load("@fbcode//buck2/prelude/rust:rust_toolchain.bzl", "RustPlatformInfo", "RustToolchainInfo")

#Zip file
load("@fbcode//buck2/prelude/zip_file:zip_file.bzl", _zip_file_extra_attributes = "extra_attributes", _zip_file_implemented_rules = "implemented_rules")

# General
load(":alias.bzl", "alias_impl", "configured_alias_impl", "versioned_alias_impl")
load(":attributes.bzl", "IncludeType", "Linkage", "attributes")
load(":command_alias.bzl", "command_alias_impl")
load(":export_file.bzl", "export_file_impl")
load(":filegroup.bzl", "filegroup_impl")
load(":genrule.bzl", "genrule_impl")
load(":http_file.bzl", "http_file_impl")
load(":remote_file.bzl", "remote_file_impl")
load(":sh_binary.bzl", "sh_binary_impl")
load(":sh_test.bzl", "sh_test_impl")
load(":test_suite.bzl", "test_suite_impl")
load(":worker_tool.bzl", "worker_tool")

def _merge_dictionaries(dicts):
    result = {}
    for d in dicts:
        for key, value in d.items():
            if key in result:
                fail("Duplicate key: '{}' while merging dictionaries".format(key))
            result[key] = value

    return result

implemented_rules = struct(
    #common rules
    alias = alias_impl,
    command_alias = command_alias_impl,
    configured_alias = configured_alias_impl,
    export_file = export_file_impl,
    filegroup = filegroup_impl,
    genrule = genrule_impl,
    http_archive = http_archive_impl,
    http_file = http_file_impl,
    remote_file = remote_file_impl,
    sh_binary = sh_binary_impl,
    sh_test = sh_test_impl,
    test_suite = test_suite_impl,
    versioned_alias = versioned_alias_impl,
    worker_tool = worker_tool,

    #c++
    cxx_binary = cxx_binary_impl,
    cxx_test = cxx_test_impl,
    cxx_toolchain = cxx_toolchain_impl,
    cxx_genrule = genrule_impl,
    cxx_library = cxx_library_impl,
    cxx_precompiled_header = cxx_precompiled_header_impl,
    cxx_python_extension = cxx_python_extension_impl,
    prebuilt_cxx_library = prebuilt_cxx_library_impl,
    prebuilt_cxx_library_group = prebuilt_cxx_library_group_impl,

    #go
    go_binary = go_binary_impl,
    go_library = go_library_impl,
    go_test = go_test_impl,

    #haskell
    haskell_library = haskell_library_impl,
    haskell_binary = haskell_binary_impl,
    haskell_prebuilt_library = haskell_prebuilt_library_impl,

    #ocaml
    ocaml_binary = ocaml_binary_impl,
    ocaml_object = ocaml_object_impl,
    ocaml_library = ocaml_library_impl,
    prebuilt_ocaml_library = prebuilt_ocaml_library_impl,

    #python
    prebuilt_python_library = prebuilt_python_library_impl,
    python_binary = python_binary_impl,
    python_library = python_library_impl,
    python_test = python_test_impl,

    #python bootstrap
    python_bootstrap_binary = python_bootstrap_binary_impl,
    python_bootstrap_library = python_bootstrap_library_impl,

    #rust
    rust_binary = rust_binary_impl,
    rust_library = rust_library_impl,
    prebuilt_rust_library = prebuilt_rust_library_impl,
    rust_test = rust_test_impl,

    #merged **kwargs
    **_merge_dictionaries([
        _android_implemented_rules,
        _apple_implemented_rules,
        _config_implemented_rules,
        _java_implemented_rules,
        _js_implemented_rules,
        _kotlin_implemented_rules,
        _zip_file_implemented_rules,
    ])
)

# The default platform for fbcode rules that didn't specify it properly
_default_fbcode_platform = read_config("fbcode", "platform", "platform009")

def _get_fbcode_select_map(prefix):
    """
    Return a map to use to select fbcode C++ based toolchain definitions.
    """
    prefix = "fbcode//buck2/platform:{}-".format(prefix)
    return {
        # TODO: Ideally we don't need the `DEFAULT` clause, but `.buckconfig`s
        # set the coarser fbcode platform to work with v1, which leaks into the
        # few rules which don't set a more granular default.
        "DEFAULT": prefix + _default_fbcode_platform + "-clang",
        "ovr_config//toolchain/fb:platform009-clang": prefix + "platform009-clang",
        "ovr_config//toolchain/fb:platform009-clang-nosan": prefix + "platform009-clang-nosan",
        "ovr_config//toolchain/fb:platform009-clang-nosan-split-dwarf": prefix + "platform009-clang-nosan-split-dwarf",
        "ovr_config//toolchain/fb:platform009-clang-split-dwarf": prefix + "platform009-clang-split-dwarf",
        "ovr_config//toolchain/fb:platform009-clang12": prefix + "platform009-clang-12",
        "ovr_config//toolchain/fb:platform009-clang12-nosan": prefix + "platform009-clang-12-nosan",
        "ovr_config//toolchain/fb:platform009-clang12-split-dwarf": prefix + "platform009-clang-12-split-dwarf",
        "ovr_config//toolchain/fb:platform009-gcc": prefix + "platform009-gcc",
        "ovr_config//toolchain/fb:platform010-clang": prefix + "platform010-clang",
        "ovr_config//toolchain/fb:platform010-clang-nosan": prefix + "platform010-clang-nosan",
        "ovr_config//toolchain/fb:platform010-clang-nosan-split-dwarf": prefix + "platform010-clang-nosan-split-dwarf",
        "ovr_config//toolchain/fb:platform010-clang-split-dwarf": prefix + "platform010-clang-split-dwarf",
        "ovr_config//toolchain/fb:platform010-compat-clang": prefix + "platform010-compat-clang",
        "ovr_config//toolchain/fb:platform010-compat-clang-nosan": prefix + "platform010-compat-clang-nosan",
        "ovr_config//toolchain/fb:platform010-compat-clang-nosan-split-dwarf": prefix + "platform010-compat-clang-nosan-split-dwarf",
        "ovr_config//toolchain/fb:platform010-compat-clang-split-dwarf": prefix + "platform010-compat-clang-split-dwarf",
        "ovr_config//toolchain/fb:platform010-compat-gcc": prefix + "platform010-compat-gcc",
        "ovr_config//toolchain/fb:platform010-gcc": prefix + "platform010-gcc",
    }

def _get_android_cxx_select_map():
    return {
        "ovr_config//os:android": select({
            "DEFAULT": "fbsource//xplat/toolchains/android/ndk:cxx-toolchain-x86",
            "ovr_config//cpu/constraints:arm32": "fbsource//xplat/toolchains/android/ndk:cxx-toolchain-armv7",
            "ovr_config//cpu/constraints:arm64": "fbsource//xplat/toolchains/android/ndk:cxx-toolchain-arm64",
            "ovr_config//cpu/constraints:x86_32": "fbsource//xplat/toolchains/android/ndk:cxx-toolchain-x86",
            "ovr_config//cpu/constraints:x86_64": "fbsource//xplat/toolchains/android/ndk:cxx-toolchain-x86_64",
        }),
    }

def get_apple_rust_select_map():
    return {
        "ovr_config//os:macos": "fbcode//buck2/platform:rust-macosx-x86_64_minimal_xcode",
    }

def _get_infer_select_map():
    return {"ovr_config//toolchain/fb:platform009-infer": "fbcode//buck2/platform:buck2-infer"}

def _select_cxx_toolchain():
    return select(
        _merge_dictionaries([
            _get_android_cxx_select_map(),
            _get_apple_cxx_select_map(),
            _get_fbcode_select_map("buck2"),
            _get_infer_select_map(),
        ]),
    )

def _select_go_toolchain():
    return "fbcode//buck2/platform:go-{}-clang".format(_default_fbcode_platform)

def _select_python_toolchain():
    return select({
        # TODO: Ideally we don't need this, `.buckconfig`s set the coarser fbcode
        # platform to work with v1, which leaks into the few rules which don't
        # set a more granular default.
        "DEFAULT": "fbcode//buck2/platform:py3.8-{}".format(_default_fbcode_platform),
        "ovr_config//os:macos": "fbcode//buck2/platform:py3.8-macosx-x86_64",
        "ovr_config//runtime/constraints:platform009": select({
            "ovr_config//third-party/python/constraints:3.8": "fbcode//buck2/platform:py3.8-platform009",
            "ovr_config//third-party/python/constraints:cinder.3.8": "fbcode//buck2/platform:cinder_py3.8-platform009",
        }),
        "ovr_config//runtime/constraints:platform010": select({
            "ovr_config//third-party/python/constraints:3.8": "fbcode//buck2/platform:py3.8-platform010",
            "ovr_config//third-party/python/constraints:cinder.3.8": "fbcode//buck2/platform:cinder_py3.8-platform010",
        }),
        "ovr_config//runtime/constraints:platform010-compat": select({
            "ovr_config//third-party/python/constraints:3.8": "fbcode//buck2/platform:py3.8-platform010-compat",
            "ovr_config//third-party/python/constraints:cinder.3.8": "fbcode//buck2/platform:cinder_py3.8-platform010-compat",
        }),
    })

def _select_python_bootstrap_toolchain():
    return select({
        "DEFAULT": "fbcode//buck2/platform:bootstrap-py3.8-{}".format(_default_fbcode_platform),
        "ovr_config//runtime/constraints:platform009": "fbcode//buck2/platform:bootstrap-py3.8-platform009",
        "ovr_config//runtime/constraints:platform010": "fbcode//buck2/platform:bootstrap-py3.8-platform010",
    })

def _select_rust_toolchain():
    return select(_merge_dictionaries([
        get_apple_rust_select_map(),
        _get_fbcode_select_map("rust"),
    ]))

def _select_ocaml_toolchain():
    return select(_get_fbcode_select_map("ocaml"))

def _select_haskell_toolchain():
    return select(_get_fbcode_select_map("haskell"))

def _cxx_python_extension_attrs():
    # cxx_python_extension is a subset of cxx_library, plus a base_module.
    # So we can reuse cxx_library, we augment it with the additional attributes it defines.
    # This isn't the ideal way to reuse it (we'd rather cxx_library was split it multiple reusable parts),
    # but it's the pragmatic way of getting it working for now.
    library = attributes["cxx_library"]
    me = attributes["cxx_python_extension"]
    res = {k: attr.default_only(library[k]) for k in library if k not in me}
    res.update({
        # Copied from cxx_library.
        "precompiled_header": attr.option(attr.dep(providers = [CPrecompiledHeaderInfo]), default = None),
        "preferred_linkage": attr.default_only(attr.string(default = "shared")),  # Force shared linkage always
        "use_link_groups": attr.bool(default = False),
        "_cxx_toolchain": attr.exec_dep(default = _select_cxx_toolchain()),
        "_hacks": attr.dep(default = "fbcode//buck2/platform:cxx-hacks"),
        # Copied from python_library.
        "_python_toolchain": attr.exec_dep(default = _select_python_toolchain(), providers = [PythonToolchainInfo, PythonPlatformInfo]),
    })
    return res

def _cxx_binary_and_test_attrs():
    return {
        "enable_distributed_thinlto": attr.bool(default = False),
        "link_whole": attr.default_only(attr.bool(default = False)),
        "precompiled_header": attr.option(attr.dep(providers = [CPrecompiledHeaderInfo]), default = None),
        "resources": attr.named_set(attr.one_of(attr.dep(), attr.source(allow_directory = True)), sorted = True, default = []),
        "use_link_groups": attr.bool(default = False),
        "_cxx_toolchain": attr.exec_dep(default = _select_cxx_toolchain()),
        "_hacks": attr.dep(default = "fbcode//buck2/platform:cxx-hacks"),
    }

extra_attributes = struct(
    export_file = {
        "src": attr.option(attr.source(allow_directory = True), default = None),
    },
    genrule = {
        "srcs": attr.named_set(attr.source(allow_directory = True), sorted = False, default = []),
    },
    # The 'actual' attribute of configured_alias is a configured_label, which is
    # currently unimplemented. Map it to dep so we can simply forward the providers.
    configured_alias = {
        # We use a separate field instead of re-purposing `actual`, as we want
        # to keep output format compatibility with v1.
        "configured_actual": attr.configured_dep(),
    },
    sh_test = {
        "list_args": attr.option(attr.list(attr.string()), default = None),
        "list_env": attr.option(attr.dict(key = attr.string(), value = attr.string(), sorted = False), default = None),
        "run_args": attr.list(attr.string(), default = []),
        "run_env": attr.dict(key = attr.string(), value = attr.string(), sorted = False, default = {}),
        "test": attr.option(attr.one_of(attr.dep(), attr.source()), default = None),
    },
    test_suite = {
        # On buck1 query, tests attribute on test_suite is treated as deps, while on buck2 it is not.
        # While buck2's behavior makes more sense, we want to preserve buck1 behavior on test_suite for now to make TD behavior match between buck1 and buck2.
        # This diff makes the behaviors match by adding a test_deps attribute to test_suite on buck2 that is used as a deps attribute. In the macro layer, we set test_deps = tests if we are using buck2.
        # For more context: https://fb.prod.workplace.com/groups/603286664133355/posts/682567096205311/?comment_id=682623719532982&reply_comment_id=682650609530293
        "test_deps": attr.list(attr.dep(), default = []),
    },
    worker_tool = {
        # overridden to handle buck1's use of @Value.Default
        "args": attr.one_of(attr.arg(), attr.list(attr.arg()), default = []),
        "_worker_tool_runner": attr.dep(default = "fbsource//xplat/buck2/tools/worker:worker_tool_runner"),
    },

    #c++
    cxx_genrule = {
        "_cxx_toolchain": attr.exec_dep(default = _select_cxx_toolchain()),
    },
    cxx_library = {
        "precompiled_header": attr.option(attr.dep(providers = [CPrecompiledHeaderInfo]), default = None),
        "prefer_stripped_objects": attr.bool(default = False),
        "preferred_linkage": attr.enum(Linkage, default = "any"),
        "resources": attr.named_set(attr.one_of(attr.dep(), attr.source(allow_directory = True)), sorted = True, default = []),
        "use_link_groups": attr.bool(default = False),
        "_cxx_toolchain": attr.exec_dep(default = _select_cxx_toolchain()),
        "_hacks": attr.dep(default = "fbcode//buck2/platform:cxx-hacks"),
    },
    cxx_binary = _cxx_binary_and_test_attrs(),
    cxx_test = _cxx_binary_and_test_attrs(),
    cxx_toolchain = {
        "archiver": attr.dep(providers = [RunInfo]),
        "asm_compiler": attr.option(attr.dep(providers = [RunInfo]), default = None),
        "asm_preprocessor": attr.option(attr.dep(providers = [RunInfo]), default = None),
        "assembler": attr.dep(providers = [RunInfo]),
        "assembler_preprocessor": attr.option(attr.dep(providers = [RunInfo]), default = None),
        "c_compiler": attr.dep(providers = [RunInfo]),
        "cxx_compiler": attr.dep(providers = [RunInfo]),
        "linker": attr.dep(providers = [RunInfo]),
        "nm": attr.dep(providers = [RunInfo]),
        "objcopy_for_shared_library_interface": attr.dep(providers = [RunInfo]),
        # Used for resolving any 'platform_*' attributes.
        "platform_name": attr.option(attr.string(), default = None),
        "ranlib": attr.option(attr.dep(providers = [RunInfo]), default = None),
        "requires_objects": attr.bool(default = False),
        "strip": attr.dep(providers = [RunInfo]),
        "supports_distributed_thinlto": attr.bool(default = False),
        "use_archiver_flags": attr.bool(default = True),
        "_dist_lto_tools": attr.dep(providers = [DistLtoToolsInfo], default = "fbcode//buck2/prelude/cxx/dist_lto/tools:dist_lto_tools"),
        "_mk_comp_db": attr.dep(providers = [RunInfo], default = "fbcode//buck2/prelude/cxx/tools:make_comp_db"),
        "_mk_hmap": attr.dep(providers = [RunInfo], default = "fbsource//xplat/buck2/tools/cxx:hmap_wrapper"),
    },
    cxx_python_extension = _cxx_python_extension_attrs(),
    prebuilt_cxx_library = {
        "exported_header_style": attr.enum(IncludeType, default = "system"),
        "header_dirs": attr.option(attr.list(attr.source(allow_directory = True)), default = None),
        "platform_header_dirs": attr.option(attr.list(attr.tuple(attr.regex(), attr.list(attr.source(allow_directory = True)))), default = None),
        "preferred_linkage": attr.enum(Linkage, default = "any"),
        "public_include_directories": attr.set(attr.string(), sorted = True, default = []),
        "public_system_include_directories": attr.set(attr.string(), sorted = True, default = []),
        "raw_headers": attr.set(attr.source(), sorted = True, default = []),
        "versioned_header_dirs": attr.option(attr.versioned(attr.list(attr.source(allow_directory = True))), default = None),
        "_cxx_toolchain": attr.exec_dep(default = _select_cxx_toolchain()),
    },

    # go
    go_binary = {
        "resources": attr.list(attr.source(allow_directory = True), default = []),
        "_cxx_toolchain": attr.dep(default = _select_cxx_toolchain()),
        "_go_toolchain": attr.dep(default = _select_go_toolchain(), providers = [GoToolchainInfo]),
    },
    go_library = {
        "_go_toolchain": attr.dep(default = _select_go_toolchain(), providers = [GoToolchainInfo]),
    },
    go_test = {
        "resources": attr.list(attr.source(allow_directory = True), default = []),
        "_cxx_toolchain": attr.dep(default = _select_cxx_toolchain()),
        "_go_toolchain": attr.dep(default = _select_go_toolchain(), providers = [GoToolchainInfo]),
        "_testmaingen": attr.dep(default = "fbcode//buck2/prelude/go/tools:testmaingen"),
    },

    #ocaml
    ocaml_binary = {
        "_cxx_toolchain": attr.exec_dep(default = _select_cxx_toolchain()),
        "_ocaml_toolchain": attr.exec_dep(default = _select_ocaml_toolchain(), providers = [OCamlToolchainInfo, OCamlPlatformInfo]),
    },
    ocaml_object = {
        "bytecode_only": attr.option(attr.bool(), default = None),
        "compiler_flags": attr.list(attr.arg(), default = []),
        "contacts": attr.list(attr.string(), default = []),
        "default_host_platform": attr.option(attr.configuration_label(), default = None),
        "deps": attr.list(attr.dep(), default = []),
        "labels": attr.list(attr.string(), default = []),
        "licenses": attr.list(attr.source(), default = []),
        "linker_flags": attr.list(attr.string(), default = []),
        "ocamldep_flags": attr.list(attr.arg(), default = []),
        "platform": attr.option(attr.string(), default = None),
        "platform_deps": attr.list(attr.tuple(attr.regex(), attr.set(attr.dep(), sorted = True)), default = []),
        "platform_linker_flags": attr.list(attr.tuple(attr.regex(), attr.list(attr.string())), default = []),
        "srcs": attr.option(attr.named_set(attr.source(), sorted = False), default = None),
        "warnings_flags": attr.option(attr.string(), default = None),
        "within_view": attr.option(attr.list(attr.string())),
        "_cxx_toolchain": attr.exec_dep(default = _select_cxx_toolchain()),
        "_ocaml_toolchain": attr.exec_dep(default = _select_ocaml_toolchain(), providers = [OCamlToolchainInfo, OCamlPlatformInfo]),
    },
    ocaml_library = {
        "_cxx_toolchain": attr.exec_dep(default = _select_cxx_toolchain()),
        "_ocaml_toolchain": attr.exec_dep(default = _select_ocaml_toolchain(), providers = [OCamlToolchainInfo, OCamlPlatformInfo]),
    },
    prebuilt_ocaml_library = {

        # These fields in 'attributes.bzl' are wrong.
        #
        # There they are defined in terms of `attr.string()`. This
        # block overrides/corrects them here so as to be in terms of
        # `attr.source()`.
        "bytecode_c_libs": attr.list(attr.source(), default = []),
        "bytecode_lib": attr.option(attr.source()),
        "c_libs": attr.list(attr.source(), default = []),
        "include_dir": attr.option(attr.source(allow_directory = True)),
        "lib_dir": attr.option(attr.source(allow_directory = True)),
        "native_c_libs": attr.list(attr.source(), default = []),
        "native_lib": attr.option(attr.source()),
    },

    #python
    prebuilt_python_library = {
        "_create_manifest_for_source_dir": attr.dep(default = "fbcode//buck2/prelude/python/tools:create_manifest_for_source_dir"),
        "_extract": attr.dep(default = "fbcode//buck2/prelude/python/tools:extract"),
        "_python_toolchain": attr.exec_dep(default = _select_python_toolchain(), providers = [PythonToolchainInfo, PythonPlatformInfo]),
    },
    python_library = {
        "resources": attr.named_set(attr.one_of(attr.dep(), attr.source(allow_directory = True)), sorted = True, default = []),
        "_create_manifest_for_source_dir": attr.dep(default = "fbcode//buck2/prelude/python/tools:create_manifest_for_source_dir"),
        "_cxx_toolchain": attr.exec_dep(default = _select_cxx_toolchain(), providers = [CxxToolchainInfo, CxxPlatformInfo]),
        "_python_toolchain": attr.exec_dep(default = _select_python_toolchain(), providers = [PythonToolchainInfo, PythonPlatformInfo]),
    },
    python_binary = {
        "package_split_dwarf_dwp": attr.bool(default = False),
        "_create_manifest_for_source_dir": attr.dep(default = "fbcode//buck2/prelude/python/tools:create_manifest_for_source_dir"),
        "_cxx_toolchain": attr.exec_dep(default = _select_cxx_toolchain(), providers = [CxxToolchainInfo, CxxPlatformInfo]),
        "_hacks": attr.dep(default = "fbcode//buck2/platform:cxx-hacks"),
        "_python_toolchain": attr.exec_dep(default = _select_python_toolchain(), providers = [PythonToolchainInfo, PythonPlatformInfo]),
    },
    python_test = {
        "package_split_dwarf_dwp": attr.bool(default = False),
        "resources": attr.named_set(attr.one_of(attr.dep(), attr.source(allow_directory = True)), sorted = True, default = []),
        "_create_manifest_for_source_dir": attr.dep(default = "fbcode//buck2/prelude/python/tools:create_manifest_for_source_dir"),
        "_cxx_toolchain": attr.exec_dep(default = _select_cxx_toolchain(), providers = [CxxToolchainInfo, CxxPlatformInfo]),
        "_hacks": attr.dep(default = "fbcode//buck2/platform:cxx-hacks"),
        "_python_toolchain": attr.exec_dep(default = _select_python_toolchain(), providers = [PythonToolchainInfo, PythonPlatformInfo]),
        "_test_main": attr.source(default = "fbcode//buck2/prelude/python/tools:__test_main__.py"),
    },
    #python bootstrap
    python_bootstrap_binary = {
        "deps": attr.list(attr.dep(providers = [PythonBootstrapSources]), default = []),
        "main": attr.source(),
        "_python_bootstrap_toolchain": attr.exec_dep(default = _select_python_bootstrap_toolchain(), providers = [PythonBootstrapToolchainInfo]),
    },
    python_bootstrap_library = {
        "srcs": attr.list(attr.source()),
    },
    #rust
    rust_binary = {
        "_cxx_toolchain": attr.exec_dep(default = _select_cxx_toolchain()),
        "_rust_toolchain": attr.exec_dep(default = _select_rust_toolchain(), providers = [RustToolchainInfo, RustPlatformInfo]),
    },
    prebuilt_rust_library = {
        "_cxx_toolchain": attr.exec_dep(default = _select_cxx_toolchain()),
        "_rust_toolchain": attr.exec_dep(default = _select_rust_toolchain(), providers = [RustToolchainInfo, RustPlatformInfo]),
    },
    rust_library = {
        # linker_flags weren't supported for rust_library in Buck v1 but the
        # fbcode macros pass them anyway. They're typically empty since the
        # config-level flags don't get injected, but it doesn't hurt to accept
        # them and it simplifies the implementation of Rust rules since they
        # don't have to know whether we're building a rust_binary or a
        # rust_library.
        "linker_flags": attr.list(attr.arg(), default = []),
        "preferred_linkage": attr.enum(Linkage, default = "any"),
        "_cxx_toolchain": attr.exec_dep(default = _select_cxx_toolchain()),
        "_rust_toolchain": attr.exec_dep(default = _select_rust_toolchain(), providers = [RustToolchainInfo, RustPlatformInfo]),
    },
    rust_test = {
        "framework": attr.bool(default = True),
        "_cxx_toolchain": attr.exec_dep(default = _select_cxx_toolchain()),
        "_rust_toolchain": attr.exec_dep(default = _select_rust_toolchain(), providers = [RustToolchainInfo, RustPlatformInfo]),
    },
    haskell_binary = {
        "_cxx_toolchain": attr.exec_dep(default = _select_cxx_toolchain()),
        "_haskell_toolchain": attr.exec_dep(default = _select_haskell_toolchain(), providers = [HaskellToolchainInfo, HaskellPlatformInfo]),
    },
    haskell_library = {
        "preferred_linkage": attr.enum(Linkage, default = "any"),
        "_cxx_toolchain": attr.exec_dep(default = _select_cxx_toolchain()),
        "_haskell_toolchain": attr.exec_dep(default = _select_haskell_toolchain(), providers = [HaskellToolchainInfo, HaskellPlatformInfo]),
    },

    # scala
    scala_library = {
        "resources_root": attr.option(attr.string(), default = None),
    },
    scala_test = {
        "resources_root": attr.option(attr.string(), default = None),
    },

    # groovy
    groovy_library = {
        "resources_root": attr.option(attr.string(), default = None),
    },
    groovy_test = {
        "resources_root": attr.option(attr.string(), default = None),
    },

    # http things get only 1 hash in v1 but in v2 we allow multiple. Also,
    # don't default hashes to empty strings.
    http_archive = {
        "sha1": attr.option(attr.string()),
        "sha256": attr.option(attr.string()),
        "_create_exclusion_list": attr.exec_dep(default = "fbcode//buck2/prelude/http_archive/tools:create_exclusion_list"),
    },
    http_file = {
        "sha1": attr.option(attr.string()),
        "sha256": attr.option(attr.string()),
    },
    remote_file = {
        "sha1": attr.option(attr.string()),
        "sha256": attr.option(attr.string()),
        "_unzip_tool": attr.dep(providers = [RunInfo], default = "fbsource//xplat/buck2/tools/zip:unzip"),
    },
    prebuilt_native_library = {
        "native_libs": attr.source(allow_directory = True),
    },
    sh_binary = {
        "resources": attr.list(attr.source(allow_directory = True), default = []),
    },
    filegroup = {
        "srcs": attr.list(attr.source(allow_directory = True), default = []),
    },
    core_data_model = {
        "path": attr.source(allow_directory = True),
    },
    scene_kit_assets = {
        "path": attr.source(allow_directory = True),
    },

    #merged **kwargs
    **_merge_dictionaries([
        _android_extra_attributes,
        _apple_extra_attributes,
        _java_extra_attributes,
        _js_extra_attributes,
        _kotlin_extra_attributes,
        _zip_file_extra_attributes,
    ])
)
