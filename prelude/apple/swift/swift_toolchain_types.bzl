# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

#####################################################################
# Providers

SwiftObjectFormat = enum(
    "object",
    "bc",
    "ir",
    "irgen",
    "object-embed-bitcode",
)

SwiftToolchainInfo = provider(fields = [
    "architecture",
    "can_toolchain_emit_obj_c_header_textually",  # bool
    "uncompiled_swift_sdk_modules_deps",  # {str: dependency} Expose deps of uncompiled Swift SDK modules.
    "uncompiled_clang_sdk_modules_deps",  # {str: dependency} Expose deps of uncompiled Clang SDK modules.
    "compiler_flags",
    "compiler",
    "prefix_serialized_debugging_options",  # bool
    "object_format",  # "SwiftObjectFormat"
    "resource_dir",  # "artifact",
    "sdk_path",
    "swift_stdlib_tool_flags",
    "swift_stdlib_tool",
    "runtime_run_paths",  # [str]
    "supports_swift_cxx_interoperability_mode",  # bool
    "supports_cxx_interop_requirement_at_import",  # bool
])

# A provider that represents a non-yet-compiled SDK (Swift or Clang) module,
# and doesn't contain any artifacts because Swift toolchain isn't resolved yet.
SdkUncompiledModuleInfo = provider(fields = [
    "deps",  # [Dependency]
    "input_relative_path",  # A relative prefixed path to a textual swiftinterface/modulemap file within an SDK.
    "is_framework",  # This is mostly needed for the generated Swift module map file.
    "is_swiftmodule",  # If True then represents a swiftinterface, otherwise Clang's modulemap.
    "module_name",  # A real name of a module, without distinguishing suffixes.
    "partial_cmd",  # Partial arguments, required to compile a particular SDK module.
    "target",  # A string of the compiler target triple to use for clang module deps, eg arm64-apple-ios16.4
])

WrappedSdkCompiledModuleInfo = provider(fields = [
    "clang_deps",  # A SwiftCompiledModuleTset of SwiftCompiledModuleInfo of transitive clang deps
    "swift_deps",  # A SwiftCompiledModuleTset of SwiftCompiledModuleInfo of transitive swift deps
])

SdkSwiftOverlayInfo = provider(fields = [
    "overlays",  # {str: [str]} A mapping providing a list of overlay module names for each underlying module
])

SwiftCompiledModuleInfo = provider(fields = [
    "clang_importer_args",  # cmd_args of include flags for the clang importer.
    "is_framework",
    "is_swiftmodule",  # If True then contains a compiled swiftmodule, otherwise Clang's pcm.
    "module_name",  # A real name of a module, without distinguishing suffixes.
    "output_artifact",  # Compiled artifact either swiftmodule or pcm.
])

def _add_swiftmodule_search_path(module_info: SwiftCompiledModuleInfo):
    # We need to import the containing folder, not the file itself.
    return ["-I", cmd_args(module_info.output_artifact).parent()] if module_info.is_swiftmodule else []

def _add_clang_import_flags(module_info: SwiftCompiledModuleInfo):
    if module_info.is_swiftmodule:
        return []
    else:
        return [module_info.clang_importer_args]

def _swift_module_map_struct(module_info: SwiftCompiledModuleInfo.type):
    return struct(
        isFramework = module_info.is_framework,
        moduleName = module_info.module_name,
        modulePath = module_info.output_artifact,
    )

SwiftCompiledModuleTset = transitive_set(
    args_projections = {
        "clang_deps": _add_clang_import_flags,
        "module_search_path": _add_swiftmodule_search_path,
    },
    json_projections = {
        "swift_module_map": _swift_module_map_struct,
    },
)
