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
    "name",  # A name of a module with `.swift`/`.clang` suffix.
    "module_name",  # A real name of a module, without distinguishing suffixes.
    "is_framework",  # This is mostly needed for the generated Swift module map file.
    "is_swiftmodule",  # If True then represents a swiftinterface, otherwise Clang's modulemap.
    "partial_cmd",  # Partial arguments, required to compile a particular SDK module.
    "input_relative_path",  # A relative prefixed path to a textual swiftinterface/modulemap file within an SDK.
    "deps",  # [Dependency]
    "target",  # A string of the compiler target triple to use for clang module deps, eg arm64-apple-ios16.4
    "transitive_clang_deps",  # A SdkTransitiveDepsTset of all transitive SDK clang modules.
    "transitive_swift_deps",  # A SdkTransitiveDepsTset of all transitive SDK swift modules.
])

SdkTransitiveDepsTset = transitive_set()

SdkDependencyInfo = provider(fields = [
    "clang_deps",  # A tset of SdkCompiledModuleInfo clang modules
    "swift_deps",  # A tset of SdkCompiledModuleInfo swift modules
])

# A provider that represents an already-compiled SDK (Swift or Clang) module.
SdkCompiledModuleInfo = provider(fields = [
    "name",  # A name of a module with `.swift`/`.clang` suffix.
    "module_name",  # A real name of a module, without distinguishing suffixes.
    "is_swiftmodule",  # If True then contains a compiled swiftmodule, otherwise Clang's pcm.
    "is_framework",
    "output_artifact",  # Compiled artifact either swiftmodule or pcm.
    "input_relative_path",
])

SdkSwiftOverlayInfo = provider(fields = [
    "overlays",  # {str: [str]} A mapping providing a list of overlay module names for each underlying module
])
