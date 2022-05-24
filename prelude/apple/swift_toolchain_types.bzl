#####################################################################
# Providers

SwiftToolchainInfo = provider(fields = [
    "architecture",
    "compiler",
    "compiler_flags",
    "swift_stdlib_tool",
    "swift_stdlib_tool_flags",
    "sdk_path",
    "compiled_sdk_swift_modules",  # {str.type: SdkCompiledModuleInfo} Expose providers of compiled Swift SDK modules.
    "compiled_sdk_clang_modules",  # {str.type: SdkCompiledModuleInfo} Expose providers of compiled Clang SDK modules.
    "resource_dir",  # "artifact"
])

# A provider that represents a non-yet-compiled SDK (Swift or Clang) module,
# and doesn't contain any artifacts because Swift toolchain isn't resolved yet.
SdkUncompiledModuleInfo = provider(fields = [
    "name",
    "is_framework",  # This is mostly needed for the generated Swift module map file.
    "is_swiftmodule",  # If True then represents a swiftinterface, otherwise Clang's modulemap.
    "partial_cmd",  # Partial arguments, required to compile a particular SDK module.
    "input_relative_path",  # A relative prefixed path to a textual swiftinterface/modulemap file within an SDK.
    "deps",  # [SdkUncompiledModuleInfo]
])

# A provider that represents an already-compiled SDK (Swift or Clang) module.
SdkCompiledModuleInfo = provider(fields = [
    "name",
    "is_swiftmodule",  # If True then contains a compiled swiftmodule, otherwise Clang's pcm.
    "output_artifact",  # Compiled artifact either swiftmodule or pcm.
    "input_relative_path",
    "deps",  # A TSet of [SdkCompiledModuleInfo]
])
