#####################################################################
# Providers

SwiftToolchainInfo = provider(fields = [
    "architecture",
    "compiler",
    "compiler_flags",
    "swift_stdlib_tool",
    "swift_stdlib_tool_flags",
    "sdk_path",
    "resource_dir",  # "artifact"
])
