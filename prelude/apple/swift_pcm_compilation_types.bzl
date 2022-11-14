SwiftPCMCompilationInfo = provider(fields = [
    "name",
    "pcm_output",
    "exported_pre",
    "deps_set",
    "sdk_deps_set",  # A TSet of direct and transitive SDK deps.
])
