# Input to build Python libraries and binaries (which are libraries wrapped in
# an executable). The various functions here must returns the inputs annotated
# below.
PythonLibraryInterface = record(
    # Shared libraries used by this Python library.
    # {str.type: SharedLibraryInfo.type}
    shared_libraries = field("function"),

    # An iterator of PythonLibraryManifests objects. This is used to collect extensions.
    # iterator of PythonLibraryManifests
    iter_manifests = field("function"),

    # A PythonLibraryManifestsInterface. This is used to convert manifests to
    # arguments for pexing. Unlike iter_manifests this allows for more
    # efficient calls, such as using t-sets projections.
    # PythonLibraryManifestsInterface
    manifests = field("function"),

    # Returns whether this Python library includes hidden resources.
    # bool
    has_hidden_resources = field("function"),

    # Converts the hidden resources in this Python library to arguments.
    # _arglike of hidden resources
    hidden_resources = field("function"),
)

PythonLibraryManifestsInterface = record(
    # Returns the source manifests for this Python library.
    # [_arglike] of source manifests
    src_manifests = field("function"),

    # Returns the files referenced by source manifests for this Python library.
    # [_arglike] of source artifacts
    src_artifacts = field("function"),

    # Returns the source manifests for this Python library.
    # [_arglike] of source manifests
    src_type_manifests = field("function"),

    # Returns the files referenced by source manifests for this Python library.
    # [_arglike] of source artifacts
    src_type_artifacts = field("function"),

    # Returns the bytecode manifests for this Python library.
    # [_arglike] of bytecode manifests
    bytecode_manifests = field("function"),

    # Returns the files referenced by bytecode manifests for this Python library.
    # [_arglike] of bytecode artifacts
    bytecode_artifacts = field("function"),

    # Returns the resources manifests for this Python library.
    # [_arglike] of resource manifests
    resource_manifests = field("function"),

    # Returns the files referenced by resource manifests for this Python library.
    # [_arglike] of resource artifacts
    resource_artifacts = field("function"),
)
