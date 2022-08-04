load(
    "@fbcode//buck2/prelude/cxx:cxx_context.bzl",
    "CxxContext",  # @unused Used as a type
)

def strip_debug_info(cxx_context: CxxContext.type, name: str.type, obj: "artifact") -> "artifact":
    """
    Strip debug information from an object.
    """
    strip = cxx_context.cxx_toolchain_info.binary_utilities_info.strip
    output = cxx_context.actions.declare_output(name)
    cmd = cmd_args([strip, "--strip-debug", "-o", output.as_output(), obj])
    cxx_context.actions.run(cmd, category = "strip_debug", identifier = name)
    return output

def strip_shared_library(cxx_context: CxxContext.type, shared_lib: "artifact", strip_flags: "cmd_args") -> "artifact":
    """
    Strip unneeded information from a shared library.
    """
    strip = cxx_context.cxx_toolchain_info.binary_utilities_info.strip
    stripped_lib = cxx_context.actions.declare_output("stripped/{}".format(shared_lib.short_path))

    # TODO(T109996375) support configuring the flags used for stripping
    cmd = cmd_args()
    cmd.add(strip)
    cmd.add(strip_flags)
    cmd.add([shared_lib, "-o", stripped_lib.as_output()])

    cxx_context.actions.run(cmd, category = "strip_shared_lib", identifier = shared_lib.short_path)

    return stripped_lib
