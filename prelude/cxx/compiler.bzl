# TODO(T110378132): Added here for compat with v1, but this might make more
# sense on the toolchain definition.
def get_flags_for_reproducible_build(compiler_type: str.type) -> [str.type]:
    """
    Return flags needed to make compilations reproducible (e.g. avoiding
    embedding the working directory into debug info.
    """

    flags = []

    if compiler_type == "clang":
        flags.extend(["-Xclang", "-fdebug-compilation-dir", "-Xclang", "."])

    return flags

def get_flags_for_colorful_output(compiler_type: str.type) -> [str.type]:
    """
    Return flags for enabling colorful diagnostic output.
    """
    flags = []
    if compiler_type == "clang":
        # https://clang.llvm.org/docs/UsersManual.html
        flags.append("-fcolor-diagnostics")
    elif compiler_type == "gcc":
        # https://gcc.gnu.org/onlinedocs/gcc/Diagnostic-Message-Formatting-Options.html
        flags.append("-fdiagnostics-color=always")

    return flags

def cc_dep_files(output: "_arglike") -> cmd_args.type:
    return cmd_args(["-MD", "-MF", output])

def get_headers_dep_files_flags_factory(compiler_type: str.type) -> ["function", None]:
    if compiler_type == "clang" or compiler_type == "gcc":
        return cc_dep_files

    return None
