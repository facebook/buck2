load("@prelude//utils:utils.bzl", "expect")

# Platform-specific linker flags handling.  Modeled after the `Linker` abstraction
# in v1 (https://fburl.com/diffusion/kqd2ylcy).
# TODO(T110378136): It might make more sense to pass these in via the toolchain.
Linker = record(
    # The format to use for the shared library name.
    shared_library_name_format = str.type,
    # The format to use for the versioned shared library name.
    shared_library_versioned_name_format = str.type,
    # How to format arguments to the linker to set a shared lib name.
    shared_library_name_linker_flags_format = [str.type],
    # Flags to pass to the linker to make it generate a shared library.
    shared_library_flags = [str.type],
)

# Allows overriding the default shared library flags.
# e.g. when building Apple tests, we want to link with `-bundle` instead of `-shared` to allow
# linking against the bundle loader.
SharedLibraryFlagOverrides = record(
    # How to format arguments to the linker to set a shared lib name.
    shared_library_name_linker_flags_format = [str.type],
    # Flags to pass to the linker to make it generate a shared library.
    shared_library_flags = [str.type],
)

LINKERS = {
    "darwin": Linker(
        shared_library_name_format = "lib{}.dylib",
        shared_library_versioned_name_format = "lib{}.{}.dylib",
        shared_library_name_linker_flags_format = ["-install_name", "@rpath/{}"],
        shared_library_flags = ["-shared"],
    ),
    "gnu": Linker(
        shared_library_name_format = "lib{}.so",
        shared_library_versioned_name_format = "lib{}.so.{}",
        shared_library_name_linker_flags_format = ["-Wl,-soname,{}"],
        shared_library_flags = ["-shared"],
    ),
    "windows": Linker(
        shared_library_name_format = "{}.dll",
        shared_library_versioned_name_format = "{}.dll",
        # NOTE(agallagher): I *think* windows doesn't support a flag to set the
        # library name, and relies on the basename.
        shared_library_name_linker_flags_format = [],
        shared_library_flags = ["/DLL"],
    ),
}

def _sanitize(s: str.type) -> str.type:
    return s.replace("/", "_")

# NOTE(agallagher): Does this belong in the native/shared_libraries.bzl?
def get_shared_library_name(
        linker_type: str.type,
        short_name: str.type,
        version: [str.type, None] = None):
    """
    Generate a platform-specific shared library name based for the given rule.
    """
    if version == None:
        return LINKERS[linker_type].shared_library_name_format.format(short_name)
    else:
        return LINKERS[linker_type].shared_library_versioned_name_format.format(short_name, version)

def _parse_ext_macro(name: str.type) -> [(str.type, [str.type, None]), None]:
    """
    Parse the `$(ext[ <version>])` macro from a user-specific library name,
    which expands to a platform-specific suffix (e.g. `.so`, `.dylib`).  If an
    optional version argument is given (e.g. `$(ext 3.4)`) it expands to a
    platform-specific versioned suffix (e.g. `.so.3.4`, `.3.4.dylib`).
    """

    # If there's no macro, then there's nothing to do.
    if ".$(ext" not in name:
        return None
    expect(name.endswith(")"))

    # Otherwise, attempt to parse out the macro.
    base, rest = name.split(".$(ext")

    # If the macro is arg-less, then return w/o a version.
    if rest == ")":
        return (base, None)

    # Otherwise, extract the version from the arg.
    expect(rest.startswith(" "))
    return (base, rest[1:-1])

def get_shared_library_name_for_param(linker_type: str.type, name: str.type):
    """
    Format a user-provided shared library name, supporting v1's `$(ext)` suffix.
    """
    parsed = _parse_ext_macro(name)
    if parsed != None:
        base, version = parsed
        name = get_shared_library_name(
            linker_type,
            base.removeprefix("lib"),
            version = version,
        )
    return name

# NOTE(agallagher): Does this belong in the native/shared_libraries.bzl?
def get_default_shared_library_name(linker_type: str.type, label: "label"):
    """
    Generate a platform-specific shared library name based for the given rule.
    """

    # TODO(T110378119): v1 doesn't use the cell/repo name, so we don't here for
    # initial compatiblity, but maybe we should?
    short_name = "{}_{}".format(_sanitize(label.package), _sanitize(label.name))
    return get_shared_library_name(linker_type, short_name)

def get_shared_library_name_linker_flags(linker_type: str.type, soname: str.type, flag_overrides: [SharedLibraryFlagOverrides.type, None] = None) -> [str.type]:
    """
    Arguments to pass to the linker to set the given soname.
    """
    if flag_overrides:
        shared_library_name_linker_flags_format = flag_overrides.shared_library_name_linker_flags_format
    else:
        shared_library_name_linker_flags_format = LINKERS[linker_type].shared_library_name_linker_flags_format

    return [
        f.format(soname)
        for f in shared_library_name_linker_flags_format
    ]

def get_shared_library_flags(linker_type: str.type, flag_overrides: [SharedLibraryFlagOverrides.type, None] = None) -> [str.type]:
    """
    Arguments to pass to the linker to link a shared library.
    """
    if flag_overrides:
        return flag_overrides.shared_library_flags

    return LINKERS[linker_type].shared_library_flags

def get_link_whole_args(linker_type: str.type, inputs: ["artifact"]) -> [""]:
    """
    Return linker args used to always link all the given inputs.
    """

    args = []

    if linker_type == "gnu":
        args.append("-Wl,--whole-archive")
        args.extend(inputs)
        args.append("-Wl,--no-whole-archive")
    elif linker_type == "darwin":
        for inp in inputs:
            args.append("-Xlinker")
            args.append("-force_load")
            args.append("-Xlinker")
            args.append(inp)
    else:
        fail("Linker type {} not supported".format(linker_type))

    return args

def get_objects_as_library_args(linker_type: str.type, objects: ["artifact"]) -> [""]:
    """
    Return linker args used to link the given objects as a library.
    """

    args = []

    if linker_type == "gnu":
        args.append("-Wl,--start-lib")
        args.extend(objects)
        args.append("-Wl,--end-lib")
    else:
        fail("Linker type {} not supported".format(linker_type))

    return args

def get_ignore_undefined_symbols_flags(linker_type: str.type) -> [str.type]:
    """
    Return linker args used to suppress undefined symbol errors.
    """

    args = []

    if linker_type == "gnu":
        args.append("-Wl,--allow-shlib-undefined")
        args.append("-Wl,--unresolved-symbols=ignore-all")
    elif linker_type == "darwin":
        args.append("-Wl,-flat_namespace,-undefined,suppress")
    else:
        fail("Linker type {} not supported".format(linker_type))

    return args

def get_no_as_needed_shared_libs_flags(linker_type: str.type) -> [str.type]:
    """
    Return linker args used to prevent linkers from dropping unused shared
    library dependencies from the e.g. DT_NEEDED tags of the link.
    """

    args = []

    if linker_type == "gnu":
        args.append("-Wl,--no-as-needed")
    elif linker_type == "darwin":
        pass
    else:
        fail("Linker type {} not supported".format(linker_type))

    return args
