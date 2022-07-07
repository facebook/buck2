load("@fbcode//buck2/prelude/linking:link_info.bzl", "Archive")
load(":cxx_context.bzl", "get_cxx_toolchain_info")

def _supports_thin(linker_type: str.type) -> bool.type:
    return linker_type == "gnu"

def _archive_flags(linker_type: str.type, use_archiver_flags: bool.type, thin: bool.type) -> [str.type]:
    if not use_archiver_flags:
        return []

    flags = ""

    # Operate in quick append mode, so that objects with identical basenames
    # won't overwrite one another.
    flags += "q"

    # Suppress warning about creating a new archive.
    flags += "c"

    # Run ranlib to generate symbol index for faster linking.
    flags += "s"

    # Generate thin archives.
    if thin:
        flags += "T"

    # GNU archivers support generating deterministic archives.
    if linker_type == "gnu":
        flags += "D"

    return [flags]

# Create a static library from a list of object files.
def _archive(ctx: "context", name: str.type, args: "cmd_args", thin: bool.type, prefer_local: bool.type) -> "artifact":
    archive_output = ctx.actions.declare_output(name)
    toolchain = get_cxx_toolchain_info(ctx)
    command = cmd_args(toolchain.linker_info.archiver)
    command.add(_archive_flags(toolchain.linker_info.type, toolchain.linker_info.use_archiver_flags, thin))
    command.add([archive_output.as_output()])
    command.add(args)
    category = "archive"
    if thin:
        category = "archive_thin"
    ctx.actions.run(command, category = category, identifier = name, prefer_local = prefer_local)
    return archive_output

# Creates a static library given a list of object files.
def make_archive(
        ctx: "context",
        name: str.type,
        objects: ["artifact"],
        args: ["cmd_args", None] = None) -> Archive.type:
    if len(objects) == 0:
        fail("no objects to archive")

    if args == None:
        args = cmd_args(objects)

    linker_info = get_cxx_toolchain_info(ctx).linker_info
    thin = _supports_thin(linker_info.type) and linker_info.archive_contents == "thin"
    archive = _archive(ctx, name, args, thin = thin, prefer_local = linker_info.archive_objects_locally)

    # TODO(T110378125): use argsfiles for GNU archiver for long lists of objects.
    # TODO(T110378123): for BSD archiver, split long args over multiple invocations.
    # TODO(T110378100): We need to scrub the static library (timestamps, permissions, etc) as those are
    # sources of non-determinisim. See `ObjectFileScrubbers.createDateUidGidScrubber()` in Buck v1.

    return Archive(artifact = archive, external_objects = objects if thin else [])
