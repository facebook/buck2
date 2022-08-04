load("@fbcode//buck2/prelude:paths.bzl", "paths")
load(
    ":compile.bzl",
    "CxxSrcCompileCommand",  # @unused Used as a type
)
load(":cxx_context.bzl", "CxxContext")  # @unused Used as a type

# Provider that exposes the compilation database information
CxxCompilationDbInfo = provider(fields = {
    "info": "A map of the file (an \"artifact.type\") to its corresponding \"CxxSrcCompileCommand\"",
    "platform": "platform for this compilation database",
    "toolchain": "toolchain for this compilation database",
})

def make_compilation_db_info(src_compile_cmds: [CxxSrcCompileCommand.type], toolchainInfo: "CxxToolchainInfo", platformInfo: "CxxPlatformInfo") -> CxxCompilationDbInfo.type:
    info = {}
    for src_compile_cmd in src_compile_cmds:
        info.update({src_compile_cmd.src: src_compile_cmd})

    return CxxCompilationDbInfo(info = info, toolchain = toolchainInfo, platform = platformInfo)

def create_compilation_database(
        cxx_context: CxxContext.type,
        src_compile_cmds: [CxxSrcCompileCommand.type]) -> DefaultInfo.type:
    mk_comp_db = cxx_context.cxx_toolchain_info.mk_comp_db[RunInfo]

    # Generate the per-source compilation DB entries.
    entries = {}
    other_outputs = []

    for src_compile_cmd in src_compile_cmds:
        cdb_path = paths.join("__comp_db__", src_compile_cmd.src.short_path + ".comp_db.json")
        if cdb_path not in entries:
            entry = cxx_context.actions.declare_output(cdb_path)
            cmd = cmd_args(mk_comp_db)
            cmd.add("gen")
            cmd.add(cmd_args(entry.as_output(), format = "--output={}"))
            cmd.add(src_compile_cmd.src.basename)
            cmd.add(cmd_args(src_compile_cmd.src).parent())
            cmd.add("--")
            cmd.add(src_compile_cmd.cxx_compile_cmd.base_compile_cmd)
            cmd.add(src_compile_cmd.cxx_compile_cmd.argsfile.cmd_form)
            cmd.add(src_compile_cmd.args)
            cxx_context.actions.run(cmd, category = "cxx_compilation_database", identifier = src_compile_cmd.src.short_path)

            # Add all inputs the command uses to runtime files.
            other_outputs.append(cmd)
            entries[cdb_path] = entry

    # Merge all entries into the actual compilation DB.
    db = cxx_context.actions.declare_output("compile_commands.json")
    cmd = cmd_args(mk_comp_db)
    cmd.add("merge")
    cmd.add(cmd_args(db.as_output(), format = "--output={}"))
    cmd.add(entries.values())
    cxx_context.actions.run(cmd, category = "cxx_compilation_database_merge")

    return DefaultInfo(default_outputs = [db], other_outputs = other_outputs)
