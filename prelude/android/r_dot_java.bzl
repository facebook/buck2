load("@fbcode//buck2/prelude/java:java_library.bzl", "compile_to_jar")
load("@fbcode//buck2/prelude/java:java_providers.bzl", "JavaClasspathEntry", "JavaLibraryInfo", "derive_compiling_deps")

def get_dummy_r_dot_java(
        ctx: "context",
        merge_android_resources_tool: RunInfo.type,
        java_toolchain: "JavaToolchainInfo",
        android_resources: ["AndroidResourceInfo"],
        union_package: [str.type, None]) -> "JavaLibraryInfo":
    r_dot_java_source_code_dir = _generate_r_dot_java_source_code(ctx, merge_android_resources_tool, android_resources, union_package = union_package)
    library_output = _generate_and_compile_r_dot_java(ctx, r_dot_java_source_code_dir, java_toolchain)
    return JavaLibraryInfo(
        compiling_deps = derive_compiling_deps(ctx.actions, library_output, []),
        library_output = library_output,
        output_for_classpath_macro = library_output.full_library,
    )

def generate_r_dot_java(
        ctx: "context",
        merge_android_resources_tool: RunInfo.type,
        java_toolchain: "JavaToolchainInfo",
        android_resources: ["AndroidResourceInfo"],
        banned_duplicate_resource_types: [str.type],
        uber_r_dot_txt_files: ["artifact"],
        override_symbols_paths: ["artifact"],
        duplicate_resources_allowlist: ["artifact", None],
        union_package: [str.type, None],
        referenced_resources_lists: ["artifact"]) -> "JavaLibraryInfo":
    r_dot_java_source_code_dir = _generate_r_dot_java_source_code(
        ctx,
        merge_android_resources_tool,
        android_resources,
        force_final_resources_ids = True,
        banned_duplicate_resource_types = banned_duplicate_resource_types,
        uber_r_dot_txt_files = uber_r_dot_txt_files,
        override_symbols_paths = override_symbols_paths,
        duplicate_resources_allowlist = duplicate_resources_allowlist,
        union_package = union_package,
        referenced_resources_lists = referenced_resources_lists,
    )

    library_output = _generate_and_compile_r_dot_java(ctx, r_dot_java_source_code_dir, java_toolchain)
    return JavaLibraryInfo(
        compiling_deps = derive_compiling_deps(ctx.actions, library_output, []),
        library_output = library_output,
        output_for_classpath_macro = library_output.full_library,
    )

def _generate_r_dot_java_source_code(
        ctx: "context",
        merge_android_resources_tool: RunInfo.type,
        android_resources: ["AndroidResourceInfo"],
        force_final_resources_ids = False,
        banned_duplicate_resource_types: [str.type] = [],
        uber_r_dot_txt_files: ["artifact"] = [],
        override_symbols_paths: ["artifact"] = [],
        duplicate_resources_allowlist: ["artifact", None] = None,
        union_package: [str.type, None] = None,
        referenced_resources_lists: ["artifact"] = []) -> "artifact":
    merge_resources_cmd = cmd_args(merge_android_resources_tool)

    r_dot_txt_info = cmd_args()
    for android_resource in android_resources:
        r_dot_txt_info.add(cmd_args([android_resource.text_symbols, android_resource.r_dot_java_package, "_"], delimiter = " "))  # pass target name

    r_dot_txt_info_file = ctx.actions.write("r_dot_txt_info_file", r_dot_txt_info)
    merge_resources_cmd.add(["--symbol-file-info", r_dot_txt_info_file])
    merge_resources_cmd.hidden([android_resource.r_dot_java_package for android_resource in android_resources])
    merge_resources_cmd.hidden([android_resource.text_symbols for android_resource in android_resources])

    output_dir = ctx.actions.declare_output("r_dot_java_source_code")
    merge_resources_cmd.add(["--output-dir", output_dir.as_output()])

    if force_final_resources_ids:
        merge_resources_cmd.add("--force-final-resource-ids")

    if len(banned_duplicate_resource_types) > 0:
        banned_duplicate_resource_types_file = ctx.actions.write("banned_duplicate_resource_types_file", banned_duplicate_resource_types)
        merge_resources_cmd.add(["--banned-duplicate-resource-types", banned_duplicate_resource_types_file])

    if len(uber_r_dot_txt_files) > 0:
        uber_r_dot_txt_files_list = ctx.actions.write("uber_r_dot_txt_files_list", uber_r_dot_txt_files)
        merge_resources_cmd.add(["--uber-r-dot-txt", uber_r_dot_txt_files_list])
        merge_resources_cmd.hidden(uber_r_dot_txt_files)

    if len(override_symbols_paths) > 0:
        override_symbols_paths_list = ctx.actions.write("override_symbols_paths_list", override_symbols_paths)
        merge_resources_cmd.add(["--override-symbols", override_symbols_paths_list])
        merge_resources_cmd.hidden(override_symbols_paths)

    if duplicate_resources_allowlist != None:
        merge_resources_cmd.add(["--duplicate-resource-allowlist-path", duplicate_resources_allowlist])

    if union_package != None:
        merge_resources_cmd.add(["--union-package", union_package])

    if referenced_resources_lists:
        referenced_resources_file = ctx.actions.write("referenced_resources_lists", referenced_resources_lists)
        merge_resources_cmd.add(["--referenced-resources-lists", referenced_resources_file])
        merge_resources_cmd.hidden(referenced_resources_lists)

    ctx.actions.run(merge_resources_cmd, category = "r_dot_java_merge_resources")

    return output_dir

def _generate_and_compile_r_dot_java(
        ctx: "context",
        r_dot_java_source_code_dir: "artifact",
        java_toolchain: "JavaToolchainInfo") -> JavaClasspathEntry.type:
    r_dot_java_out = ctx.actions.declare_output("r_dot_java.jar")
    r_dot_java_src_listing = ctx.actions.declare_output("r_dot_java.srclist")
    ctx.actions.run(
        [
            java_toolchain.src_dir_helper[RunInfo],
            "list",
            "--dir",
            r_dot_java_source_code_dir,
            "--output",
            r_dot_java_src_listing.as_output(),
        ],
        category = "list_r_dot_java_srcs",
    )

    def compile_r_dot_java_srcs(ctx):
        src_listing = ctx.artifacts[r_dot_java_src_listing].read_string().split("\n")
        r_dot_java_srcs = []
        copied_root = ctx.actions.declare_output("copied_r_dot_java")
        for path in src_listing:
            r_dot_java_srcs.append(copied_root.project(path))

        cmd = cmd_args([
            java_toolchain.src_dir_helper[RunInfo],
            "copy",
            "--src-dir",
            r_dot_java_source_code_dir,
            "--dest-dir",
            copied_root.as_output(),
        ] + src_listing)
        ctx.actions.run(
            cmd,
            category = "copy_r_dot_java_sources",
        )

        compile_to_jar(
            ctx,
            output = ctx.outputs[r_dot_java_out],
            actions_prefix = "r_dot_java",
            javac_tool = None,
            srcs = r_dot_java_srcs,
        )

    # Extracting an abi is unnecessary as there's not really anything to strip.
    outputs = JavaClasspathEntry(
        full_library = r_dot_java_out,
        abi = r_dot_java_out,
        required_for_source_only_abi = False,
    )

    todo_inputs = []
    ctx.actions.dynamic_output([r_dot_java_src_listing], todo_inputs, [r_dot_java_out], compile_r_dot_java_srcs)
    return outputs
