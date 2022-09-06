DexLibraryInfo = provider(
    fields = [
        # the .dex.jar file. May be None if there were not any Java classes to dex. If None, the
        # remaining fields should be ignored.
        "dex",  # ["artifact", None]
        # the names of the .class files that went into the DEX file
        "class_names",  # ["artifact", None]
        # resources that are referenced by the classes in this DEX file
        "referenced_resources",  # ["artifact", None]
        # a value that estimates how much space the code represented by this object will take up in
        # a DEX file. The units for this estimate are not important, as long as they are consistent
        # with those used when determining how secondary DEX files should be packed.
        "weight_estimate",  # ["artifact", None]
    ],
)

def get_dex_produced_from_java_library(
        ctx: "context",
        dex_toolchain: "DexToolchainInfo",
        jar_to_dex: "artifact",
        needs_desugar: bool.type = False,
        desugar_deps: ["artifact"] = []) -> "DexLibraryInfo":
    # TODO(T102963008) check whether the java_library actually contains any classes

    d8_cmd = cmd_args(dex_toolchain.d8_command[RunInfo])

    library_path = jar_to_dex.short_path
    prefix = "dex/{}".format(library_path)
    output_dex_file = ctx.actions.declare_output(prefix + ".dex.jar")
    d8_cmd.add(["--output-dex-file", output_dex_file.as_output()])

    jar_to_dex_file = ctx.actions.write(prefix + "_jar_to_dex_file.txt", jar_to_dex)
    d8_cmd.add(["--files-to-dex-list", jar_to_dex_file])
    d8_cmd.hidden(jar_to_dex)

    d8_cmd.add(["--android-jar", dex_toolchain.android_jar])

    d8_cmd.add(["--intermediate", "--no-optimize", "--force-jumbo"])
    if not needs_desugar:
        d8_cmd.add("--no-desugar")
    else:
        desugar_deps_file = ctx.actions.write(prefix + "_desugar_deps_file.txt", desugar_deps)
        d8_cmd.add(["--classpath-files", desugar_deps_file])
        d8_cmd.hidden(desugar_deps)

    referenced_resources_file = ctx.actions.declare_output(prefix + "_referenced_resources.txt")
    d8_cmd.add(["--referenced-resources-path", referenced_resources_file.as_output()])

    weight_estimate_file = ctx.actions.declare_output(prefix + "_weight_estimate.txt")
    d8_cmd.add(["--weight-estimate-path", weight_estimate_file.as_output()])

    class_names_file = ctx.actions.declare_output(prefix + "_class_names.txt")
    d8_cmd.add(["--class-names-path", class_names_file.as_output()])

    ctx.actions.run(
        d8_cmd,
        category = "d8",
        identifier = "{}:{} {}".format(ctx.label.package, ctx.label.name, output_dex_file.short_path),
    )

    return DexLibraryInfo(
        dex = output_dex_file,
        class_names = class_names_file,
        referenced_resources = referenced_resources_file,
        weight_estimate = weight_estimate_file,
    )
