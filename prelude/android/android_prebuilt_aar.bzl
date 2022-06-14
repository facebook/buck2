load("@fbcode//buck2/prelude/android:android_providers.bzl", "AndroidResourceInfo", "CPU_FILTER_TO_ABI_DIRECTORY", "NativeLibraryFromPrebuiltAar", "merge_android_packageable_info")
load("@fbcode//buck2/prelude/android:android_resource.bzl", "aapt2_compile", "extract_package_from_manifest")
load("@fbcode//buck2/prelude/android:android_toolchain.bzl", "AndroidToolchainInfo")
load(
    "@fbcode//buck2/prelude/java:java_providers.bzl",
    "JavaClasspathEntry",
    "create_java_library_providers",
    "maybe_create_abi",
)
load("@fbcode//buck2/prelude/java:java_toolchain.bzl", "JavaToolchainInfo")

def android_prebuilt_aar_impl(ctx: "context") -> ["provider"]:
    manifest = ctx.actions.declare_output("unpack_dir/AndroidManifest.xml")
    unpack_dir = cmd_args(manifest.as_output()).parent()
    all_classes_jar = ctx.actions.declare_output("classes.jar")
    r_dot_txt = ctx.actions.declare_output("unpack_dir/R.txt")
    res = ctx.actions.declare_output("unpack_dir/res")
    assets = ctx.actions.declare_output("unpack_dir/assets")

    android_toolchain = ctx.attr._android_toolchain[AndroidToolchainInfo]
    unpack_aar_tool = android_toolchain.unpack_aar[RunInfo]
    java_toolchain = ctx.attr._java_toolchain[JavaToolchainInfo]
    jar_tool = java_toolchain.jar

    sub_dir_paths = {cpu_type: ctx.actions.declare_output("unpack_dir/jni/{}".format(
        abi_directory,
    )) for cpu_type, abi_directory in CPU_FILTER_TO_ABI_DIRECTORY.items()}

    unpack_aar_cmd = [
        unpack_aar_tool,
        "--aar",
        ctx.attr.aar,
        "--unpack-dir",
        unpack_dir,
        "--manifest-path",
        manifest.as_output(),
        "--all-classes-jar-path",
        all_classes_jar.as_output(),
        "--r-dot-txt-path",
        r_dot_txt.as_output(),
        "--res-path",
        res.as_output(),
        "--assets-path",
        assets.as_output(),
        "--jar-tool",
        jar_tool,
        "--native-libs-sub-dir-paths",
    ] + [sub_dir_path.as_output() for sub_dir_path in sub_dir_paths.values()]

    ctx.actions.run(unpack_aar_cmd, category = "android_unpack_aar")

    resource_info = AndroidResourceInfo(
        aapt2_compile_output = aapt2_compile(ctx, res, android_toolchain),
        assets = assets,
        manifest_file = manifest,
        r_dot_java_package = extract_package_from_manifest(ctx, manifest),
        res = res,
        text_symbols = r_dot_txt,
    )

    abi = maybe_create_abi(ctx.actions, java_toolchain, all_classes_jar)
    library_output_classpath_entry = JavaClasspathEntry(
        full_library = all_classes_jar,
        abi = abi or all_classes_jar,
        required_for_source_only_abi = ctx.attr.required_for_source_only_abi,
    )

    java_library_info, java_packaging_info, shared_library_info, cxx_resource_info, template_placeholder_info = create_java_library_providers(
        ctx = ctx,
        library_output = library_output_classpath_entry,
        exported_deps = ctx.attr.deps,
        needs_desugar = True,
        is_prebuilt_jar = True,
    )

    native_library = NativeLibraryFromPrebuiltAar(
        sub_dirs = sub_dir_paths,
        use_system_library_loader = ctx.attr.use_system_library_loader,
    )

    return [
        java_library_info,
        java_packaging_info,
        shared_library_info,
        cxx_resource_info,
        template_placeholder_info,
        merge_android_packageable_info(ctx.actions, ctx.attr.deps, manifest = manifest, native_lib_from_prebuilt_aar = native_library, resource_info = resource_info),
        resource_info,
        DefaultInfo(default_outputs = [all_classes_jar]),
    ]
