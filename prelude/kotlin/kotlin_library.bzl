# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

load("@prelude//:validation_deps.bzl", "get_validation_deps_outputs")
load("@prelude//android:android_providers.bzl", "merge_android_packageable_info")
load(
    "@prelude//java:java_library.bzl",
    "build_java_library",
    "split_on_archives_and_plain_files",
)
load(
    "@prelude//java:java_providers.bzl",
    "JavaClasspathEntry",
    "JavaCompilingDepsTSet",
    "JavaLibraryInfo",
    "JavaPackagingDepTSet",
    "JavaPackagingInfo",
    "JavaProviders",
    "create_java_library_providers",
    "create_native_providers",
    "derive_compiling_deps",
    "single_library_compiling_deps",
    "to_list",
)
load(
    "@prelude//java:java_toolchain.bzl",
    "AbiGenerationMode",
    "JavaToolchainInfo",
)
load("@prelude//java/plugins:java_annotation_processor.bzl", "AnnotationProcessorProperties", "create_annotation_processor_properties", "create_ksp_annotation_processor_properties")
load("@prelude//java/plugins:java_plugin.bzl", "create_plugin_params")
load("@prelude//java/utils:java_more_utils.bzl", "get_path_separator_for_exec_os")
load("@prelude//java/utils:java_utils.bzl", "derive_javac", "get_abi_generation_mode", "get_class_to_source_map_info", "get_default_info", "get_java_version_attributes")
load("@prelude//jvm:nullsafe.bzl", "get_nullsafe_info")
load(
    "@prelude//kotlin:kotlin_toolchain.bzl",
    "KotlinToolchainInfo",
)
load("@prelude//kotlin:kotlin_utils.bzl", "get_kotlinc_compatible_target")
load("@prelude//kotlin:kotlincd_jar_creator.bzl", "create_jar_artifact_kotlincd")
load("@prelude//utils:argfile.bzl", "at_argfile")
load("@prelude//utils:expect.bzl", "expect")
load("@prelude//utils:lazy.bzl", "lazy")
load("@prelude//utils:utils.bzl", "map_idx")

_JAVA_OR_KOTLIN_FILE_EXTENSION = [".java", ".kt"]

def _create_kotlin_sources(
        ctx: AnalysisContext,
        srcs: list[Artifact],
        deps: list[Dependency],
        annotation_processor_properties: AnnotationProcessorProperties,
        ksp_annotation_processor_properties: AnnotationProcessorProperties,
        additional_classpath_entries: JavaCompilingDepsTSet | None,
        bootclasspath_entries: list[Artifact]) -> (Artifact, Artifact | None, Artifact | None):
    """
    Runs kotlinc on the provided kotlin sources.
    """

    kotlin_toolchain = ctx.attrs._kotlin_toolchain[KotlinToolchainInfo]
    compile_kotlin_tool = kotlin_toolchain.compile_kotlin[RunInfo]
    kotlinc = kotlin_toolchain.kotlinc[RunInfo]
    kotlinc_output = ctx.actions.declare_output("kotlinc_classes_output", dir = True)

    compile_kotlin_cmd_args = [
        compile_kotlin_tool,
        "--kotlinc_output",
        kotlinc_output.as_output(),
    ]
    compile_kotlin_cmd_hidden = cmd_args()

    java_toolchain = ctx.attrs._java_toolchain[JavaToolchainInfo]
    zip_scrubber_args = ["--zip_scrubber", cmd_args(java_toolchain.zip_scrubber, delimiter = " ")]
    compile_kotlin_cmd_args.append(zip_scrubber_args)

    kotlinc_cmd_args = cmd_args([kotlinc])

    compiling_classpath = cmd_args()
    if additional_classpath_entries:
        compiling_classpath.add(additional_classpath_entries.project_as_args("args_for_compiling"))

    # kotlic doesn't support -bootclasspath param, so adding `bootclasspath_entries` into kotlin classpath
    compiling_classpath.add(bootclasspath_entries)

    compiling_deps_tset = derive_compiling_deps(ctx.actions, None, deps + [kotlin_toolchain.kotlin_stdlib])
    if compiling_deps_tset:
        compiling_classpath.add(compiling_deps_tset.project_as_args("args_for_compiling"))

    classpath_args = cmd_args(
        compiling_classpath,
        delimiter = get_path_separator_for_exec_os(ctx),
    )

    compile_kotlin_cmd_hidden.add(compiling_classpath)

    kotlinc_cmd_args.add(["-classpath"])
    kotlinc_cmd_args.add(at_argfile(
        actions = ctx.actions,
        name = "kotlinc_classpath",
        args = classpath_args,
        allow_args = True,
    ))

    module_name = ctx.label.package.replace("/", ".") + "." + ctx.label.name
    kotlinc_cmd_args.add(
        [
            "-module-name",
            module_name,
            "-no-stdlib",
            "-no-reflect",
        ] + ctx.attrs.extra_kotlinc_arguments + get_language_version_arg(ctx),
    )

    jvm_target = get_kotlinc_compatible_target(ctx.attrs.target) if ctx.attrs.target else None
    if jvm_target:
        kotlinc_cmd_args.add([
            "-jvm-target",
            jvm_target,
        ])

    kapt_generated_sources_output = None
    if annotation_processor_properties.annotation_processors:
        compile_kotlin_cmd_args.extend(["--kapt_annotation_processing_jar", kotlin_toolchain.annotation_processing_jar[JavaLibraryInfo].library_output.full_library])
        compile_kotlin_cmd_args.extend(["--kapt_annotation_processors", ",".join([p for ap in annotation_processor_properties.annotation_processors for p in ap.processors])])
        compile_kotlin_cmd_args.extend(["--kapt_annotation_processor_params", ";".join(annotation_processor_properties.annotation_processor_params)])

        annotation_processor_classpath_tsets = (
            filter(None, ([ap.deps for ap in annotation_processor_properties.annotation_processors])) +
            [dep[JavaPackagingInfo].packaging_deps for dep in [kotlin_toolchain.annotation_processing_jar, kotlin_toolchain.kotlin_stdlib]]
        )
        annotation_processor_classpath = ctx.actions.tset(
            JavaPackagingDepTSet,
            children = annotation_processor_classpath_tsets,
        ).project_as_args("full_jar_args")
        kapt_classpath_file = ctx.actions.write("kapt_classpath_file", annotation_processor_classpath)
        compile_kotlin_cmd_args.extend(["--kapt_classpath_file", kapt_classpath_file])
        compile_kotlin_cmd_hidden.add(annotation_processor_classpath)

        sources_output = ctx.actions.declare_output("kapt_sources_output")
        compile_kotlin_cmd_args.append(["--kapt_sources_output", sources_output.as_output()])
        classes_output = ctx.actions.declare_output("kapt_classes_output")
        compile_kotlin_cmd_args.append(["--kapt_classes_output", classes_output.as_output()])
        stubs = ctx.actions.declare_output("kapt_stubs")
        compile_kotlin_cmd_args.append(["--kapt_stubs", stubs.as_output()])

        kapt_generated_sources_output = ctx.actions.declare_output("kapt_generated_sources_output.src.zip")
        compile_kotlin_cmd_args.append(["--kapt_generated_sources_output", kapt_generated_sources_output.as_output()])
        compile_kotlin_cmd_args.append(["--kapt_base64_encoder", cmd_args(kotlin_toolchain.kapt_base64_encoder[RunInfo], delimiter = " ")])
        generated_kotlin_output = ctx.actions.declare_output("kapt_generated_kotlin_output")
        compile_kotlin_cmd_args.append(["--kapt_generated_kotlin_output", generated_kotlin_output.as_output()])
        if jvm_target:
            compile_kotlin_cmd_args.append(["--kapt_jvm_target", jvm_target])

    friend_paths = ctx.attrs.friend_paths
    if friend_paths:
        concat_friends_paths = cmd_args([friend_path.library_output.abi for friend_path in map_idx(JavaLibraryInfo, friend_paths) if friend_path.library_output], delimiter = ",")
        kotlinc_cmd_args.add(cmd_args(["-Xfriend-paths", concat_friends_paths], delimiter = "="))

    zipped_sources, plain_sources = split_on_archives_and_plain_files(srcs, _JAVA_OR_KOTLIN_FILE_EXTENSION)

    kotlinc_cmd_args.add(plain_sources)

    ksp_zipped_sources_output = None
    if ksp_annotation_processor_properties.annotation_processors:
        ksp_cmd = [compile_kotlin_tool]
        ksp_cmd.append(zip_scrubber_args)

        ksp_annotation_processor_classpath_tsets = filter(None, ([ap.deps for ap in ksp_annotation_processor_properties.annotation_processors]))
        if ksp_annotation_processor_classpath_tsets:
            ksp_annotation_processor_classpath = ctx.actions.tset(
                JavaPackagingDepTSet,
                children = ksp_annotation_processor_classpath_tsets,
            ).project_as_args("full_jar_args")
            ksp_cmd.append("--ksp_processor_jars")
            ksp_cmd.append(cmd_args(ksp_annotation_processor_classpath, delimiter = ","))

        ksp_cmd.extend(["--ksp_classpath", classpath_args])
        ksp_classes_and_resources_output = ctx.actions.declare_output("ksp_output_dir/ksp_classes_and_resources_output")
        ksp_cmd.extend(["--ksp_classes_and_resources_output", ksp_classes_and_resources_output.as_output()])
        ksp_output = cmd_args(ksp_classes_and_resources_output.as_output(), parent = 1)
        ksp_cmd.extend(["--ksp_output", ksp_output])
        ksp_sources_output = ctx.actions.declare_output("ksp_output_dir/ksp_sources_output")
        ksp_cmd.extend(["--ksp_sources_output", ksp_sources_output.as_output()])
        ksp_zipped_sources_output = ctx.actions.declare_output("ksp_output_dir/ksp_zipped_sources_output.src.zip")
        ksp_cmd.extend(["--ksp_zipped_sources_output", ksp_zipped_sources_output.as_output()])
        ksp_cmd.extend(["--ksp_project_base_dir", ctx.label.path])

        ksp_kotlinc_cmd_args = cmd_args(kotlinc_cmd_args)
        plugins_cmd_args = _add_plugins(ctx, is_ksp = True)
        ksp_kotlinc_cmd_args.add(plugins_cmd_args.kotlinc_cmd_args)
        ksp_cmd.append(plugins_cmd_args.compile_kotlin_cmd)

        ksp_cmd_args_file, _ = ctx.actions.write(
            "ksp_kotlinc_cmd",
            ksp_kotlinc_cmd_args,
            allow_args = True,
        )

        ksp_cmd.extend(["--kotlinc_cmd_file", ksp_cmd_args_file])

        ctx.actions.run(
            cmd_args(ksp_cmd, hidden = ksp_kotlinc_cmd_args),
            category = "ksp_kotlinc",
        )

        zipped_sources = (zipped_sources or []) + [ksp_zipped_sources_output]
        compile_kotlin_cmd_args.extend(["--ksp_generated_classes_and_resources", ksp_classes_and_resources_output])

    plugin_cmd_args = _add_plugins(ctx, is_ksp = False)
    kotlinc_cmd_args.add(plugin_cmd_args.kotlinc_cmd_args)
    compile_kotlin_cmd_args.append(plugin_cmd_args.compile_kotlin_cmd)

    if zipped_sources:
        zipped_sources_file = ctx.actions.write("kotlinc_zipped_source_args", zipped_sources)
        compile_kotlin_cmd_args.append(["--zipped_sources_file", zipped_sources_file])
        compile_kotlin_cmd_hidden.add(zipped_sources)

    args_file, _ = ctx.actions.write(
        "kotlinc_cmd",
        kotlinc_cmd_args,
        allow_args = True,
    )

    compile_kotlin_cmd_hidden.add(plain_sources)

    compile_kotlin_cmd_args.append("--kotlinc_cmd_file")
    compile_kotlin_cmd_args.append(args_file)
    compile_kotlin_cmd_hidden.add(kotlinc_cmd_args)

    ctx.actions.run(
        cmd_args(compile_kotlin_cmd_args, hidden = compile_kotlin_cmd_hidden),
        category = "kotlinc",
    )

    return kotlinc_output, kapt_generated_sources_output, ksp_zipped_sources_output

def _is_ksp_plugin(plugin: str) -> bool:
    return "symbol-processing" in plugin

_PluginCmdArgs = record(
    kotlinc_cmd_args = cmd_args,
    compile_kotlin_cmd = cmd_args,
)

def _add_plugins(
        ctx: AnalysisContext,
        is_ksp: bool) -> _PluginCmdArgs:
    kotlinc_cmd_args = cmd_args()
    compile_kotlin_cmd = cmd_args()
    for plugin, plugin_options in ctx.attrs.kotlin_compiler_plugins.items():
        if _is_ksp_plugin(str(plugin)) != is_ksp:
            continue

        kotlinc_cmd_args.add(cmd_args(["-Xplugin", plugin], delimiter = "="))
        options = []
        for option_key, option_val in plugin_options.items():
            # "_codegen_dir_" means buck should provide a dir
            if option_val == "__codegen_dir__":
                option_val = ctx.actions.declare_output("kotlin_compiler_plugin_dir")
                options.append(cmd_args([option_key, option_val.as_output()], delimiter = "="))
                compile_kotlin_cmd.add(["--kotlin_compiler_plugin_dir", option_val.as_output()])
            else:
                options.append(cmd_args([option_key, option_val], delimiter = "="))

        if options:
            kotlinc_cmd_args.add(["-P", cmd_args(options, delimiter = ",")])

    return _PluginCmdArgs(kotlinc_cmd_args = kotlinc_cmd_args, compile_kotlin_cmd = compile_kotlin_cmd)

def get_language_version(ctx: AnalysisContext) -> str:
    kotlin_toolchain = ctx.attrs._kotlin_toolchain[KotlinToolchainInfo]

    # kotlin compiler expects relase version of format 1.6, 1.7, etc. Don't include patch version
    current_kotlin_release_version = ".".join(kotlin_toolchain.kotlin_version.split(".")[:2])

    current_language_version = None
    for arg in ctx.attrs.extra_kotlinc_arguments:
        # If `-language-version` is defined multiple times, we use the last one, just like the compiler does
        if isinstance(arg, str) and "-language-version" in arg:
            current_language_version = arg.split("=")[1].strip()

    if ctx.attrs.k2 == True and kotlin_toolchain.allow_k2_usage:
        if not current_language_version or current_language_version < "2.0":
            if current_kotlin_release_version < "2.0":
                current_language_version = "2.0"
            else:
                current_language_version = current_kotlin_release_version
    else:  # use K1
        if not current_language_version or current_language_version >= "2.0":
            if current_kotlin_release_version >= "2.0":
                current_language_version = "1.9"
            else:
                current_language_version = current_kotlin_release_version

    return current_language_version

def get_language_version_arg(ctx: AnalysisContext) -> list[str]:
    language_version = get_language_version(ctx)
    return ["-language-version=" + language_version]

def filter_out_language_version(extra_arguments: list) -> list:
    return [arg for arg in extra_arguments if not (isinstance(arg, str) and "-language-version" in arg)]

def kotlin_library_impl(ctx: AnalysisContext) -> list[Provider]:
    packaging_deps = ctx.attrs.deps + ctx.attrs.exported_deps + ctx.attrs.runtime_deps

    # TODO(T107163344) this shouldn't be in kotlin_library itself, use overlays to remove it.
    android_packageable_info = merge_android_packageable_info(ctx.label, ctx.actions, packaging_deps)
    if ctx.attrs._build_only_native_code:
        shared_library_info, cxx_resource_info, linkable_graph = create_native_providers(ctx, ctx.label, packaging_deps)
        return [
            shared_library_info,
            cxx_resource_info,
            linkable_graph,
            # Add an unused default output in case this target is used an an attr.source() anywhere.
            DefaultInfo(default_output = ctx.actions.write("{}/unused.jar".format(ctx.label.name), [])),
            TemplatePlaceholderInfo(keyed_variables = {
                "classpath": "unused_but_needed_for_analysis",
            }),
            android_packageable_info,
        ]

    java_providers = build_kotlin_library(
        ctx = ctx,
        validation_deps_outputs = get_validation_deps_outputs(ctx),
    )
    return to_list(java_providers) + [android_packageable_info]

def _check_exported_deps(exported_deps: list[Dependency], attr_name: str):
    for exported_dep in exported_deps:
        # TODO(navidq) add a check that the exported dep always have a JavaLibraryInfo provider
        if JavaLibraryInfo in exported_dep:
            expect(
                not exported_dep[JavaLibraryInfo].may_not_be_exported,
                "{} has 'may_not_be_exported' label and should not be present in {}.".format(exported_dep.label.raw_target(), attr_name),
            )

def build_kotlin_library(
        ctx: AnalysisContext,
        additional_classpath_entries: JavaCompilingDepsTSet | None = None,
        bootclasspath_entries: list[Artifact] = [],
        extra_sub_targets: dict = {},
        validation_deps_outputs: [list[Artifact], None] = None) -> JavaProviders:
    srcs = ctx.attrs.srcs
    has_kotlin_srcs = lazy.is_any(lambda src: src.extension == ".kt" or src.basename.endswith(".src.zip") or src.basename.endswith("-sources.jar"), srcs)

    if not has_kotlin_srcs:
        return build_java_library(
            ctx,
            ctx.attrs.srcs,
            bootclasspath_entries = bootclasspath_entries,
            additional_classpath_entries = additional_classpath_entries,
            # Match buck1, which always does class ABI generation for Kotlin targets unless explicitly specified.
            override_abi_generation_mode = get_abi_generation_mode(ctx.attrs.abi_generation_mode) or AbiGenerationMode("class"),
            extra_sub_targets = extra_sub_targets,
            validation_deps_outputs = validation_deps_outputs,
        )

    else:
        deps_query = getattr(ctx.attrs, "deps_query", []) or []
        provided_deps_query = getattr(ctx.attrs, "provided_deps_query", []) or []
        _check_exported_deps(ctx.attrs.exported_deps, "exported_deps")
        _check_exported_deps(ctx.attrs.exported_provided_deps, "exported_provided_deps")
        deps = (
            ctx.attrs.deps +
            deps_query +
            ctx.attrs.exported_deps +
            ctx.attrs.provided_deps +
            provided_deps_query +
            ctx.attrs.exported_provided_deps
        )
        annotation_processor_properties = create_annotation_processor_properties(
            ctx,
            ctx.attrs.plugins,
            ctx.attrs.annotation_processors,
            ctx.attrs.annotation_processor_params,
            ctx.attrs.annotation_processor_deps,
        )
        ksp_annotation_processor_properties = create_ksp_annotation_processor_properties(ctx.attrs.plugins)

        kotlin_toolchain = ctx.attrs._kotlin_toolchain[KotlinToolchainInfo]
        if kotlin_toolchain.kotlinc_protocol == "classic":
            kotlinc_classes, kapt_generated_sources, ksp_generated_sources = _create_kotlin_sources(
                ctx,
                ctx.attrs.srcs,
                (deps or []) + [kotlin_toolchain.kotlin_stdlib],
                annotation_processor_properties,
                ksp_annotation_processor_properties,
                additional_classpath_entries,
                bootclasspath_entries,
            )
            srcs = [src for src in ctx.attrs.srcs if not src.extension == ".kt"]
            if kapt_generated_sources:
                srcs.append(kapt_generated_sources)
            if ksp_generated_sources:
                srcs.append(ksp_generated_sources)
            kotlinc_classes_classpath = [single_library_compiling_deps(
                ctx.actions,
                JavaClasspathEntry(
                    full_library = kotlinc_classes,
                    abi = kotlinc_classes,
                    abi_as_dir = None,
                    required_for_source_only_abi = True,
                    abi_jar_snapshot = None,
                ),
            )]
            children = kotlinc_classes_classpath + ([additional_classpath_entries] if additional_classpath_entries else []) + [kotlin_toolchain.kotlin_stdlib[JavaLibraryInfo].compiling_deps]
            all_additional_classpath_entries = ctx.actions.tset(JavaCompilingDepsTSet, children = children)
            java_lib = build_java_library(
                ctx,
                srcs,
                run_annotation_processors = False,
                bootclasspath_entries = bootclasspath_entries,
                additional_classpath_entries = all_additional_classpath_entries,
                additional_compiled_srcs = kotlinc_classes,
                generated_sources = filter(None, [kapt_generated_sources, ksp_generated_sources]),
                extra_sub_targets = extra_sub_targets,
                validation_deps_outputs = validation_deps_outputs,
            )
            return java_lib
        elif kotlin_toolchain.kotlinc_protocol == "kotlincd":
            expect(
                ctx.attrs._java_toolchain[JavaToolchainInfo].javac_protocol == "javacd",
                "Kotlin compiler mode: kotlincd and java compiler mode: {} don't match.".format(ctx.attrs._java_toolchain[JavaToolchainInfo].javac_protocol) +
                "\nHint: If you have a Java toolchain with a custom javac, you should also provide a custom kotlinc for your Kotlin toolchain.",
            )
            source_level, target_level = get_java_version_attributes(ctx)
            extra_arguments = cmd_args(
                ctx.attrs.extra_arguments,
                # The outputs of validation_deps need to be added as hidden arguments
                # to an action for the validation_deps targets to be built and enforced.
                hidden = validation_deps_outputs or [],
            )

            common_kotlincd_kwargs = {
                "abi_generation_mode": get_abi_generation_mode(ctx.attrs.abi_generation_mode),
                "actions": ctx.actions,
                "additional_classpath_entries": additional_classpath_entries,
                "annotation_processor_properties": AnnotationProcessorProperties(
                    annotation_processors = annotation_processor_properties.annotation_processors + ksp_annotation_processor_properties.annotation_processors,
                    annotation_processor_params = annotation_processor_properties.annotation_processor_params + ksp_annotation_processor_properties.annotation_processor_params,
                ),
                "bootclasspath_entries": bootclasspath_entries,
                "debug_port": getattr(ctx.attrs, "debug_port", None),
                "deps": deps + [kotlin_toolchain.kotlin_stdlib],
                "enable_used_classes": ctx.attrs.enable_used_classes,
                "extra_kotlinc_arguments": filter_out_language_version(ctx.attrs.extra_kotlinc_arguments or []),
                "friend_paths": ctx.attrs.friend_paths,
                "is_building_android_binary": ctx.attrs._is_building_android_binary,
                "jar_postprocessor": ctx.attrs.jar_postprocessor[RunInfo] if hasattr(ctx.attrs, "jar_postprocessor") and ctx.attrs.jar_postprocessor else None,
                "java_toolchain": ctx.attrs._java_toolchain[JavaToolchainInfo],
                "javac_tool": derive_javac(ctx.attrs.javac) if ctx.attrs.javac else None,
                "kotlin_compiler_plugins": ctx.attrs.kotlin_compiler_plugins,
                "kotlin_toolchain": kotlin_toolchain,
                "label": ctx.label,
                "language_version": get_language_version(ctx),
                "manifest_file": ctx.attrs.manifest_file,
                "remove_classes": ctx.attrs.remove_classes,
                "required_for_source_only_abi": ctx.attrs.required_for_source_only_abi,
                "resources": ctx.attrs.resources,
                "resources_root": ctx.attrs.resources_root,
                "source_level": source_level,
                "source_only_abi_deps": ctx.attrs.source_only_abi_deps,
                "srcs": srcs,
                "target_level": target_level,
            }
            outputs, proto = create_jar_artifact_kotlincd(
                plugin_params = create_plugin_params(ctx, ctx.attrs.plugins),
                extra_arguments = extra_arguments,
                actions_identifier = "",
                incremental = ctx.attrs.incremental,
                **common_kotlincd_kwargs
            )

            if proto:
                extra_sub_targets = extra_sub_targets | {"jar_command_proto_json": [DefaultInfo(default_output = proto)]}
            if outputs and outputs.incremental_state_dir:
                extra_sub_targets = extra_sub_targets | {"incremental_state_dir": [
                    DefaultInfo(default_output = outputs.incremental_state_dir),
                ]}

            if outputs and outputs.annotation_processor_output:
                generated_sources = [outputs.annotation_processor_output]
                extra_sub_targets = extra_sub_targets | {"generated_sources": [
                    DefaultInfo(default_output = outputs.annotation_processor_output),
                ]}
            else:
                generated_sources = []

            java_toolchain = ctx.attrs._java_toolchain[JavaToolchainInfo]
            if (
                not java_toolchain.is_bootstrap_toolchain and
                not ctx.attrs._is_building_android_binary
            ):
                nullsafe_info = get_nullsafe_info(ctx)
                if nullsafe_info:
                    create_jar_artifact_kotlincd(
                        actions_identifier = "nullsafe",
                        plugin_params = nullsafe_info.plugin_params,
                        extra_arguments = nullsafe_info.extra_arguments,
                        # To make sure that even for pure Kotlin targets empty output dir is always present
                        optional_dirs = [nullsafe_info.output.as_output()],
                        is_creating_subtarget = True,
                        incremental = False,
                        **common_kotlincd_kwargs
                    )

                    extra_sub_targets = extra_sub_targets | {"nullsafex-json": [
                        DefaultInfo(default_output = nullsafe_info.output),
                    ]}

            class_to_src_map, sources_jar, class_to_src_map_sub_targets = get_class_to_source_map_info(
                ctx,
                outputs = outputs,
                deps = ctx.attrs.deps + deps_query + ctx.attrs.exported_deps,
                generate_sources_jar = True,
            )
            extra_sub_targets = extra_sub_targets | class_to_src_map_sub_targets

            java_library_info, java_packaging_info, global_code_info, shared_library_info, cxx_resource_info, linkable_graph, template_placeholder_info, intellij_info = create_java_library_providers(
                ctx,
                library_output = outputs.classpath_entry if outputs else None,
                global_code_config = java_toolchain.global_code_config,
                declared_deps = ctx.attrs.deps + deps_query,
                exported_deps = ctx.attrs.exported_deps,
                provided_deps = ctx.attrs.provided_deps + provided_deps_query,
                exported_provided_deps = ctx.attrs.exported_provided_deps,
                runtime_deps = ctx.attrs.runtime_deps,
                needs_desugar = source_level > 7 or target_level > 7,
                generated_sources = generated_sources,
                has_srcs = bool(srcs),
                sources_jar = sources_jar,
                preprocessed_library = outputs.preprocessed_library if outputs else None,
                used_jars_json = outputs.used_jars_json if outputs else None,
            )

            default_info = get_default_info(
                ctx.actions,
                ctx.attrs._java_toolchain[JavaToolchainInfo],
                outputs,
                java_packaging_info,
                extra_sub_targets = extra_sub_targets,
            )
            return JavaProviders(
                java_library_info = java_library_info,
                java_library_intellij_info = intellij_info,
                java_packaging_info = java_packaging_info,
                java_global_code_info = global_code_info,
                shared_library_info = shared_library_info,
                cxx_resource_info = cxx_resource_info,
                linkable_graph = linkable_graph,
                template_placeholder_info = template_placeholder_info,
                default_info = default_info,
                class_to_src_map = class_to_src_map,
            )
        else:
            fail("unrecognized kotlinc protocol `{}`".format(kotlin_toolchain.kotlinc_protocol))
