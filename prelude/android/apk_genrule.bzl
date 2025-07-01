# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

load("@prelude//:genrule.bzl", "process_genrule")
load("@prelude//android:android_apk.bzl", "get_install_info")
load("@prelude//android:android_providers.bzl", "AndroidAabInfo", "AndroidApkInfo", "AndroidApkUnderTestInfo")
load("@prelude//android:android_toolchain.bzl", "AndroidToolchainInfo")
load("@prelude//android:bundletool_util.bzl", "derive_universal_apk")
load("@prelude//java:class_to_srcs.bzl", "JavaClassToSourceMapInfo")
load("@prelude//java:java_providers.bzl", "KeystoreInfo")
load("@prelude//utils:expect.bzl", "expect")

def apk_genrule_impl(ctx: AnalysisContext) -> list[Provider]:
    expect((ctx.attrs.apk == None) != (ctx.attrs.aab == None), "Exactly one of 'apk' and 'aab' must be specified")

    input_android_apk_under_test_info = None
    input_unstripped_shared_libraries = None
    input_android_apk_subtargets = None
    input_android_aab_subtargets = None
    if ctx.attrs.apk != None:
        # TODO(T104150125) The underlying APK should not have exopackage enabled
        input_android_apk_info = ctx.attrs.apk[AndroidApkInfo]
        expect(input_android_apk_info != None, "'apk' attribute must be an Android APK!")
        input_apk = input_android_apk_info.apk
        input_manifest = input_android_apk_info.manifest
        input_materialized_artifacts = input_android_apk_info.materialized_artifacts
        input_unstripped_shared_libraries = input_android_apk_info.unstripped_shared_libraries
        input_android_apk_under_test_info = ctx.attrs.apk[AndroidApkUnderTestInfo]
        input_android_apk_subtargets = ctx.attrs.apk[DefaultInfo].sub_targets

        env_vars = {
            "APK": cmd_args(input_apk),
        }
    else:
        input_android_aab_info = ctx.attrs.aab[AndroidAabInfo]
        expect(input_android_aab_info != None, "'aab' attribute must be an Android Bundle!")

        # It's not an APK, but buck1 does this so we do it too for compatibility
        input_apk = input_android_aab_info.aab
        input_manifest = input_android_aab_info.manifest
        input_materialized_artifacts = input_android_aab_info.materialized_artifacts
        input_android_aab_subtargets = ctx.attrs.aab[DefaultInfo].sub_targets

        env_vars = {
            "AAB": cmd_args(input_apk),
        }

    genrule_providers = process_genrule(
        ctx,
        ctx.attrs.out,
        ctx.attrs.outs,
        env_vars,
        other_outputs = input_materialized_artifacts,
        genrule_error_handler = ctx.attrs._android_toolchain[AndroidToolchainInfo].android_error_handler,
    )

    genrule_default_info = filter(lambda x: isinstance(x, DefaultInfo), genrule_providers)

    expect(
        len(genrule_default_info) == 1,
        "Expecting a single DefaultInfo, but got {}",
        genrule_default_info,
    )

    genrule_default_output = genrule_default_info[0].default_outputs[0]
    genrule_default_output_is_aab = genrule_default_output.extension == ".aab"
    genrule_default_output_is_apk = genrule_default_output.extension == ".apk"

    expect(
        genrule_default_output_is_aab or genrule_default_output_is_apk,
        "apk_genrule must output a '.apk' or '.aab' file, but got {}",
        genrule_default_info,
    )

    if ctx.attrs.aab:
        if genrule_default_output_is_aab:
            output_aab_info = AndroidAabInfo(
                aab = genrule_default_output,
                manifest = input_manifest,
                materialized_artifacts = input_materialized_artifacts,
            )
            output_apk = None
        else:
            output_aab_info = None
            output_apk = genrule_default_output

        if ctx.attrs.use_derived_apk:
            expect(genrule_default_output_is_aab, "Default genrule output must end in '.aab' if use_derived_apk is True.")

            output_apk = derive_universal_apk(
                ctx = ctx,
                android_toolchain = ctx.attrs._android_toolchain[AndroidToolchainInfo],
                app_bundle = genrule_default_output,
                keystore = ctx.attrs.keystore[KeystoreInfo] if ctx.attrs.keystore else None,
            )
            default_providers = [
                DefaultInfo(
                    default_output = output_apk,
                    other_outputs = input_materialized_artifacts + genrule_default_info[0].other_outputs,
                    sub_targets = {
                        "aab": [DefaultInfo(
                            default_outputs = [genrule_default_output],
                        )],
                        "native_libs": [input_android_aab_subtargets["native_libs"][DefaultInfo]],
                    },
                ),
            ] + filter(lambda x: not isinstance(x, DefaultInfo), genrule_providers)
        else:
            sub_targets = {k: [v[DefaultInfo]] for k, v in genrule_default_info[0].sub_targets.items()}
            sub_targets.update({
                "native_libs": [input_android_aab_subtargets["native_libs"][DefaultInfo]],
            })
            default_providers = [
                DefaultInfo(
                    default_output = genrule_default_output,
                    other_outputs = genrule_default_info[0].other_outputs,
                    sub_targets = sub_targets,
                ),
            ] + filter(lambda x: not isinstance(x, DefaultInfo), genrule_providers)

    else:
        sub_targets = {k: [v[DefaultInfo]] for k, v in genrule_default_info[0].sub_targets.items()}
        sub_targets.update({
            "manifest": [input_android_apk_subtargets["manifest"][DefaultInfo]],
            "native_libs": [input_android_apk_subtargets["native_libs"][DefaultInfo]],
            "unstripped_native_libraries": [input_android_apk_subtargets["unstripped_native_libraries"][DefaultInfo]],
            "unstripped_native_libraries_files": [input_android_apk_subtargets["unstripped_native_libraries_files"][DefaultInfo]],
            "unstripped_native_libraries_json": [input_android_apk_subtargets["unstripped_native_libraries_json"][DefaultInfo]],
        })
        expect(genrule_default_output_is_apk, "apk_genrule output must end in '.apk'")
        output_apk = genrule_default_output
        output_aab_info = None
        default_providers = [
            DefaultInfo(
                default_output = output_apk,
                other_outputs = genrule_default_info[0].other_outputs,
                sub_targets = sub_targets,
            ),
        ] + filter(lambda x: not isinstance(x, DefaultInfo), genrule_providers)

    class_to_src_map = [ctx.attrs.apk[JavaClassToSourceMapInfo]] if (ctx.attrs.apk and JavaClassToSourceMapInfo in ctx.attrs.apk) else []

    if output_apk:
        apk_providers = [
            AndroidApkInfo(
                apk = output_apk,
                manifest = input_manifest,
                materialized_artifacts = input_materialized_artifacts,
                unstripped_shared_libraries = input_unstripped_shared_libraries,
            ),
            get_install_info(
                ctx,
                output_apk = output_apk,
                manifest = input_manifest,
                exopackage_info = None,
            ),
        ]
    else:
        apk_providers = []

    aab_providers = filter(None, [output_aab_info])
    apk_under_test_providers = filter(None, [input_android_apk_under_test_info])

    return default_providers + apk_providers + aab_providers + apk_under_test_providers + class_to_src_map
