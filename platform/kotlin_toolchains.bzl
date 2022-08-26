load("@prelude//kotlin:kotlin_toolchain.bzl", "KotlinToolchainInfo")

def config_backed_kotlin_toolchain(
        name,
        annotation_processing_jar,
        compile_kotlin,
        kapt_base64_encoder,
        kotlinc,
        kotlinc_classpath,
        kotlin_stdlib,
        **kwargs):
    kwargs["annotation_processing_jar"] = annotation_processing_jar
    kwargs["compile_kotlin"] = compile_kotlin
    kwargs["kapt_base64_encoder"] = kapt_base64_encoder
    kwargs["kotlinc"] = kotlinc
    kwargs["kotlinc_classpath"] = kotlinc_classpath
    kwargs["kotlin_stdlib"] = kotlin_stdlib

    _config_backed_kotlin_toolchain_rule(
        name = name,
        **kwargs
    )

def _config_backed_kotlin_toolchain_rule_impl(ctx):
    return [
        DefaultInfo(),
        KotlinToolchainInfo(
            annotation_processing_jar = ctx.attrs.annotation_processing_jar,
            compile_kotlin = ctx.attrs.compile_kotlin,
            kapt_base64_encoder = ctx.attrs.kapt_base64_encoder,
            kotlinc = ctx.attrs.kotlinc,
            kotlinc_classpath = ctx.attrs.kotlinc_classpath,
            kotlin_stdlib = ctx.attrs.kotlin_stdlib,
        ),
    ]

_config_backed_kotlin_toolchain_rule = rule(
    attrs = {
        "annotation_processing_jar": attrs.dep(),
        "compile_kotlin": attrs.dep(providers = [RunInfo]),
        "kapt_base64_encoder": attrs.dep(providers = [RunInfo]),
        "kotlin_stdlib": attrs.dep(),
        "kotlinc": attrs.dep(providers = [RunInfo]),
        "kotlinc_classpath": attrs.list(attrs.dep(), default = []),
    },
    impl = _config_backed_kotlin_toolchain_rule_impl,
)
