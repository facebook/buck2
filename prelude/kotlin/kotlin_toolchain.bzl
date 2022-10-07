KotlinToolchainInfo = provider(
    "Kotlin toolchain info",
    fields = [
        "annotation_processing_jar",
        "compile_kotlin",
        "kapt_base64_encoder",
        "kotlinc",
        "kotlinc_classpath",
        "kotlin_stdlib",
    ],
)
