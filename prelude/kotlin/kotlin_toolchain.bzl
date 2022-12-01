# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

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
