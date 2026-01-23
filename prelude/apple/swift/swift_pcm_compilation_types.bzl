# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

load(
    "@prelude//:artifact_tset.bzl",
    "ArtifactTSet",
)
load("@prelude//apple/swift:swift_toolchain_types.bzl", "SwiftCompiledModuleTset")
load("@prelude//cxx:preprocessor.bzl", "CPreprocessor")

SwiftPCMUncompiledInfo = provider(fields = {
    "exported_deps": provider_field(list[Dependency], default = []),
    "exported_preprocessor": provider_field([CPreprocessor, None], default = None),
    # If True represents an apple_library target that can't be compiled into a
    # pcm, but which can re-export modular deps.
    "is_transient": provider_field(bool, default = False),
    # For non-transient modules, the artifact of this module's modulemap.
    "modulemap_artifact": provider_field(Artifact | None, default = None),
    "name": provider_field(str),
    "propagated_preprocessor_args_cmd": provider_field(typing.Any, default = None),  # cmd_args
    "uncompiled_sdk_modules": provider_field(list[str], default = []),
})

WrappedSwiftPCMCompiledInfo = provider(fields = {
    "clang_debug_info": provider_field(ArtifactTSet),
    "clang_deps": provider_field(SwiftCompiledModuleTset),  # Tset of `SwiftCompiledModuleInfo`
})
