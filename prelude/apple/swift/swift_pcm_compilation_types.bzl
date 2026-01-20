# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

load("@prelude//apple/swift:swift_toolchain_types.bzl", "SwiftCompiledModuleTset")
load("@prelude//cxx:preprocessor.bzl", "CPreprocessor")

SwiftPCMUncompiledInfo = provider(
    # @unsorted-dict-items
    fields = {
        "name": provider_field(str),
        # If True represents an apple_library targe that can't be compiled into
        # a pcm, but which can re-export modular deps.
        "is_transient": provider_field(bool, default = False),
        "exported_preprocessor": provider_field([CPreprocessor, None], default = None),
        "exported_deps": provider_field(list[Dependency], default = []),
        "propagated_preprocessor_args_cmd": provider_field(typing.Any, default = None),  # cmd_args
        "uncompiled_sdk_modules": provider_field(list[str], default = []),
        "modulemap_artifacts": provider_field(list[Artifact], default = []),
    },
)

# A tset can't be returned from the rule, so we need to wrap it into a provider.
WrappedSwiftPCMCompiledInfo = provider(fields = {
    "tset": provider_field(SwiftCompiledModuleTset),  # Tset of `SwiftCompiledModuleInfo`
})
