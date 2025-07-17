# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

SwiftPCMUncompiledInfo = provider(
    # @unsorted-dict-items
    fields = {
        "name": provider_field(typing.Any, default = None),
        "is_transient": provider_field(typing.Any, default = None),  # If True represents a transient apple_library target, that can't be compiled into pcm, but which we need to pass up for BUCK1 compatibility, because it can re-export some deps.
        "exported_preprocessor": provider_field(typing.Any, default = None),  # CPreprocessor
        "exported_deps": provider_field(typing.Any, default = None),  # [Dependency]
        "propagated_preprocessor_args_cmd": provider_field(typing.Any, default = None),  # cmd_args
        "uncompiled_sdk_modules": provider_field(typing.Any, default = None),  # [str] a list of required sdk modules
        "modulemap_artifacts": provider_field(list[Artifact], default = []),
    },
)

# A tset can't be returned from the rule, so we need to wrap it into a provider.
WrappedSwiftPCMCompiledInfo = provider(fields = {
    "tset": provider_field(typing.Any, default = None),  # Tset of `SwiftCompiledModuleInfo`
})
