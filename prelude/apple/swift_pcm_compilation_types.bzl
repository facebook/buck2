# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

SwiftPCMCompilationInfo = provider(fields = [
    "name",
    "pcm_output",
    "exported_pre",
    "deps_set",
    "sdk_deps_set",  # A TSet of direct and transitive SDK deps.
])

SwiftPCMUncompiledInfo = provider(fields = [
    "name",
    "exported_preprocessor",
    "exported_deps",
    "propagated_preprocessor_args_cmd",
    "sdk_modules",  # a list of required sdk modules
])

SwiftPCMCompiledInfo = provider(fields = [
    "name",
    "pcm_output",
    "exported_preprocessor",
    "sdk_modules",
    "exported_deps",
])
