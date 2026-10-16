# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

load(":bitcode.bzl", "llvm_link_bitcode")
load(":cxx.bzl", "cxx_binary", "cxx_library", "cxx_precompiled_header", "cxx_test", "prebuilt_cxx_library")
load(":cxx_flags.bzl", "cxx_flags")
load(":cxx_genrule.bzl", "cxx_genrule")
load(":cxx_toolchain.bzl", "cxx_toolchain")
load(":prebuilt_cxx_library_group.bzl", "prebuilt_cxx_library_group")
load(":transformation_spec.bzl", "transformation_spec")
load(":windows_resource.bzl", "windows_resource")

rules = {
    "cxx_binary": cxx_binary,
    "cxx_flags": cxx_flags,
    "cxx_genrule": cxx_genrule,
    "cxx_library": cxx_library,
    "cxx_precompiled_header": cxx_precompiled_header,
    "cxx_test": cxx_test,
    "cxx_toolchain": cxx_toolchain,
    "llvm_link_bitcode": llvm_link_bitcode,
    "prebuilt_cxx_library": prebuilt_cxx_library,
    "prebuilt_cxx_library_group": prebuilt_cxx_library_group,
    "transformation_spec": transformation_spec,
    "windows_resource": windows_resource,
}
