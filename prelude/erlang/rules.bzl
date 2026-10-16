# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

load(":erlang_application.bzl", "erlang_app")
load(":erlang_application_includes.bzl", "erlang_app_includes")
load(":erlang_escript.bzl", "erlang_escript")
load(":erlang_release.bzl", "erlang_release")
load(":erlang_tests.bzl", "erlang_test")
load(":erlang_toolchain.bzl", "erlang_otp_binaries")

rules = {
    "erlang_app": erlang_app,
    "erlang_app_includes": erlang_app_includes,
    "erlang_escript": erlang_escript,
    "erlang_otp_binaries": erlang_otp_binaries,
    "erlang_release": erlang_release,
    "erlang_test": erlang_test,
}
