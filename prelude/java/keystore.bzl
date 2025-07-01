# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

load("@prelude//java:java_providers.bzl", "KeystoreInfo")

def keystore_impl(ctx: AnalysisContext) -> list[Provider]:
    sub_targets = {}
    sub_targets["keystore"] = [DefaultInfo(default_output = ctx.attrs.store)]
    sub_targets["properties"] = [DefaultInfo(default_output = ctx.attrs.properties)]

    return [
        KeystoreInfo(store = ctx.attrs.store, properties = ctx.attrs.properties),
        DefaultInfo(default_outputs = [ctx.attrs.store, ctx.attrs.properties], sub_targets = sub_targets),
    ]
