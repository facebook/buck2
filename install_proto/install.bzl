# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

def _installer_impl(ctx: "context") -> ["provider"]:
    installer = ctx.attrs.installer
    return [DefaultInfo(), InstallInfo(installer = installer, files = ctx.attrs.files)]

installer = rule(impl = _installer_impl, attrs = {
    "files": attrs.dict(key = attrs.string(), value = attrs.source(), default = {}),
    "installer": attrs.label(),
})
