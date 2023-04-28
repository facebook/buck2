# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

def cxx_toolchain_macro_impl(cxx_toolchain_rule = None, **kwargs):
    # `cxx.linker_map_enabled` overrides toolchain behavior
    linker_map_enabled = read_config("cxx", "linker_map_enabled")
    if linker_map_enabled != None:
        if linker_map_enabled.lower() == "true":
            kwargs["generate_linker_maps"] = True
        else:
            kwargs["generate_linker_maps"] = False
    cxx_toolchain_rule(
        **kwargs
    )
