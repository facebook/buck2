# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

def cxx_attrs_use_fbcc_rust_wrapper(attrs: struct) -> bool:
    return attrs.use_fbcc_rust_wrapper if hasattr(attrs, "use_fbcc_rust_wrapper") else False

def cxx_attrs_get_allow_cache_upload(attrs: struct, default: [None, bool] = None) -> bool:
    default_value = default if default != None else False
    if not hasattr(attrs, "allow_cache_upload"):
        return default_value
    value = attrs.allow_cache_upload
    return value if value != None else default_value

def cxx_toolchain_allow_cache_upload_args():
    doc = """
        Whether to allow uploading of object files to cache when the compile
        action is executed locally and the configuration allows uploads (i.e.,
        there is a cache configured and the client has permission to write to it).
        """
    return {
        "c_compiler_allow_cache_upload": attrs.option(
            attrs.bool(),
            default = None,
            doc = doc,
        ),
        "cuda_compiler_allow_cache_upload": attrs.option(
            attrs.bool(),
            default = None,
            doc = doc,
        ),
        "cxx_compiler_allow_cache_upload": attrs.option(
            attrs.bool(),
            default = None,
            doc = doc,
        ),
    }
