# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

def cxx_attrs_get_allow_cache_upload(attrs: struct) -> bool:
    value = attrs.allow_cache_upload
    return value if value != None else False
