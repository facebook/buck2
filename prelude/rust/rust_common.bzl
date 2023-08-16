# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

def rust_common_macro_wrapper(rust_rule):
    def rust_common_impl(**kwargs):
        rust_rule(**kwargs)

    return rust_common_impl
