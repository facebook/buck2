# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

def export_file(visibility = ["PUBLIC"], **kwargs):
    # @lint-ignore BUCKLINT: avoid "native is forbidden in fbcode"
    native.export_file(visibility = visibility, **kwargs)

def export_files(files, visibility = ["PUBLIC"], **kwargs):
    # @lint-ignore BUCKLINT: avoid "native is forbidden in fbcode"
    for file in files:
        native.export_file(name = file, visibility = visibility, **kwargs)
