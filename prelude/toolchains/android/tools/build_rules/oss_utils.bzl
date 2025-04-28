# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

# Returns true if build.is_oss is set to true; this should indicate that the current build is
# being done for the purpose of an oss release.
def is_oss_build():
    return read_config("build", "is_oss", "false") == "true"
