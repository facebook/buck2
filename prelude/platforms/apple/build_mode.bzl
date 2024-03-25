# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

# @oss-enable BUILD_MODE_DEBUG = "debug"
# @oss-enable BUILD_MODE_PROFILE = "profile"
# @oss-enable BUILD_MODE_RELEASE = "release"
# @oss-disable: BUILD_MODE_LOCAL = "local" 
# @oss-disable: BUILD_MODE_MASTER = "master" 
# @oss-disable: BUILD_MODE_RELEASE_CANDIDATE = "rc" 
# @oss-disable: BUILD_MODE_PRODUCTION = "production" 
# @oss-disable: BUILD_MODE_PROFILE = "profile" 

# @oss-enable APPLE_BUILD_MODES = [BUILD_MODE_DEBUG, BUILD_MODE_PROFILE, BUILD_MODE_RELEASE]
# @oss-disable: APPLE_BUILD_MODES = [BUILD_MODE_LOCAL, BUILD_MODE_MASTER, BUILD_MODE_RELEASE_CANDIDATE, BUILD_MODE_PRODUCTION, BUILD_MODE_PROFILE] 

BUILD_MODE = struct(
    # @oss-enable DEBUG: BUILD_MODE_DEBUG
    # @oss-enable PROFILE: BUILD_MODE_PROFILE,
    # @oss-enable RELEASE: BUILD_MODE_RELEASE,
    # @oss-disable: LOCAL = BUILD_MODE_LOCAL, 
    # @oss-disable: MASTER = BUILD_MODE_MASTER, 
    # @oss-disable: RELEASE_CANDIDATE = BUILD_MODE_RELEASE_CANDIDATE, 
    # @oss-disable: PRODUCTION = BUILD_MODE_PRODUCTION, 
    # @oss-disable: PROFILE = BUILD_MODE_PROFILE, 
)

# @oss-enable CONSTRAINT_PACKAGE = "prelude//platforms/apple/constraints"
# @oss-disable: CONSTRAINT_PACKAGE = "ovr_config//build_mode/apple/constraints" 
