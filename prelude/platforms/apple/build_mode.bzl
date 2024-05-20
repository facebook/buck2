# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

# @oss-disable: load("@prelude//platforms/apple/meta_only:build_mode.bzl", _APPLE_BUILD_MODES = "APPLE_BUILD_MODES", _BUILD_MODE = "BUILD_MODE", _get_build_mode = "get_build_mode", _get_build_mode_debug = "get_build_mode_debug", _get_build_mode_release = "get_build_mode_release") 

BUILD_MODE_DEBUG = "debug" # @oss-enable
BUILD_MODE_PROFILE = "profile" # @oss-enable
BUILD_MODE_RELEASE = "release" # @oss-enable

APPLE_BUILD_MODES = [BUILD_MODE_DEBUG, BUILD_MODE_PROFILE, BUILD_MODE_RELEASE] # @oss-enable
# @oss-disable: APPLE_BUILD_MODES = _APPLE_BUILD_MODES 

BUILD_MODE = struct( # @oss-enable
    DEBUG = BUILD_MODE_DEBUG, # @oss-enable
    PROFILE = BUILD_MODE_PROFILE, # @oss-enable
    RELEASE = BUILD_MODE_RELEASE, # @oss-enable
) # @oss-enable
# @oss-disable: BUILD_MODE = _BUILD_MODE 

CONSTRAINT_PACKAGE = "prelude//platforms/apple/constraints" # @oss-enable
# @oss-disable: CONSTRAINT_PACKAGE = "ovr_config//build_mode/apple/constraints" 

def get_build_mode():
    return read_root_config("apple", "build_mode", BUILD_MODE_DEBUG) # @oss-enable
    # @oss-disable: return _get_build_mode() 

def get_build_mode_debug():
    return BUILD_MODE.DEBUG # @oss-enable
    # @oss-disable: return _get_build_mode_debug() 

def get_build_mode_release():
    return BUILD_MODE.RELEASE # @oss-enable
    # @oss-disable: return _get_build_mode_release() 
