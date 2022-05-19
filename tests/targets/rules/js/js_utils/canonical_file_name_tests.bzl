# Copyright 2017 The Bazel Authors. All rights reserved.
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#    http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

"""Unit tests for js_utils."""

load("@fbcode//buck2/prelude/js:js_utils.bzl", "get_canonical_src_name")
load("@fbcode//buck2/tests/targets/starlib:unittest.bzl", "asserts")

def get_canonical_src_name_test():
    """Unit test for js_utils.get_canonical_src_name"""
    asserts.equals("non_asset.js", get_canonical_src_name("non_asset.js"))
    asserts.equals("non_asset", get_canonical_src_name("non_asset"))
    asserts.equals("asset.png", get_canonical_src_name("asset.png"))
    asserts.equals("asset.png", get_canonical_src_name("asset.android.png"))
    asserts.equals("asset.invalid_platform.png", get_canonical_src_name("asset.invalid_platform.png"))
    asserts.equals("asset.png", get_canonical_src_name("asset@1.5x.png"))
    asserts.equals("asset.png", get_canonical_src_name("asset@3x.png"))
    asserts.equals("assetblah.png", get_canonical_src_name("asset@1.5xblah.png"))
    asserts.equals("asset.png", get_canonical_src_name("asset@1.5x.android.png"))
    asserts.equals("asset.png", get_canonical_src_name("asset@3x.android.png"))

def test():
    """Creates the test targets for js_utils.bzl tests."""
    get_canonical_src_name_test()
