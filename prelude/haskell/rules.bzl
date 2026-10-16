# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

load(":haskell.bzl", "haskell_binary", "haskell_library", "haskell_prebuilt_library")
load(":haskell_ghci.bzl", "haskell_ghci")
load(":haskell_haddock.bzl", "haskell_haddock")
load(":haskell_ide.bzl", "haskell_ide")

rules = {
    "haskell_binary": haskell_binary,
    "haskell_ghci": haskell_ghci,
    "haskell_haddock": haskell_haddock,
    "haskell_ide": haskell_ide,
    "haskell_library": haskell_library,
    "haskell_prebuilt_library": haskell_prebuilt_library,
}
