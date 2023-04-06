@REM Copyright (c) Meta Platforms, Inc. and affiliates.
@REM
@REM This source code is licensed under both the MIT license found in the
@REM LICENSE-MIT file in the root directory of this source tree and the Apache
@REM License, Version 2.0 found in the LICENSE-APACHE file in the root directory
@REM of this source tree.

@echo off
:: %1: Symlink to create
:: %2: Where to point the symlink to
if exist %1 (
    del %1
)
mklink %1 %2