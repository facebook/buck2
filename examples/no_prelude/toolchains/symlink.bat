@REM Copyright (c) Meta Platforms, Inc. and affiliates.
@REM
@REM This source code is dual-licensed under either the MIT license found in the
@REM LICENSE-MIT file in the root directory of this source tree or the Apache
@REM License, Version 2.0 found in the LICENSE-APACHE file in the root directory
@REM of this source tree. You may select, at your option, one of the
@REM above-listed licenses.

@echo off
:: %1: Symlink to create
:: %2: Where to point the symlink to
if exist %1 (
    del %1
)
mklink %1 %2