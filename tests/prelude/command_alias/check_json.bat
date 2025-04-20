@REM Copyright (c) Meta Platforms, Inc. and affiliates.
@REM
@REM This source code is licensed under both the MIT license found in the
@REM LICENSE-MIT file in the root directory of this source tree and the Apache
@REM License, Version 2.0 found in the LICENSE-APACHE file in the root directory
@REM of this source tree.

@echo off
set EXPECTED={"key":"value","key2":{"test2_key":"test2_value","test_key":"test_value"}}
if "%COMMAND_ALIAS_ENV_TEST_VARS%" NEQ "%EXPECTED%" (
  exit /b 1
)
