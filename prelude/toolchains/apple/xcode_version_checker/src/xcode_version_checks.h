/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

#pragma once

#import <Foundation/Foundation.h>

BOOL checkVersionsMatch(NSString* shortVersion, NSString* expectedShortVersion);
void execTool(char const* arguments[], int argumentsCount);
BOOL checkPlistValueMatch(
    NSString* plistPath,
    NSString* plistKey,
    NSString* expectedValue,
    BOOL (*comparator)(NSString* value, NSString* expectedValue),
    BOOL logComparisonFailure);

BOOL checkProductBuildVersionMatch(
    NSString* productBuild,
    NSString* expectedProductBuild);
BOOL checkVersionPlistProductBuildMatch(
    NSString* plistPath,
    NSString* expectedProductBuild,
    BOOL logComparisonFailure);
BOOL checkXcodeProductBuildMatch(NSString* expectedProductBuild);

BOOL checkVersionPlistShortVersionMatch(
    NSString* plistPath,
    NSString* expectedShortVersion,
    BOOL logComparisonFailure);
BOOL checkXcodeShortVersionMatch(NSString* expectedShortVersion);
