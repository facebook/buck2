/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

#ifndef __XCODE_VERSION_CHECKS_H__
#define __XCODE_VERSION_CHECKS_H__

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

#endif
