/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

#import <Foundation/Foundation.h>

#import "xcode_version_checks.h"

typedef struct {
  NSString* actualVersion;
  NSString* expectedVersion;
  BOOL versionsMatch;
} version_check;

typedef struct {
  NSString* actualBuild;
  NSString* expectedBuild;
  BOOL versionsMatch;
} build_check;

typedef struct {
  NSString* plistPath;
  BOOL(*plistFunction)
  (NSString* plistPath, NSString* expectedValue, BOOL logComparisonFailure);
  NSString* expectedValue;
  BOOL versionsMatch;
} plist_check;

int main(int argc, char const* argv[]) {
  @autoreleasepool {
    version_check checks[] = {
        {
            .actualVersion = @"14",
            .expectedVersion = @"14",
            .versionsMatch = YES,
        },
        {
            .actualVersion = @"14.2",
            .expectedVersion = @"14.2",
            .versionsMatch = YES,
        },
        {
            .actualVersion = @"14.2",
            .expectedVersion = @"14",
            .versionsMatch = YES,
        },
        {
            .actualVersion = @"14.2.1",
            .expectedVersion = @"14.2",
            .versionsMatch = YES,
        },
        {
            .actualVersion = @"13",
            .expectedVersion = @"14",
            .versionsMatch = NO,
        },
        {
            .actualVersion = @"15",
            .expectedVersion = @"14",
            .versionsMatch = NO,
        },
        {
            .actualVersion = @"14.2.1",
            .expectedVersion = @"14.2.1",
            .versionsMatch = YES,
        },
        {
            .actualVersion = @"14.2.3",
            .expectedVersion = @"14.2.2",
            .versionsMatch = NO,
        },
        {
            .actualVersion = @"14.2.1",
            .expectedVersion = @"14.2.2",
            .versionsMatch = NO,
        },
        {
            .actualVersion = @"14.3",
            .expectedVersion = @"14.2",
            .versionsMatch = NO,
        },
        {
            .actualVersion = @"14.1",
            .expectedVersion = @"14.2",
            .versionsMatch = NO,
        },
        {
            .actualVersion = @"14",
            .expectedVersion = @"14.0",
            .versionsMatch = YES,
        },
        {
            .actualVersion = @"14.0",
            .expectedVersion = @"14",
            .versionsMatch = YES,
        },
        {
            .actualVersion = @"14.0.0",
            .expectedVersion = @"14.0",
            .versionsMatch = YES,
        },
        {
            .actualVersion = @"14.0",
            .expectedVersion = @"14.0.0",
            .versionsMatch = YES,
        },
        {
            .actualVersion = @"14.0.0.0",
            .expectedVersion = @"14",
            .versionsMatch = YES,
        },
        {
            .actualVersion = @"14",
            .expectedVersion = @"14.0.0.0",
            .versionsMatch = YES,
        },
        {
            .actualVersion = @"14.1.0",
            .expectedVersion = @"14.1",
            .versionsMatch = YES,
        },
        {
            .actualVersion = @"14.1",
            .expectedVersion = @"14.1.0",
            .versionsMatch = YES,
        },
    };

    for (NSUInteger i = 0; i < sizeof(checks) / sizeof(*checks); ++i) {
      version_check check = checks[i];
      if (checkVersionsMatch(check.actualVersion, check.expectedVersion) !=
          check.versionsMatch) {
        fprintf(
            stderr,
            "Version checked failed, version: `%s`, expected: `%s`, expected match result: %d...\n",
            check.actualVersion.UTF8String,
            check.expectedVersion.UTF8String,
            check.versionsMatch);
        return 1;
      }
    }

    const int numberOfArgs = argc - 1;
    if (numberOfArgs < 1) {
      fprintf(stderr, "Expected path to Info.plist as first argument...\n");
      return 1;
    }

    NSString* versionPlistPath = [[NSString alloc] initWithUTF8String:argv[1]];
    plist_check plist_checks[] = {
        {
            .plistPath = versionPlistPath,
            .plistFunction = checkVersionPlistShortVersionMatch,
            .expectedValue = @"14.2",
            .versionsMatch = YES,
        },
        {
            .plistPath = versionPlistPath,
            .plistFunction = checkVersionPlistShortVersionMatch,
            .expectedValue = @"14.3",
            .versionsMatch = NO,
        },
        {
            .plistPath = versionPlistPath,
            .plistFunction = checkVersionPlistProductBuildMatch,
            .expectedValue = @"14C18",
            .versionsMatch = YES,
        },
        {
            .plistPath = versionPlistPath,
            .plistFunction = checkVersionPlistProductBuildMatch,
            .expectedValue = @"14B5033e",
            .versionsMatch = NO,
        },
    };

    for (NSUInteger i = 0; i < sizeof(plist_checks) / sizeof(*plist_checks);
         ++i) {
      plist_check check = plist_checks[i];
      if (check.plistFunction(
              check.plistPath,
              check.expectedValue,
              /* logComparisonFailure = */ NO) != check.versionsMatch) {
        fprintf(
            stderr,
            "Version check failed, plist path: `%s`, expected value: `%s`, expected match result: %d...\n",
            check.plistPath.UTF8String,
            check.expectedValue.UTF8String,
            check.versionsMatch);
        return 1;
      }
    }

    build_check build_checks[] = {
        {
            .actualBuild = @"14e222b",
            .expectedBuild = @"14E222b",
            .versionsMatch = YES,
        },
        {
            .actualBuild = @"14e300b'",
            .expectedBuild = @"14e222b",
            .versionsMatch = NO,
        },
    };

    for (NSUInteger i = 0; i < sizeof(build_checks) / sizeof(*build_checks);
         ++i) {
      build_check check = build_checks[i];
      if (checkProductBuildVersionMatch(
              check.actualBuild, check.expectedBuild) != check.versionsMatch) {
        fprintf(
            stderr,
            "Build check failed, build: `%s`, expected: `%s`, expected match result: %d...\n",
            check.actualBuild.UTF8String,
            check.expectedBuild.UTF8String,
            check.versionsMatch);
        return 1;
      }
    }

    return 0;
  }
}
