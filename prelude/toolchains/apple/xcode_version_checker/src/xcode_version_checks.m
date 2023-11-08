/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

#import "xcode_version_checks.h"

#import <crt_externs.h>
#import <stdlib.h>

extern char** environ;

static BOOL IsInsideRemoteExecutionWorker(void) {
  // TODO: Remove dependence on ACTION_DIGEST, once D41872225 lands
  return getenv("INSIDE_RE_WORKER") != NULL || getenv("ACTION_DIGEST") != NULL;
}

static NSUInteger effectiveVersionComponentsCount(
    NSArray<NSString*>* versionComponents) {
  NSUInteger zeroTrailingComponentsCount = 0;
  for (NSInteger i = [versionComponents count]; i > 0; --i) {
    NSString* component = [versionComponents objectAtIndex:(i - 1)];
    if ([component isEqual:@"0"]) {
      zeroTrailingComponentsCount++;
    }
  }

  // Ignore trailing zero components, e.g.,, treat "14.0.0" as "14"
  return [versionComponents count] - zeroTrailingComponentsCount;
}

// Looks for _exact_ match for versions up to all components specified in
// `expectedShortVersion`
BOOL checkVersionsMatch(
    NSString* shortVersion,
    NSString* expectedShortVersion) {
  NSArray<NSString*>* expectedShortVersionComponents =
      [expectedShortVersion componentsSeparatedByString:@"."];
  NSArray<NSString*>* shortVersionComponents =
      [shortVersion componentsSeparatedByString:@"."];

  NSUInteger shortVersionComponentsCount =
      effectiveVersionComponentsCount(shortVersionComponents);
  NSUInteger expectedShortVersionComponentsCount =
      effectiveVersionComponentsCount(expectedShortVersionComponents);
  if (shortVersionComponentsCount < expectedShortVersionComponentsCount) {
    return NO;
  }

  for (NSUInteger i = 0; i < expectedShortVersionComponentsCount; ++i) {
    NSString* expectedComponent =
        [expectedShortVersionComponents objectAtIndex:i];
    NSString* actualComponent = [shortVersionComponents objectAtIndex:i];
    if (![expectedComponent isEqual:actualComponent]) {
      return NO;
    }
  }

  return YES;
}

BOOL checkPlistValueMatch(
    NSString* plistPath,
    NSString* plistKey,
    NSString* expectedValue,
    BOOL (*comparator)(NSString* value, NSString* expectedValue),
    BOOL logComparisonFailure) {
  NSURL* xcodePlistUrl = [NSURL fileURLWithPath:plistPath];
  NSError* xcodePlistError = nil;
  NSData* xcodePlistData = [NSData dataWithContentsOfURL:xcodePlistUrl
                                                 options:0
                                                   error:&xcodePlistError];
  if (xcodePlistData == nil) {
    fprintf(
        stderr,
        "Failed to read Xcode plist file: %s\n",
        (xcodePlistError.description.UTF8String ?: ""));
    return NO;
  }

  NSError* plistError = nil;
  id xcodePlist =
      [NSPropertyListSerialization propertyListWithData:xcodePlistData
                                                options:NSPropertyListImmutable
                                                 format:nil
                                                  error:&plistError];
  if (xcodePlist == nil) {
    fprintf(
        stderr,
        "Failed to parse Xcode plist file: %s\n",
        (plistError.description.UTF8String ?: ""));
    return NO;
  }

  if (![xcodePlist isKindOfClass:[NSDictionary class]]) {
    fprintf(stderr, "Plist is not a dictionary\n");
    return NO;
  }

  NSDictionary* xcodePlistDict = (NSDictionary*)xcodePlist;
  NSString* value = xcodePlistDict[plistKey];
  if (![value isKindOfClass:[NSString class]]) {
    fprintf(stderr, "Version is not of type string\n");
    return NO;
  }

  BOOL comparisonResult = comparator(value, expectedValue);
  if (!comparisonResult && logComparisonFailure) {
    fprintf(
        stderr,
        "Found unexpected Xcode version, expected %s but found %s, aborting...\n",
        expectedValue.UTF8String,
        value.UTF8String);
    if (IsInsideRemoteExecutionWorker()) {
      fprintf(
          stderr,
          "This error was generated inside an RE worker. To debug, check the following:\n"
          "1) The command being executed\n"
          "2) The subplatform property of the RE action\n"
          "3) The execution platform constraints\n"
          "4) The Xcode installed on the particular worker\n");
    } else {
      fprintf(
          stderr,
          "This error was generated when running locally. To debug, check the following:\n"
          "1) The command being executed\n"
          "2) The selected Xcode version\n"
          "3) Did you select a different Xcode _after_ a buck2 daemon was launched?\n"
          "4) The execution platform constraints\n"
          "5) The tools specified for the `apple_toolchain()`\n");
    }
  }

  return comparisonResult;
}

BOOL checkProductBuildVersionMatch(
    NSString* productBuild,
    NSString* expectedProductBuild) {
  if (productBuild == expectedProductBuild) {
    return YES;
  }

  if (productBuild == nil || expectedProductBuild == nil) {
    // `caseInsensitiveCompare:` is declared taking non-null, so guard against
    return NO;
  }

  NSComparisonResult cmpResult =
      [productBuild caseInsensitiveCompare:expectedProductBuild];
  return (cmpResult == NSOrderedSame);
}

BOOL checkVersionPlistProductBuildMatch(
    NSString* plistPath,
    NSString* expectedProductBuild,
    BOOL logComparisonFailure) {
  return checkPlistValueMatch(
      plistPath,
      @"ProductBuildVersion",
      expectedProductBuild,
      checkProductBuildVersionMatch,
      logComparisonFailure);
}

NSString* const VERSION_PLIST_PATH =
    @"/var/db/xcode_select_link/../version.plist";
BOOL checkXcodeProductBuildMatch(NSString* expectedProductBuild) {
  return checkVersionPlistProductBuildMatch(
      VERSION_PLIST_PATH,
      expectedProductBuild,
      /* logComparisonFailure = */ YES);
}

BOOL checkVersionPlistShortVersionMatch(
    NSString* plistPath,
    NSString* expectedShortVersion,
    BOOL logComparisonFailure) {
  return checkPlistValueMatch(
      plistPath,
      @"CFBundleShortVersionString",
      expectedShortVersion,
      checkVersionsMatch,
      logComparisonFailure);
}

BOOL checkXcodeShortVersionMatch(NSString* expectedShortVersion) {
  return checkVersionPlistShortVersionMatch(
      VERSION_PLIST_PATH,
      expectedShortVersion,
      /* logComparisonFailure = */ YES);
}

void execTool(char const* arguments[], int argumentsCount) {
  int execArgvSize = argumentsCount + 1 /* NULL terminator */;
  char** execArgv = malloc(
      sizeof(char*) *
      execArgvSize); // no need to free() as either we execve() or exit
  memcpy(execArgv, arguments, argumentsCount * sizeof(char*));
  execArgv[execArgvSize - 1] = NULL;
  execve(arguments[0], execArgv, environ);
  // execve() never returns on success, so we must have failed if we're here
  fprintf(
      stderr,
      "Failed to execve(), errno %d, description: %s\n",
      errno,
      strerror(errno));
}
