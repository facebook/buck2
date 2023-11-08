/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

#import <Foundation/Foundation.h>
#import <unistd.h>

#import "xcode_version_checks.h"

int main(int argc, char const* argv[]) {
  @autoreleasepool {
    const int numberOfArgs = argc - 1;
    if (numberOfArgs < 1) {
      fprintf(
          stderr, "Expected at least one arguments: executable, aborting...\n");
      return 1;
    }

    execTool(argv + 1, argc - 1);
    // `execTool` should never return, if it did, it means it failed to `execve`
    return 1;
  }
}
