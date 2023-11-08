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
    // This executable gets called with the following arguments:
    // - Version format (argv[1]) (`-n` for short version, `-b` for product
    // build)
    // - Expected Xcode version (argv[2])
    // - Executable (argv[3])
    // - Args to forward executable
    //
    // For example, it will be something like:
    //   xcode_version_checker -n 13.4
    //   /var/db/xcode_select_link/Toolchains/XcodeDefault.xctoolchain/usr/bin/clang
    //   -c file.c -o file.o

    // TODO(T128745718): We need to figure out the fastest way to implement this
    // forwarder. There are multiple options with varying tradeoffs and we need
    // to make decisions along the following axes:
    // - Plist parsing vs memory comparison
    // - Embedding of expected plist data
    // - Hashing vs full comparison
    //
    // I believe it's important to focus on max performance given this will be
    // running for _every_ tool invocation which will amount to millions of
    // executions per _day_.

    const int numberOfArgs = argc - 1;
    if (numberOfArgs < 3) {
      fprintf(
          stderr,
          "Expected at least three arguments: version format, Xcode version and executable, aborting...\n");
      return 1;
    }

    NSString* expectedVersion = [[NSString alloc] initWithUTF8String:argv[2]];
    const char* versionFormat = argv[1];
    if (strcmp(versionFormat, "-n") == 0) {
      if (!checkXcodeShortVersionMatch(expectedVersion)) {
        return 1;
      }
    } else if (strcmp(versionFormat, "-b") == 0) {
      if (!checkXcodeProductBuildMatch(expectedVersion)) {
        return 1;
      }
    } else {
      fprintf(
          stderr,
          "Found unknown version format `%s`, expected version `%s`, aborting...\n",
          argv[1],
          expectedVersion.UTF8String);
      return 1;
    }

    const int stripArgsCount = 3; // executable + version
    execTool(argv + stripArgsCount, argc - stripArgsCount);
    // `execTool` should never return, if it did, it means it failed to `execve`
    return 1;
  }
}
