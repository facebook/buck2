# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

load("@prelude//apple:apple_error_handler_types.bzl", "AppleErrorCategory")
# @oss-disable[end= ]: load("@prelude//apple/meta_only:apple_extra_error_categories.bzl", "APPLE_META_STDERR_ERROR_CATEGORIES")

_APPLE_STDERR_ERROR_CATEGORIES = [

    # STOP! @oss-disable
    # If you work at Meta, you probably want to include things in APPLE_META_STDERR_ERROR_CATEGORIES   @oss-disable
    # so you can include a link to an internal resource (wiki, task, etc)                              @oss-disable
    # I would only add additional categories here if you think someone in open-source would benefit    @oss-disable

    #codesigning issues
    AppleErrorCategory(matcher = "codesignprovisioningerror", category = "code_sign_error"),
    AppleErrorCategory(matcher = "the timestamp service is not available", category = "code_sign_error"),

    #compilation issues
    AppleErrorCategory(matcher = "failed to emit precompiled module", category = "pcm_compilation_failure"),
    AppleErrorCategory(matcher = "please rebuild precompiled header", category = "pcm_compilation_failure"),
    AppleErrorCategory(matcher = "llvm-lipo", category = "lipo_failure"),
    AppleErrorCategory(matcher = ".modulemap:", category = "modulemap_compilation_failure"),
    AppleErrorCategory(matcher = "missing required modules", category = "missing_required_modules_error"),
    AppleErrorCategory(matcher = "has a minimum deployment target", category = "deployment_target_error"),

    #toolchain / genrule issues
    AppleErrorCategory(matcher = "stack dump:", category = "binary_execution_failure"),
    AppleErrorCategory(matcher = "thread 'main' panicked", category = "binary_execution_failure"),
    AppleErrorCategory(matcher = "error while loading shared libraries", category = "binary_execution_failure"),
    AppleErrorCategory(matcher = "traceback (most recent call last)", category = "python_execution_failure"),
    AppleErrorCategory(matcher = "command not found", category = "command_not_found_failure"),
    AppleErrorCategory(matcher = "command timed out", category = "timeout_failure"),
    AppleErrorCategory(matcher = "no such file or directory", category = "no_such_file_failure"),

    #user errors
    AppleErrorCategory(matcher = "unknown target", category = "unknown_buck_target_failure"),

    #linker issues
    AppleErrorCategory(matcher = "linker command failed", category = "linker_failure"),
    AppleErrorCategory(matcher = "duplicate symbol", category = "duplicate_symbol_failure"),
    AppleErrorCategory(matcher = "undefined symbol", category = "undefined_symbol_failure"),
    AppleErrorCategory(matcher = "framework not found", category = "framework_not_found_failure"),

    #buck configuration issues
    AppleErrorCategory(matcher = "unknown cell alias", category = "unknown_cell_alias_failure"),
]

def _match(matcher: str | BuckRegex, lowercase_stderr: str) -> bool:
    if isinstance(matcher, str):
        return matcher in lowercase_stderr
    elif isinstance(matcher, BuckRegex):
        return matcher.match(lowercase_stderr)
    else:
        fail("Unknown matcher type: {}", type(matcher))

def _add_category_strings(ctx: ActionErrorCtx, lowercase_stderr: str, errors: list[ActionSubError], source: list[AppleErrorCategory]):
    for error_category in source:
        if _match(error_category.matcher, lowercase_stderr):
            errors.append(ctx.new_sub_error(category = "apple_" + error_category.category, message = error_category.message))

def apple_build_error_handler(ctx: ActionErrorCtx) -> list[ActionSubError]:
    lowercase_stderr = ctx.stderr.lower()

    errors = []
    _add_category_strings(ctx, lowercase_stderr, errors, _APPLE_STDERR_ERROR_CATEGORIES)
    # @oss-disable[end= ]: _add_category_strings(ctx, lowercase_stderr, errors, APPLE_META_STDERR_ERROR_CATEGORIES)

    return errors
